(module telebot (;;; basic API wrappers
                 getMe
                 getUpdates
                 sendMessage
                 forwardMessage
		 resolve-query
                 sendPhoto
                 sendAudio
                 sendDocument
                 sendSticker
                 sendVideo
                 sendVoice
                 sendLocation
                 sendVenue
                 sendContact
                 sendChatAction
                 getUserProfilePhotos
                 getFile
                 kickChatMember
                 unbanChatMember
                 answerCallbackQuery
                 editMessageText
                 editMessageCaption
                 editMessageReplyMarkup
                 answerInlineQuery
                 ;;; framework
                 is-message?
		 is-edited_message?
                 is-inline_query?
                 is-chosen_inline_result?
		 is-text?
		 is-location?
                 poll-updates
                 make-conversation-manager)
  (import chicken scheme)
  (use srfi-1
       srfi-69)
  (use openssl
       http-client)
  (use medea
       vector-lib
       data-structures)
  (use loops)

  (define-constant api-base "https://api.telegram.org/bot")

  ;;; helper functions

  (define (get-query-url token method)
    (string-append api-base token "/" method))

  (define (clean-query-parameters parameters)
    (let ((cleaned-parameters (remove (lambda (p) (equal? #f (cdr p)))
                                      parameters)))
      (if (null-list? cleaned-parameters)
        #f
        cleaned-parameters)))

  (define (resolve-query query tree)
    (fold (lambda (x y) (alist-ref x y))
          tree
          query))

  ;;; plain API wrappers, returning deserialized JSON

  (define-syntax wrap-api-method
    (syntax-rules (required optional)
    ((wrap-api-method method
                      (required required_params ...)
                      (optional optional_params ...))
     (define (method token
                     #!key required_params ...
                           optional_params ...)
       (if (any (lambda (x) (equal? #f x))
                (list required_params ...))
         (abort 'required-parameter-missing)
         (with-input-from-request
           (get-query-url token (symbol->string 'method))
           (clean-query-parameters
             (map (lambda (l) (cons (first l) (second l)))
                  (zip '(required_params ... optional_params ...)
                       (list required_params ... optional_params ...))))
           read-json))))))

  (wrap-api-method getMe (required) (optional))

  (wrap-api-method getUpdates
                   (required)
                   (optional offset
                             limit
                             timeout))

  (wrap-api-method sendMessage
                   (required chat_id
                             text)
                   (optional parse_mode
                             disable_web_page_preview
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method forwardMessage
                   (required chat_id
                             from_chat_id
                             message_id)
                   (optional disable_notification))

  (wrap-api-method sendPhoto
                   (required chat_id
                             photo)
                   (optional caption
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendAudio
                   (required chat_id
                             audio)
                   (optional duration
                             performer
                             title
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendDocument
                   (required chat_id
                             document)
                   (optional caption
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendSticker
                   (required chat_id
                             sticker)
                   (optional disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendVideo
                   (required chat_id
                             video)
                   (optional duration
                             width
                             height
                             caption
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendVoice
                   (required chat_id
                             voice)
                   (optional duration
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendLocation
                   (required chat_id
                             latitude
                             longitude)
                   (optional disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendVenue
                   (required chat_id
                             latitude
                             longitude
                             title
                             address)
                   (optional foursquare_id
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendContact
                   (required chat_id
                             phone_number
                             first_name)
                   (optional last_name
                             disable_notification
                             reply_to_message_id
                             reply_markup))

  (wrap-api-method sendChatAction
                   (required chat_id
                             action)
                   (optional))

  (wrap-api-method getUserProfilePhotos
                   (required user_id)
                   (optional offset
                             limit))

  (wrap-api-method getFile
                   (required file_id)
                   (optional))

  (wrap-api-method kickChatMember
                   (required chat_id
                             user_id)
                   (optional))

  (wrap-api-method unbanChatMember
                   (required chat_id
                             user_id)
                   (optional))

  (wrap-api-method answerCallbackQuery
                   (required callback_query_id)
                   (optional text
                             show_alert))

  (wrap-api-method editMessageText
                   (required text)
                   (optional chat_id
                             message_id
                             inline_message_id
                             parse_mode
                             disable_web_page_preview
                             reply_markup))

  (wrap-api-method editMessageCaption
                   (required)
                   (optional chat_id
                             message_id
                             inline_message_id
                             caption
                             reply_markup))

  (wrap-api-method editMessageReplyMarkup
                   (required)
                   (optional chat_id
                             message_id
                             inline_message_id
                             reply_markup))

  (wrap-api-method answerInlineQuery
                   (required inline_query_id
                             results)
                   (optional cache_time
                             is_personal
                             next_offset))

  ;;; framework

  (define (update-predicate type)
    (lambda (update)
      (not (equal? #f (resolve-query type update)))))

  (define is-message?              (update-predicate '(message)))
  (define is-edited_message?       (update-predicate '(edited_message)))
  (define is-inline_query?         (update-predicate '(inline_query)))
  (define is-chosen_inline_result? (update-predicate '(chosen_inline_result)))

  (define is-text?                 (update-predicate '(message text)))
  (define is-location?             (update-predicate '(message location)))

  (define (poll-updates token handler)
    (let ((offset 0))
      (do-forever
        (vector-for-each (lambda (i u)
                           (handler u)
                           (set! offset (+ 1 (alist-ref 'update_id u))))
                         (alist-ref 'result
                                    (getUpdates token
                                                offset:  offset
                                                timeout: 60))))))

  (define (make-conversation-manager token make-handler)
    (let ((token         token)
          (conversations (make-hash-table)))
      (lambda (update)
        (if (is-message? update)
          (let ((chat_id (resolve-query '(message from id) update)))
            (if (hash-table-exists? conversations chat_id)
              ((hash-table-ref conversations chat_id) update)
              (hash-table-set! conversations
                               chat_id
                               (make-handler token chat_id))))))))
)
