(module telebot (getMe
                 getUpdates
                 sendMessage
                 forwardMessage
                 sendPhoto
                 sendAudio
                 sendDocument
                 sendSticker
                 sendVideo
                 sendVoice
                 sendLocation
                 sendChatAction
                 getUserProfilePhotos
                 getFile
                 is-message?
                 is-inline_query?
                 is-chosen_inline_result?
                 pollUpdates)
  (import chicken scheme)
  (use srfi-1)
  (use openssl)
  (use http-client)
  (use medea)
  (use loops)
  (use vector-lib)
  (use data-structures)

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

  ;;; framework

  (define (update-predicate type)
    (lambda (update)
      (not (equal? #f (alist-ref type update)))))

  (define is-message?              (update-predicate 'message))
  (define is-inline_query?         (update-predicate 'inline_query))
  (define is-chosen_inline_result? (update-predicate 'chosen_inline_result))

  (define (pollUpdates token handler)
    (let ((offset 0))
      (do-forever
        (vector-for-each (lambda (i u)
                           (handler u)
                           (set! offset (+ 1 (alist-ref 'update_id u))))
                         (alist-ref 'result
                                    (getUpdates token
                                                offset:  offset
                                                timeout: 60))))))
)
