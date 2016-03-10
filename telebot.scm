(module telebot (getMe
                 getUpdates
                 sendMessage
                 sendChatAction
                 forwardMessage
                 getUserProfilePhotos)
  (import chicken scheme)
  (use srfi-1)
  (use openssl)
  (use http-client)
  (use medea)

  (define api-base "https://api.telegram.org/bot")

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
    (syntax-rules ()
    ((wrap-api-method method(parameters ...))
     (define (method
              token
              #!key parameters ...)
       (with-input-from-request
         (get-query-url token (symbol->string 'method))
         (clean-query-parameters
           (map (lambda (l) (cons (first l) (second l)))
                (zip '(parameters ...)
                     (list parameters ...))))
         read-json)))))

  (wrap-api-method getMe())

  (wrap-api-method getUpdates(offset limit timeout))

  (wrap-api-method sendMessage(chat_id
                               text
                               parse_mode
                               disable_web_page_preview
                               disable_notification
                               reply_to_message_id
                               reply_markup))

  (wrap-api-method sendChatAction(chat_id action))

  (wrap-api-method forwardMessage(chat_id
                                  from_chat_id
                                  message_id
                                  disable_notification))

  (wrap-api-method getUserProfilePhotos(user_id
                                        offset
                                        limit))
)
