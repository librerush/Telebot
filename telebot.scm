(module telebot (get-me
                 get-updates
                 send-message
                 forward-message)
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

  (define (get-me token)
    (with-input-from-request (get-query-url token "getMe")
                             #f
                             read-json))

  (define (get-updates token
                       #!key offset
                             limit
                             timeout)
    (with-input-from-request
      (get-query-url token "getUpdates")
      (clean-query-parameters
        (list (cons 'offset  offset)
              (cons 'limit   limit)
              (cons 'timeout timeout)))
      read-json))

  (define (send-message token
                        #!key chat-id
                              text
                              parse-mode
                              disable-web-page-preview
                              disable-notification
                              reply-to-message-id
                              reply-markup)
    (with-input-from-request
      (get-query-url token "sendMessage")
      (clean-query-parameters
        (list (cons 'chat_id                  chat-id)
              (cons 'text                     text)
              (cons 'parse_mode               parse-mode)
              (cons 'disable_web_page_preview disable-web-page-preview)
              (cons 'disable_notification     disable-notification)
              (cons 'reply_to_message_id      reply-to-message-id)
              (cons 'reply_markup             reply-markup)))
      read-json))

  (define (forward-message token
                           #!key chat-id
                                 from-chat-id
                                 message-id
                                 disable-notification)
    (with-input-from-request
      (get-query-url token "forwardMessage")
      (list (cons 'chat_id              chat-id)
            (cons 'from_chat_id         from-chat-id)
            (cons 'message_id           message-id)
            (cons 'disable_notification disable-notification))
      read-json))
)
