(module telebot (get-me get-updates send-message)
  (import chicken scheme)
  (use openssl)
  (use http-client)
  (use medea)

  (define api-base "https://api.telegram.org/bot")

  (define (get-query-url token method)
    (string-append api-base token "/" method))

  (define (get-me token)
    (with-input-from-request (get-query-url token "getMe")
                             #f
                             read-json))

  (define (get-updates token)
    (with-input-from-request (get-query-url token "getUpdates")
                             #f
                             read-json))

  (define (send-message token user message)
    (with-input-from-request (get-query-url token "sendMessage")
                             (list (cons 'chat_id user)
                                   (cons 'text    message))
                             read-json))
)
