(require-extension telebot
                   (prefix telebot telebot:))

(import (chicken process-context))
(import (srfi 1))

(define token (car (command-line-arguments)))

(define (print-message msg)
  (print (resolve-query '(message from first_name) msg)
         ": "
         (resolve-query '(message text) msg)
         " ("
         (resolve-query '(update_id) msg)
         ")"))

(define (echo-message msg)
  (let ((chat_id (resolve-query '(message from id) msg))
        (text (resolve-query '(message text) msg)))
    (telebot:sendMessage token
                         chat_id: chat_id
                         text: text)))

(telebot:poll-updates token
                      (lambda (u)
                        (if (telebot:is-message? u)
                            (begin (print-message u)
                                   (echo-message  u)))))
