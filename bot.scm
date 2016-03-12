(include "telebot.scm")
(import (prefix telebot telebot:))

(use loops)
(use vector-lib)
(use data-structures)

(define (resolve-query query tree)
  (fold (lambda (x y) (alist-ref x y))
        tree
        query))

(define (updates-for-each func updates)
  (vector-for-each (lambda (i u) (func u))
                   updates))

(define offset 0)
(define token (car (command-line-arguments)))

(define (print-message msg)
  (print (resolve-query '(message from first_name) msg)
         ": "
         (resolve-query '(message text) msg)
         " ("
         (resolve-query '(update_id) msg)
         ")"))

(define (echo-message msg)
  (let ((chat_id    (resolve-query '(message from id) msg))
        (text       (resolve-query '(message text)    msg)))
    (telebot:sendMessage token
                         chat_id: chat_id
                         text:    text)))

(do-forever
  (updates-for-each (lambda (u)
                      (begin (print-message u)
                             (echo-message u)
                             (set! offset
                               (+ 1 (resolve-query '(update_id) u)))))
                    (resolve-query '(result)
                                   (telebot:getUpdates token
                                                       offset:  offset
                                                       timeout: 60))))
