(include "telebot.scm")
(import (prefix telebot telebot:))

(use loops)
(use vector-lib)
(use data-structures)

(define (assure-list value)
  (if (list? value)
    value
    (list)))

(define (resolve-query query tree)
  (fold-right (lambda (x y) (alist-ref x (assure-list y)))
              tree
              (reverse query)))

(define (updates-for-each func updates)
  (vector-for-each (lambda (i u) (func u))
                   updates))

(define offset 0)
(define token (car (command-line-arguments)))

(define (print-message msg)
  (print (resolve-query (list 'message 'from 'first_name) msg)
         ": "
         (resolve-query (list 'message 'text) msg)
         " ("
         (resolve-query (list 'update_id) msg)
         ")"))

(define (echo-message msg)
  (let ((chat_id    (resolve-query (list 'message 'from 'id)   msg))
        (text       (resolve-query (list 'message 'text)       msg))
        (message_id (resolve-query (list 'message 'message_id) msg)))
    (telebot:sendMessage token
                         chat_id:             chat_id
                         text:                text
                         reply_to_message_id: message_id)))

(do-forever
  (updates-for-each (lambda (u)
                      (begin (print-message u)
                             (echo-message u)
                             (set! offset
                               (+ 1 (resolve-query (list 'update_id) u)))))
                    (resolve-query (list 'result)
                                   (telebot:getUpdates token
                                                       offset:  offset
                                                       timeout: 60))))
