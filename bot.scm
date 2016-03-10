(include "telebot.scm")
(import (prefix telebot telebot:))

(define (assure-list value)
  (if (list? value)
    value
    (list)))

(define (find-pair symbol nodes)
  (find (lambda (x) (equal? symbol (car x)))
        (filter pair? nodes)))

(define (resolve-query query tree)
  (fold-right (lambda (x y) (find-pair x (assure-list y)))
              tree
              (reverse query)))

(define token (car (command-line-arguments)))

(print (resolve-query (list 'result 'username)
                      (telebot:get-me token)))

(pretty-print (resolve-query (list 'result)
                             (telebot:get-updates token)))
