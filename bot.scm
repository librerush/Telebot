(include "telebot.scm")
(import (prefix telebot telebot:))

(use vector-lib)

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

(define (updates-for-each func updates)
  (vector-for-each (lambda (i u) (func u))
                   (cdr updates)))

(define token (car (command-line-arguments)))

(updates-for-each (lambda (u)
                    (print (cdr (resolve-query (list 'message 'from 'first_name) u))
                           ": "
                           (cdr (resolve-query (list 'message 'text) u))
                           " ("
                           (cdr (resolve-query (list 'update_id) u))
                           ")"))
                  (resolve-query (list 'result)
                                 (telebot:get-updates token)))
