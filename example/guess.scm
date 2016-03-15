(require-extension telebot
                   (prefix telebot telebot:))

(use srfi-69)
(use extras)
(use data-structures)

(define (resolve-query query tree)
  (fold (lambda (x y) (alist-ref x y))
        tree
        query))

(define token (car (command-line-arguments)))
(define conversations (make-hash-table))

(define (send chat_id text)
  (telebot:sendMessage token
                       chat_id: chat_id
                       text:    text))

(define (print-failed-guess chat_id answer guess)
  (print "User " chat_id " guessed " guess " instead of " answer))

(define (make-conversation chat_id)
  (let ((chat_id   chat_id)
        (answer    (random 100)))
    (lambda (text)
      (let ((guess (string->number text)))
        (if (equal? #f guess)
          (send chat_id
                "This is not a number - please provide your guess in base 10.")
          (cond ((= guess answer)
                 (begin (send chat_id
                              "Correct! Feel free to guess the next number.")
                        (print "User " chat_id " correctly guessed " answer)
                        (set! answer (random 100))))
                ((< guess answer)
                 (begin (send chat_id
                              "Too small. Try again.")
                        (print-failed-guess chat_id answer guess)))
                ((> guess answer)
                 (begin (send chat_id
                              "Too large. Try again.")
                        (print-failed-guess chat_id answer guess)))))))))

(define (handle-message m)
  (let ((chat_id (resolve-query '(message from id) m))
        (text    (resolve-query '(message text)    m)))
    (if (hash-table-exists? conversations chat_id)
      ((hash-table-ref conversations chat_id) text)
      (begin (hash-table-set! conversations chat_id (make-conversation chat_id))
             (print "Started talking to user " chat_id)
             (send chat_id
                   "Hi there! I just generated a random number for you to guess!")))))

(randomize)
(telebot:pollUpdates token
                     (lambda (u) (if (telebot:is-message? u)
                                   (handle-message u))))
