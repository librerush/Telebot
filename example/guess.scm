(import (prefix (telebot) telebot:)
        (chicken process-context)
        (srfi 1))

(define (resolve-query query tree)
  (fold (lambda (x y) (alist-ref x y))
        tree
        query))

(define (make-sender token chat_id)
  (lambda (text)
    (print chat_id " <- \"" text "\"")
    (telebot:sendMessage token
                         chat_id: chat_id
                         text:    text)))

(define (make-conversation token chat_id)
  (let* ((chat_id chat_id)
         (send    (make-sender token chat_id))
         (answer  (random 100)))
    (send "Hi there! I just generated a random number for you to guess!")
    (lambda (update)
      (let* ((text  (resolve-query '(message text) update))
             (guess (string->number text)))
        (print chat_id " -> \"" text "\"")
        (if (number? guess)
          (cond ((= guess answer)
                 (begin (send "Correct! Feel free to guess the next number.")
                        (set! answer (random 100))))
                ((< guess answer) (send "Too small. Try again."))
                ((> guess answer) (send "Too large. Try again.")))
          (send "This is not a number - please provide your guess in base 10."))))))

(let* ((token    (car (command-line-arguments)))
       (converse (telebot:make-conversation-manager token
                                                    make-conversation)))
  (randomize)
  (telebot:poll-updates token converse))
