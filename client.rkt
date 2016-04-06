#lang racket

(display "input file: ")
(define input-file-name (read-line))

(define input (open-input-file input-file-name))
(define output (open-output-file "getcodd-in" #:exists 'append)) 

(let loop ()
  (let ([query (read-line)])
    (match query
      [(or "q" "quit")
       (close-output-port output)
       (exit)]
      [(or "t" "truncate")
       (close-input-port input)
       (system (format "truncate \"~a\" --size 0" input-file-name))
       (set! input (open-input-file input-file-name))]
      [(or "r" "read")
       (define resp (read input))
       (displayln resp)]
      [_
       (displayln query output)
       (flush-output output)]))
  (loop))
