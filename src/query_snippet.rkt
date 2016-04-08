#lang racket

(require "http.rkt"
         "ostream.rkt")

(define (query-snippet ostr query-body)
  (for ([i (in-range 0 3)]) (http-get-async "http://example.org" (current-thread)))
  (define responses
    (for/list ([response (in-range 0 3)])
      (displayln "got-resp")
      (string-length (thread-receive))))
  (display responses)
  (ostream-write ostr query-body))

(provide query-snippet)

