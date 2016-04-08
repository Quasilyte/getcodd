#lang racket

(require net/url)

(define (http-get url-string)
  (call/input-url (string->url url-string)
                  (curry get-pure-port #:redirections 5)
                  port->string))

(define (http-get-async url-string master)
  (thread
   (lambda ()
     (thread-send master (http-get url-string)))))

(provide http-get
         http-get-async)
