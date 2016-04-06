#lang racket

(require net/url)

(define (http-get url-string)
  (call/input-url (string->url url-string)
                  (curry get-pure-port #:redirections 5)
                  port->string))

(provide http-get)
