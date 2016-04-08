#lang racket

(require "wn.rkt")

(define (nlp-normalize words)
  (string-downcase words))

(define nlp-synonyms wn-synonyms)

(provide nlp-normalize
         nlp-synonyms)


