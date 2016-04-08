#lang racket

(require "wn.rkt")

(define (nlp-normalize words)
  (string-downcase words))

(define nlp-synonyms wn-synonyms)

(define (nlp-similarity a b)
  (define a-extended (nlp-synonyms (nlp-normalize a)))
  (define b-extended (nlp-synonyms (nlp-normalize b)))
  (length (set-intersect a-extended b-extended)))

(provide nlp-normalize
         nlp-synonyms
         nlp-similarity)


