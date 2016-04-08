#lang racket

(require "wn.rkt")

(define (nlp-normalize words)
  (string-downcase words))

(define nlp-synonyms wn-synonyms)

(define (nlp-similarity a b)
  (let ([a-synset (nlp-synonyms (nlp-normalize a))]
        [b-synset (nlp-synonyms (nlp-normalize b))])
    (define intersection (set-intersect a-synset b-synset))
    (if (= 0 (length intersection))
        0.0
        (exact->inexact 
         (/ (length intersection)
            (min (length a-synset) (length b-synset)))))))

(provide nlp-normalize
         nlp-synonyms
         nlp-similarity)


