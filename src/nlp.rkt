#lang racket

(require "wn.rkt")


(define (nlp-normalize words)
  (string-downcase words))

(define nlp-synonyms wn-synonyms)

(define (nlp-synset word)
  ;; #FIXME: what to do with stop-words?
  (nlp-synonyms (nlp-normalize word)))

(define (nlp-synset-similarity a-synset b-synset)
  (define intersection (set-intersect a-synset b-synset))
  (if (= 0 (length intersection))
      0.0
      (exact->inexact 
       (/ (length intersection)
          (min (length a-synset) (length b-synset))))))

(define (nlp-similarity a b)
  (nlp-synset-similarity (nlp-synset a) (nlp-synset b)))

(provide nlp-normalize
         nlp-synonyms
         nlp-synset
         nlp-synset-similarity
         nlp-similarity)


