#lang racket

(require "http.rkt"
         "ostream.rkt"
         "strategy.rkt"
         "rosetta_strategy.rkt")

(define (query-snippet ostr lang query-body)
  ;; #FIXME: should work with multiple concurrent strategies
  ;; #FIXME: fix list pack/unpack (list is destructured again inside fetcher)
  (define snippets
    ((strategy-fetcher rosetta-strategy)
     (list lang (string-split query-body " "))))
  (ostream-write ostr (string-join snippets "{#delimeter#}")))

(provide query-snippet)

