#lang racket

(require "src/rosetta_strategy.rkt")
(require "src/strategy.rkt")

(define (exec strategy)
  (displayln ((strategy-fetcher strategy) (list "Haskell" "=)")))
  (displayln ((strategy-evaluator strategy) "sd"))
  (define formatters (strategy-formatters strategy))
  (if (hash-has-key? formatters "raw")
      (displayln "ok")
      (displayln "not ok")))

(exec rosetta-strategy)
