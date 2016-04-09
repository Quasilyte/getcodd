#lang racket

(require "src/rosetta_strategy.rkt")
(require "src/strategy.rkt")

(define (exec strategy)
  (define results ((strategy-fetcher strategy)
                   (list "Scheme" (list "system" "time"))))
  (for ([result results])
    (displayln result)
    (newline))
  (displayln ((strategy-evaluator strategy) "sd"))
  (define formatters (strategy-formatters strategy))
  (if (hash-has-key? formatters "raw")
      (displayln "ok")
      (displayln "not ok")))

(time
 (exec rosetta-strategy))
