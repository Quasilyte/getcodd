#lang racket

;; #todo: add stats
(define-struct strategy
  (fetcher evaluator formatters))

(provide (struct-out strategy))
