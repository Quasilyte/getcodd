#lang racket

(define (make-rc-cell datum)
  (cons 1 datum))

(define (rc-cell-refs rc-cell)
  (car rc-cell))

(define (rc-cell-unbox rc-cell)
  (cdr rc-cell))

(define (rc-cell-unique? rc-cell)
  (= 1 (rc-cell-refs rc-cell)))

(define (rc-cell-increase rc-cell)
  (cons (add1 (rc-cell-refs rc-cell)) (rc-cell-unbox rc-cell)))

(define (rc-cell-decrease rc-cell)
  (cons (sub1 (rc-cell-refs rc-cell)) (rc-cell-unbox rc-cell)))

(provide make-rc-cell
         rc-cell-unbox
         rc-cell-unique?
         rc-cell-increase
         rc-cell-decrease)
