#lang racket

(require "rc_cell.rkt")

(define *ostreams* (make-hash))

(define (make-ostream path)
  (cond [(hash-has-key? *ostreams* path)
         (printf "reusing ostream `~a`\n" path)
         (define boxed-ostr (hash-ref *ostreams* path))
         (hash-set! *ostreams* path (rc-cell-increase boxed-ostr))
         (rc-cell-unbox boxed-ostr)]
        [else
         (printf "new ostream `~a`\n" path)
         (define ostr (cons path
                            (open-output-file path #:exists 'append)))
         (hash-set! *ostreams* path (make-rc-cell ostr))
         ostr]))

(define (ostream-path ostr)
  (car ostr))

(define (ostream-file ostr)
  (cdr ostr))

(define (ostream-write ostr data)
  (define output-port (ostream-file ostr))
  (write data output-port)
  (flush-output output-port))

(define (ostream-close ostr)
  (define path (ostream-path ostr))
  (cond [(hash-has-key? *ostreams* path)
         (define boxed-ostr (hash-ref *ostreams* path))
         (cond [(rc-cell-unique? boxed-ostr)
                (printf "closing `~a` ostream\n" path)
                (close-output-port (ostream-file ostr))
                (hash-remove! *ostreams* path)]
               [else
                (printf "decrease ref-count for `~a` ostream\n" path)
                (hash-set! *ostreams* path (rc-cell-decrease boxed-ostr))])]
        [else (printf "attempt to close non-existing ostream `~a`\n" path)]))

(provide make-ostream
         ostream-write
         ostream-close)
