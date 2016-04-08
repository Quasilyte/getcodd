#lang racket

(define (storage-key key)
  (string-append "storage/" key))

(define (storage-contains? key)
  (file-exists? (storage-key key)))

(define (storage-write key data)
  (with-output-to-file (storage-key key) #:exists 'truncate
    (lambda () (write data))))

(define (storage-read key)
  (with-input-from-file (storage-key key)
    (lambda () (read))))

(provide storage-contains?
         storage-write
         storage-read)
