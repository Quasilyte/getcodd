#lang racket

(define (w/o-parens s)
  (regexp-replace #rx"\\(.*?\\)" s ""))

(define (split-by-comma s)
  (string-split s ","))

(define (trim-all list-of-strings)
  (map string-trim list-of-strings))

(define (flatmap proc elts)
  (flatten (map proc elts)))

(define (arrows-scan input)
  (define rx-arrow #rx"=>[^\n]+")
  (define (arrows-split s)
    (string-split (substring (w/o-parens s) 3) ","))
  (trim-all (flatmap arrows-split (regexp-match* rx-arrow input))))

(define (senses-scan input)
  (define rx-senses #rx"Sense [0-9]+\n[^\n]+")
  (define (senses-split s)
    (string-split (w/o-parens s) "\n"))
  (trim-all
   (flatmap split-by-comma
            (map second
                 (map senses-split (regexp-match* rx-senses input))))))

(define (wn-exec word params)
  (with-output-to-string
    (lambda ()
      (apply system*
             (append (list "nlp/wordnet" word)
                     params)))))
                   
(define (wn-synonyms word)
  (define wn-output (wn-exec word (list "-synsn" "-synsv" "-synsa" "-synsr")))
  (remove-duplicates (append (senses-scan wn-output)
                             (arrows-scan wn-output))))

(provide wn-synonyms)

