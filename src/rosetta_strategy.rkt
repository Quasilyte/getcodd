#lang racket

(require "http.rkt"
         "strategy.rkt"
         "storage.rkt")

(define page-anchors
  (let ([rx-page-haystack
         #rx"<h2>Pages in category"]
        [rx-page-anchors
         (regexp
          (format "\\<a href=\"~a(?!~a).+?\" title=\".+?\"\\>"
                  ;; base url
                  "(https?://(www.)?rosettacode.org/wiki/|/wiki/)"
                  ;; excluded prefixes:
                  (string-join '("Category:"
                                 "Help:"
                                 "Special:"
                                 "Reports:"
                                 "Rosetta_Code:"
                                 "Category_talk:")
                               "|")))]
        [rx-anchor-attributes
         #rx"href=\"(.*?)\" title=\"(.*?)\""])
    (lambda (page)
      (define haystack-offset
        (caar (regexp-match-positions rx-page-haystack page)))
      (define haystack (substring page haystack-offset))
      (define anchors
        (for/list ([pos (regexp-match-positions* rx-page-anchors haystack)])
          (define matches
            (regexp-match rx-anchor-attributes haystack (car pos) (cdr pos)))
          (cond [matches (cons (second matches) (third matches))]
                [else
                 (displayln "rosetta-strategy: fetch-anchors failed")
                 (cons "" "")])))
      (cond [(= 0 (length anchors))
             (displayln "rosetta-strategy: fetch-anchors failed")
             '()]
            [else anchors]))))

(define (lang-url lang)
  ;; #FIXME: not working for urls with url-encoded chars (C++ -> C%2B%2B)
  (format "http://rosettacode.org/wiki/Category:~a" lang))

(define (fetch-anchors lang)
  ;; #FIXME: in high concurrent env. we need an update lock or
  ;; second checkout after (page-anchors) call.
  (define storage-key (string-append "rosetta_code/" lang))
  (cond [(storage-contains? storage-key)
         (printf "returning `~a` from cache\n" storage-key)
         (storage-read storage-key)]
        [else
         (define lang-info (http-get (lang-url lang)))
         (define anchors (page-anchors lang-info))
         (printf "returning new `~a`\n" storage-key)
         (storage-write storage-key anchors)
         anchors]))

(define (similar-enough? terms-synsets title-words)
  (cond [(= (length terms-synsets) (length title-words))
         (= (length title-words)
            (foldl + 0.0 (map nlp-synset-similarity
                              terms-synsets
                              (map list title-words))))]
        ;; #FIXME: should try harder (check for bigrams?)
        [else #f]))

(define (fetcher request)
  (match-define (list lang terms) request)
  (define terms-synsets (map nlp-synonyms terms))
  (define anchors (fetch-anchors lang))
  (length anchors))

(define (evaluator result)
  result)

(define formatters
  (for/hash ([name-and-proc
              (list (cons "raw" identity))])
    (values (car name-and-proc) (cdr name-and-proc))))

(define rosetta-strategy
  (make-strategy fetcher evaluator formatters))
   
(provide rosetta-strategy)
