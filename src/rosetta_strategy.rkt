#lang racket

(require "http.rkt"
         "strategy.rkt"
         "storage.rkt"
         "nlp.rkt")

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

(define (restore-url partial-url)
  (string-append "http://rosettacode.org/" partial-url))

(define (lang-url lang)
  ;; #FIXME: not working for urls with url-encoded chars (C++ -> C%2B%2B)
  (format "http://rosettacode.org/wiki/Category:~a" lang))

(define (normalize-anchors anchors)
  ;; #FIXME: better to use nlp-normalize
  (map (lambda (anchor)
         (cons (car anchor) (string-downcase (cdr anchor))))
       anchors))

(define (fetch-anchors lang)
  ;; #FIXME: in high concurrent env. we need an update lock or
  ;; second checkout after (page-anchors) call.
  (define storage-key (string-append "rosetta_code/" lang))
  (cond [(storage-contains? storage-key)
         (printf "returning `~a` from cache\n" storage-key)
         (storage-read storage-key)]
        [else
         (define lang-info (http-get (lang-url lang)))
         (define normalized-anchors
           (normalize-anchors (page-anchors lang-info)))
         (printf "returning new `~a`\n" storage-key)
         (storage-write storage-key normalized-anchors)
         normalized-anchors]))

(define (similar-enough? terms-synsets title-words)
  (cond [(= (length terms-synsets) (length title-words))
         (= (length title-words)
            (foldl + 0.0 (map nlp-synset-similarity
                              terms-synsets
                              (map list title-words))))]
        ;; #FIXME: should try harder (check for bigrams?)
        [else #f]))

(define get-code-from
  ;; #FIXME: it grabs `output` sections as snippet
  (let ([rx-lang-snippets #rx"<pre.*?>(.*?)</pre>"]
        [rx-pre-tags #rx"</?pre.*?>"])
    (lambda (lang url)
      (let ([rx-lang-block (regexp (format "id=\"~a\".*?<h[23]>" lang))])
        (map (lambda (snippet)
               (regexp-replace* rx-pre-tags snippet ""))
             (regexp-match*
              rx-lang-snippets
              (car (regexp-match rx-lang-block (http-get url)))))))))

(define (fetcher request)
  (match-define (list lang terms) request)
  (define terms-synsets (map nlp-synonyms terms))
  (define anchors (fetch-anchors lang))
  (define url
    (let seek-url ([tail anchors])
      (cond [(null? tail) '()]
            [else
             (define splitted-tail (string-split (cdar tail) " "))
             (let ([similar (similar-enough? terms-synsets splitted-tail)])
               (if similar
                   (caar tail)
                   (seek-url (cdr tail))))])))
  ;; (displayln url)
  (if (null? url)
      '()
      (get-code-from lang (restore-url url))))
  
(define (evaluator result)
  result)

(define formatters
  (for/hash ([name-and-proc
              (list (cons "raw" identity))])
    (values (car name-and-proc) (cdr name-and-proc))))

(define rosetta-strategy
  (make-strategy fetcher evaluator formatters))
   
(provide rosetta-strategy)
