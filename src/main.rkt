#lang racket

(require net/url)

(require "ostream.rkt"
         "http.rkt")

(define *master* (current-thread))

(define *input* (open-input-file "getcodd-in"))

(define *request-handlers*
  (for/hash ([name-and-proc
              (list (cons "query" handle-query)
                    (cons "ping" (lambda (ostr _) (handle-ping ostr))))])
    (let ([name (car name-and-proc)] [proc (cdr name-and-proc)])
      (values name
              (lambda (output-path params)
                (define ostr (make-ostream output-path))
                (proc ostr params)
                (ostream-close ostr))))))

(define (reopen-input!)
  (displayln "reopening pipe...")
  (close-input-port *input*)
  (set! *input* (open-input-file "getcodd-in")))

(define (query-snippet ostr query-body)
  (sleep 6)
  (displayln "done-query")
  (ostream-write ostr query-body))

(define (handle-query ostr params)
  (match params
    [(list query-type query-body)
     (match query-type
       ;; todo: "source", "docs", "error", ...
       ["snippet" (query-snippet ostr query-body)]
       [_ (printf "unknown query type: `~a`\n" query-type)])]
    [_ (displayln "bad query format")]))

(define (handle-ping ostr)
  (sleep 10)
  (displayln "done-ping")
  (ostream-write ostr "pong"))
              
(define (handle-request request)
  (match (string-split request ";")
    [(list output-path request-type params ...)
     (cond [(hash-has-key? *request-handlers* request-type)
            (define request-handler (hash-ref *request-handlers* request-type))
            (thread
             (lambda ()
               (request-handler output-path params)))]
           [else (printf "unknown request type `~a`\n" request-type)])]
    [_ (displayln "malformed request")]))

(displayln "started listening")
(let listener-loop ()
  (match (read-line *input*)
    [(? eof-object?) (reopen-input!)]
    [request (handle-request request)])
  (listener-loop))