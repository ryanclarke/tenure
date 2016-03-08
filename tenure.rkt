#lang racket

(require json
         net/http-client
         racket/date)
(date-display-format 'iso-8601)

(define default-date "2001-01-01")
(define today (date->string (current-date)))

(define (get-response uri)
  (define-values (status header response)
    (http-sendrecv "app.net.sep.com" uri))
  (read-json response))

(define (get-date hash property)
  (hash-ref hash property today))
(define (good-date? property)
  (equal? default-date property))

(define (normalize h)
  (for/list (((k v) (in-hash h))
             #:unless (or (good-date? (get-date v 'hiredate))
                          (good-date? (get-date v 'enddate))))
    (list k
          (get-date v 'hiredate)
          (get-date v 'enddate))))

(define hired-data (get-response "/hired"))
(define alum-data (get-response "/alums"))

(define hired-output (normalize hired-data))
(define alum-output (normalize alum-data))

(define output (append hired-output alum-output))
(printf "~a~%" output)