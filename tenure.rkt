#lang racket

(require json
         net/http-client
         racket/date)
(date-display-format 'iso-8601)

(define (dump x) (printf "~a~%" x))

(define (string->date date)
  (define ymd-string-list (string-split date "-" #:repeat? #t))
  (define ymd-nums (for/list ((n ymd-string-list))
                     (string->number n)))
  (define dmy-nums (reverse ymd-nums))
  (seconds->date (apply find-seconds 0 0 0 dmy-nums)))
(define default-date "2001-01-01")
(define today (date->string (current-date)))
(define (years-between hired end)
  (exact->inexact (abs (/ (- (date->seconds hired)
                        (date->seconds end))
                     31536000))))

(define (get-response uri)
  (define-values (status header response)
    (http-sendrecv "app.net.sep.com" uri))
  (read-json response))

(define (get-date hash property)
  (string->date(hash-ref hash property today)))
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

(define all (append hired-output alum-output))

(define (person-duration l)
  (for/list (((x) (in-list l)))
      (list (first x)
            (years-between (third x) (second x)))))
 
(dump (person-duration all))

