#lang racket

(require racket/date)

(date-display-format 'iso-8601)

;(provide (all-defined-out))

(define (string->date date)
  (define ymd-string-list (string-split date "-" #:repeat? #t))
  (define ymd-nums (for/list ((n ymd-string-list))
                     (string->number n)))
  (define dmy-nums (reverse ymd-nums))
  (seconds->date (apply find-seconds 0 0 0 dmy-nums)))

(define default-date "2001-01-01")

(define today (date->string (current-date)))

(define (years-between hired end)
  (abs (/ (- (date->seconds hired)
             (date->seconds end))
          31536000)))

(define (get-date hash property)
  (string->date(hash-ref hash property today)))

(define (good-date? property)
  (equal? default-date property))

(define (employment-dates h)
  (for/list (((k v) (in-hash h))
             #:unless (or (good-date? (get-date v 'hiredate))
                          (good-date? (get-date v 'enddate))))
    (list (get-date v 'hiredate)
          (get-date v 'enddate))))

(provide tenure)
(define (tenure h)
  (define l (employment-dates h))
  (for/list (((x) (in-list l)))
    (define-values (hiredate enddate) (apply values x))
    (years-between hiredate enddate)))

