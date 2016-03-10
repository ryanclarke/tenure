#lang racket

(require json
         net/http-client
         racket/date)
(date-display-format 'iso-8601)

(define (dump a b) (printf "~a ~a~%" a b))

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

(define (get-response uri)
  (define-values (status header response)
    (http-sendrecv "app.net.sep.com" uri))
  (read-json response))

(define (get-date hash property)
  (string->date(hash-ref hash property today)))
(define (good-date? property)
  (equal? default-date property))

(define (service-length l)
  (for/list (((x) (in-list l)))
    (define-values (name hiredate enddate) (apply values x))
    (years-between hiredate enddate)))

(define (per-year l)
  (group-by (λ(x) (floor x)) (sort l <)))

(define (per-count l)
  (for/list (((x) (in-list l)))
    ;(list (round (floor (first x)))
    (length x)))

(define (year-split x i)
  (define y (per-count (per-year x)))
  (exact->inexact (/ (apply + (take y i)) (apply + y))))

(define (average l)
  (exact->inexact (/ (apply + l) (length l))))

(define (normalize h)
  (for/list (((k v) (in-hash h))
             #:unless (or (good-date? (get-date v 'hiredate))
                          (good-date? (get-date v 'enddate))))
    (list k
          (get-date v 'hiredate)
          (get-date v 'enddate))))

(define hired-data (get-response "/hired"))
(define alums-data (get-response "/alums"))

(define hired-output (normalize hired-data))
(define alums-output (normalize alums-data))
(define all-output (append hired-output alums-output))

(define hdata (service-length hired-output))
(define adata (service-length alums-output))
(define xdata (service-length all-output))

(for/list ((x (in-list (list hdata adata xdata)))) 
  (dump "Average:" (average x))
  (dump "By year:" (per-count (per-year x)))
  (dump "3 split:" (year-split x 3))
  (printf "~%"))
