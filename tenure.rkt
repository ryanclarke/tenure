#lang racket

(require "api.rkt"
         "date.rkt")

(provide (all-defined-out))

(define (dump a b) (printf "~a ~a~%" a b))

(define (per-year l)
  (group-by (λ(x) (floor x)) (sort l <)))

(define (per-count l)
  (for/list (((x) (in-list l)))
    (length x)))

(define (by-year l)
  (per-count (per-year l)))

(define (year-split l i)
  (define x (by-year l))
  (exact->inexact (/ (apply + (take x i)) (apply + x))))

(define (average l)
  (exact->inexact (/ (apply + l) (length l))))

(define hired-output (employment-dates hired-data))
(define alums-output (employment-dates alums-data))
(define all-output (append hired-output alums-output))

(define hdata (tenure hired-output))
(define adata (tenure alums-output))
(define xdata (tenure all-output))

(for/list ((c (in-list (list (cons "Hired" hdata)
                             (cons "Alums" adata)
                             (cons "Everyone" xdata)))))
  (define title (car c))
  (define data (cdr c))
  (printf "Data set '~a'~%" title)
  (dump "Average:" (average data))
  (dump "By year:" (by-year data))
  (dump "3yr pct:" (year-split data 3))
  (printf "~%")
  title)

