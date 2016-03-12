#lang racket

(require racket/hash
         "api.rkt"
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

(define data-sets
  (hash "Hired" hired-data
        "Alums" alums-data
        "Everyone" (hash-union hired-data alums-data)))

(for/list (((title data-set) (in-hash data-sets)))
  (define data (tenure data-set))
  (printf "Data set '~a'~%" title)
  (dump "Average:" (average data))
  (dump "By year:" (by-year data))
  (dump "3yr pct:" (year-split data 3))
  (printf "~%")
  title)

