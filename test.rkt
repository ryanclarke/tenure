#lang racket

(module test racket
  (require test-engine/racket-tests)
  (require "tenure.rkt")
  (check-within (average (list 1 3)) 2 1)
  (test))