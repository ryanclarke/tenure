#lang racket

(require json
         net/http-client
         racket/date)
(date-display-format 'iso-8601)


(define (get-response uri)
  (define-values (status header response)
    (http-sendrecv "app.net.sep.com" uri))
  (read-json response))

(define hired-data (get-response "/hired"))
(define alum-data (get-response "/alums"))

(define (hired-parse h)
  (for/list (((k v) (in-hash h))
             #:unless (equal? "2001-01-01" (hash-ref v 'hiredate)))
    (list k
          (hash-ref v 'hiredate)
          (date->string (current-date)))))
(define hired-output (hired-parse hired-data))

(define (alum-parse h)
  (for/list (((k v) (in-hash h))
             #:unless (or (equal? "2001-01-01" (hash-ref v 'hiredate))
                          (equal? "2001-01-01" (hash-ref v 'enddate))))
    (list k
          (hash-ref v 'hiredate)
          (hash-ref v 'enddate))))
(define alum-output (alum-parse alum-data))

(define output (append hired-output alum-output))
(printf "~a~%" output)