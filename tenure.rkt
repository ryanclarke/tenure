#lang racket

(require json
         net/http-client
         racket/date)
(date-display-format 'iso-8601)
(define-values (status header response)
  (http-sendrecv "app.net.sep.com" "/hired"))
(define data (read-json response))
(define (pp h)
  (for/list ([(k v) (in-hash h)])
             (values (list k
                           (hash-ref v 'hiredate)
                           (date->string (current-date))))))
(define output (pp data))
(printf "~a~%" output)