#lang racket

(require net/http-client json)
(define-values (status header response)
  (http-sendrecv "app.net.sep.com" "/hired"))
(define data (read-json response))
(printf "~a~%" data)