#lang racket

(require json
         net/http-client)

(define (get-response uri)
  (define-values (status header response)
    (http-sendrecv "app.net.sep.com" uri))
  (read-json response))

(provide hired-data)
(define hired-data (get-response "/hired"))

(provide alums-data)
(define alums-data (get-response "/alums"))

