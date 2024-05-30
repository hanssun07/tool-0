#lang rr/base

(require 
    crypto crypto/gcrypt
    racket/format
    racket/file)

(define ->id
    (let* ([digest-impl (get-digest 'blake2s-128 gcrypt-factory)]
           [digest (lambda (x) (digest digest-impl x))])
        (unless digest-impl (error "did not find impl for blake2s-128"))
        (lambda (str)
            (~> str digest bytes->hex-string))))

(define (->doc v)
    (define v-str (~a v))
    :{ :id   (->id v-str)
       :at   'UNIMPL
       :val  v-str })
