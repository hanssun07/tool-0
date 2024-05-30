#lang racket/base

(provide
    srcloc
    skip-whitespace
    define/reader)

; TODO: properly handle span
(define (srcloc src in)
    (define-values (line col pos) (port-next-location in))
    (and line (vector src line col pos 1)))
(define (skip-whitespace in)
    (when (char-whitespace? (peek-char in))
        (read-char in)
        (skip-whitespace in)))

(define ((reader-wrap fn) src in)
    (define loc (srcloc src in))
    (datum->syntax #f (fn src in) loc))
(define-syntax-rule
    (define/reader (name args ...) body ...)
    (define name (reader-wrap (lambda (args ...) body ...))))


