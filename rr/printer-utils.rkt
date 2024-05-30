#lang racket/base

(provide define/printer)

(define ((make-printer fn) v port mode)
    (define recur
        (case mode [(#t) write]
                   [(#f) display]
                   [else (lambda (p) (print p (current-output-port) mode))]))
    (parameterize ([current-output-port port])
        (fn v recur)))
(define-syntax-rule
    (define/printer (name args ...) body ...)
    (define name (make-printer (lambda (args ...) body ...))))
