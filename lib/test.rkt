#lang rr/base

(require
    threading
    (for-syntax threading)
)

(define (iterate f n) (if (zero? n) (lambda (x) x) (lambda (x) (f ((iterate f (sub1 n)) x)))))
(define-namespace-anchor na)
(parameterize ([current-namespace (namespace-anchor->namespace na)])
    (displayln ((iterate expand-once 2) (datum->syntax #f '(~> rec 'key))))
    (displayln ((iterate expand-once 2) (datum->syntax #f '(~> rec :key))))
)
