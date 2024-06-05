#lang rr/base

(require threading)
(define (syn k) (datum->syntax #f k))
(define (iterate f n) (if (zero? n) (lambda (x) x) (lambda (x) (f ((iterate f (sub1 n)) x)))))
(require racket/pretty)
(define (exp i k) (pretty-display ((iterate expand-once i) (syn k))))

(define-namespace-anchor na)
(current-namespace (namespace-anchor->namespace na))

'(~> x f g (h y _))
(exp 4 '(~> x f g (h y _)))
(exp 3 '(~> rec 'key-a 'key-b))
(exp 3 '(~> rec :key-a :key-b))
