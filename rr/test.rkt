#lang rr/base

:{}
(define x :{ :a 1 :b 2 :c 3 })
x
(:a x)
(x :a)

(:{ :a :{ :b :{ :c :d } } } :a :b :c)

(require racket/match)
(match :{ :a 1 :b 2 :c 3 }
    [:{ } 'yes])

(match :{ :a 1 :b 2 :c 3 }
    [:{ :a j :b k } (list j k)])

(match :{ :a 1 :b 2 :c 3 }
    [:{ :a :b k } (list a k)])

(match :{ :a :{ :b 1 } }
    [:{ :a :{ :b v } } v])

(match (list :{ :a 1 })
    [(list :{ :a v }) v])

(match :{ :a 1 :b 2 :c 3 }
    [:{ :a :b } (list a b)])

(match-define :{ :a :b :c } :{ :a 1 :b 2 :c '(1 2) :d 3 })
(list a b c)

#;(begin 
    (require racket/pretty (for-template rr/record))
    (define-namespace-anchor na)
    (parameterize ([current-namespace (namespace-anchor->namespace na)])
        (~> ':{ :a :b }
            (datum->syntax #f _)
            record-match-expander
            syntax->datum
            pretty-display)
        (~> '(match :{ :a 1 :b 2 :c 3 }
                [:{ :a :b } (list a b)])
            (datum->syntax #f _)
            expand-once
            syntax->datum
)))
