#lang rr/base

:{}
(define x :{ :a 1 :b 2 :c 3 })
x
(:a x)
(x :a)

(:{ :a :{ :b :{ :c :d } } } :a :b :c)
