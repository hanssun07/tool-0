#lang racket/base

(provide
    (rename-out
        [record?      record?]
        [record-value record->hasheq]
        [key?         record-key?]
        [key-name     record-key->symbol]))

(module+ lang
    (provide
        (all-from-out racket/base)
        ~>
        #%record #%key)
    (require racket/base))
(module+ reader
    ; side effect of visit:
    ; (current-readtable (make-readtable-with-record))
)

(require
    threading
    racket/match
    racket/hash
    "printer-utils.rkt"
    (for-syntax
        threading
        racket/base
        racket/match
        syntax/parse))

(define (univ-ref x . is)
    (if (null? is) x
     (let ([i (car is )] [ir (cdr is)])
      (apply univ-ref (cond [(procedure? i) (i x)]
                      [(list?      x) (list-ref i x)])
                      ir))))
(define record-ref* univ-ref)
(define/printer (record-print r recur)
    (write-string ":{")
    (for ([(key value) (~> r record-value in-hash)])
        (printf " ~a " key)
        (recur value))
    (unless (~> r record-value hash-empty?) (write-string " "))
    (write-string "}"))

(struct record (value)
    #:property prop:procedure record-ref*
    #:methods gen:custom-write
        [(define write-proc record-print)])

(define empty-record (record (hasheq)))
(define (make-record . vs) (record (apply hasheq vs)))
(define (record-merge-< . rs)
    (cond [(null? rs)        empty-record]
          #;[(~> rs cdr null?) (car rs)]
          [#t (~> rs
                  (map record-value _)
                  (apply hash-union _ #:combine (lambda (_ y) y))
                  record)]))

(begin-for-syntax
    (define (keysym->key k)
        (~> k symbol->string string->list cdr list->string string->symbol))
    (define ((over-syntax f) s [lex s])
        (~> s syntax->datum f (datum->syntax lex _)))
    (define-syntax-rule
        (syntax-cases a ...)
        (lambda (stx) (syntax-case stx a ...)))
    (define record-match-expander (syntax-cases (#%key)
        [(root) #'(? record?)]
        [(root (#%key k))
         (with-syntax ([id ((over-syntax keysym->key) #'k #'root)])
                 #'(record (hash* [(make-key 'k) id])))]
        [(root (#%key k) (#%key j) r ...)
         (with-syntax ([id ((over-syntax keysym->key) #'k #'root)]
                       [rest (record-match-expander #'(root (#%key j) r ...))])
            #'(and (record (hash* [(make-key 'k) id]))
                   rest))]
        [(root (#%key k) v r ...)
         (with-syntax ([rest (record-match-expander #'(root r ...))])
            #'(and (record (hash* [(make-key 'k) v]))
                   rest))]))
    (define (record-literal-expander stx)
        (define lex (syntax-cases (#%key)
            [(root) '()]
            #;[(root (#%key k))
             `((implicit ,#'k)  )]
            [(root (#%key k) (#%key j) r ...)
             `((implicit ,#'k)  ,@(lex #'(root (#%key j) r ...)))]
            [(root (#%key k) v r ...)
             (~> #'v syntax->datum (eq? '...) not)
             `((pair ,#'(make-key 'k) ,#'v) ,@(lex #'(root r ...)))]
            [(root (#%key k) r ...)
             `((implicit ,#'k)  ,@(lex #'(root r ...)))]
            [(root ooo v r ...)
             (~> #'ooo syntax->datum (eq? '...))
             `((inline ,#'v)    ,@(lex #'(root r ...)))]))
        (define (sanitize tok) (match tok
            [`(implicit ,k) `(pair ,#`(make-key '#,k) ,((over-syntax keysym->key) k stx))]
            [v v]))
        (define (gather x y) (match (list x y)
            [`((pair ,k ,v) ((pair ,p ...) ,@r))
             `((pair ,k ,v ,@p) ,@r)]
            [`(,v ,r)
             `(,v ,@r)]))
        (define (transform-group g) (match g
            [`(pair ,s ...) (cons #'make-record s)]
            [`(inline ,v)   v]))
        (~> stx
            lex
            (map sanitize _)
            (foldr gather '() _)
            (map transform-group _)
            (cons #'record-merge-< _)
            (datum->syntax stx _))))

(define-match-expander #%record
    record-match-expander
    record-literal-expander)


(define/printer (key-print key recur)
    (~> key key-name display))
(define (key-invoke key record)
    (hash-ref (record-value record) key #f))

(struct key (name)
    #:property prop:procedure key-invoke
    #:methods gen:custom-write
        [(define write-proc key-print)])

(define key-registry (make-hasheq))
(define (make-key name)
    (hash-ref! key-registry name (lambda () (key name))))
    
(define-syntax-rule (#%key a) (make-key (quote a)))

(module+ reader
    (require "reader-utils.rkt")

    (define/reader (read-: src in)
        (match (peek-char in)
            [(? char-whitespace?) ':]
            [(? eof-object?)      ':]
            [#\{                  (read-char in)
                                  `(#%record ,@(read-:map src in))]
            [_                    (read-:key src in)]))
        
    (define/reader (read-:key src in)
        `(#%key ,(~> in read symbol->string (string-append ":" _) string->symbol)))
    (define/reader (read-:map src in)
        (skip-whitespace in)
        (match (peek-char in)
            ; TODO: proper error handling, see
            ; https://docs.racket-lang.org/guide/hash-reader.html#%28part._readtable%29
            [(? eof-object?)    (error)]
            ;; other closing brackets handled by recursive read dispatch, probably
            [#\}                (read-char in)
                                '()]
            [_                  (cons (read-syntax/recursive src in) (read-:map src in))]))

    (define dispatch-: (case-lambda
        [(ch in)
         (read-: (object-name in) in)]
        [(ch in src line col pos)
         (read-: src in)]))
    (define (make-readtable-with-record [from-readtable (current-readtable)])
        (make-readtable from-readtable
                        #\: 'terminating-macro
                        dispatch-:))
    (current-readtable (make-readtable-with-record))
)
