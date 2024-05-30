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
    (provide read-:))

(require
    threading
    racket/match
    "printer-utils.rkt")

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

;; TODO: syntax validation, maybe as a macro
(define (#%record . vs) (record (apply hasheq vs)))
#|  wishlist

    (define e 7)
    (define k
        :{ :a 1  :b 2
           ... :{ :b 3  :c 4  :d 5 }   ;; merging
           :c 6
           :e _ })                     ;; default value-ref
    (equal? k
        :{ :a 1  :b 3  :c 6  :d 5  :e 7 }

    (match-define :{ :a _  :b asdf } k)
    => (define a 1) (define asdf 3)
    using (define-match-expander) with the reader?
    struct for records, with different constructor,
    then prop:match-expander?
|#


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
)
