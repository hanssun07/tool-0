(module reader syntax/module-reader
    (submod rr/record lang)
    #:read        read
    #:read-syntax read-syntax

    (require
        racket/match
        threading
        rr/reader-utils
        (submod rr/record reader))

    #;(define (readtable) (make-readtable-with-record))
    #;(define (+read in)
        (parameterize ([current-readtable (readtable)]) (read in)))

    (define (+read-syntax-debug src in)
        (let ([res (read-syntax src in)])
            (unless (eof-object? res) (~> res syntax->datum displayln))
            res))
    #;(define (+read-syntax src in)
        (parameterize ([current-readtable (readtable)]) (read-syntax src in)))
)
