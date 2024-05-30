(module reader syntax/module-reader
    (submod rr/record lang)
    #:read        +read
    #:read-syntax +read-syntax

    (require
        racket/match
        threading
        rr/reader-utils
        (submod rr/record reader))

    (define (+read in)
        (parameterize ([current-readtable (new-readtable)]) (read in)))

    (define (+read-syntax-debug src in)
        (let ([res (+read-syntax src in)])
            (unless (eof-object? res) (~> res syntax->datum displayln))
            res))
    (define (+read-syntax src in)
        (parameterize ([current-readtable (new-readtable)]) (read-syntax src in)))

    (define (new-readtable)
        (make-readtable (current-readtable)
                        #\: 'terminating-macro
                        dispatch-:))

    (define dispatch-: (case-lambda
        [(ch in)
         (read-: (object-name in) in)]
        [(ch in src line col pos)
         (read-: src in)]))
)
