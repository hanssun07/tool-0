#lang rr/base

(require
    threading
    racket/promise)

(struct source (inner))

(define (make-source get)
    (define (next-node prev)
        (let ([pr (delay (force prev) (get))])
            (cons pr (delay (next-node pr)))))
    (source (next-node (delay #f))))
(define (source-next s)
    (~> s source-inner cdr force source))
(define (source-get s)
    (~> s source-inner car force))

(define (channel->source ch)
    (make-source (lambda () (channel-get ch))))

(require racket/async-channel)
(define (async-channel->source ach)
    (make-source (lambda () (async-channel-get ach))))

(require racket/place)
(define (place-channel->source pch)
    (make-source (lambda () (place-channel-get pch))))

(require racket/stream)
(define (stream->source s)
    (make-source (lambda () (begin0 (stream-first s) (set! s (stream-rest s))))))

(define (source->stream s)
    (stream-cons (source-get s) (source->stream (source-next s))))
