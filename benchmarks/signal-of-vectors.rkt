#lang racket

(require
  "common.rkt"
  "../src/signal.rkt")

(define (vector-set vec pos v)
  (define res (vector-copy vec))
  (vector-set! res pos v)
  res)

(define mem (register/e (make-vector mem-length 0) enable
              (for/signal (address wdata this-reg)
                (vector-set this-reg address wdata))))

(define rdata (for/signal (mem address)
                (vector-ref mem address)))

(run "Signal of vectors, using register/e" rdata)
