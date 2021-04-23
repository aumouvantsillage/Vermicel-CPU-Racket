#lang racket

(require
  data/pvector
  (only-in data/collection
     nth set-nth)
  "common.rkt"
  "../src/signal.rkt")

(define mem (register/e (make-pvector mem-length 0) enable
              (for/signal (address wdata this-reg)
                (set-nth this-reg address wdata))))

(define rdata (for/signal (mem address)
                (nth mem address)))

(run "Signal of persistent vectors, using register/e" rdata)
