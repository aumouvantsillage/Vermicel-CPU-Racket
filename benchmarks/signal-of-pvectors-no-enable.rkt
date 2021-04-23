#lang racket

(require
  data/pvector
  (only-in data/collection
     nth set-nth)
  "common.rkt"
  "../src/signal.rkt")

(define mem (register (make-pvector mem-length 0)
              (for/signal (enable address wdata this-reg)
                (if enable
                  (set-nth this-reg address wdata)
                  this-reg))))

(define rdata (for/signal (mem address)
                (nth mem address)))

(run "Signal of persistent vectors, using register" rdata)
