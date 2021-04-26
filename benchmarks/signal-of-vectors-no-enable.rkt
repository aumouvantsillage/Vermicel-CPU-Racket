; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "common.rkt"
  "../src/signal.rkt")

(define (vector-set vec pos v)
  (define res (vector-copy vec))
  (vector-set! res pos v)
  res)

(define mem (register (make-vector mem-length 0)
              (for/signal (enable address wdata this-reg)
                (if enable
                  (vector-set this-reg address wdata)
                  this-reg))))

(define rdata (for/signal (mem address)
                (vector-ref mem address)))

(run "Signal of vectors, using register" rdata)
