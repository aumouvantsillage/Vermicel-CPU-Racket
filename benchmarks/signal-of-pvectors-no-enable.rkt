; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
