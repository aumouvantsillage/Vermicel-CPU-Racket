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

(define mem (register/e (make-pvector mem-length 0) enable
              (for/signal (address wdata this-reg)
                (set-nth this-reg address wdata))))

(define rdata (for/signal (mem address)
                (nth mem address)))

(run "Signal of persistent vectors, using register/e" rdata)
