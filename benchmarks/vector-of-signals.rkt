; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "common.rkt"
  "../src/signal.rkt")

(define mem (for/vector ([n (in-range mem-length)])
              (define e (for/signal (enable address)
                          (and enable (= n address))))
              (register/e 0 e wdata)))

; Like vector-ref, but with a vector of signals.
(define (signal-vector-ref vec pos)
  (signal-cons
    (signal-first (vector-ref vec (signal-first pos)))
    (signal-vector-ref (vector-map signal-rest vec) (signal-rest pos))))

(define rdata (signal-vector-ref mem address))

(run "Vector of signals" rdata)
