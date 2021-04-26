; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require "../src/signal.rkt")

(provide (all-defined-out))

(define mem-length 1000)
(define duration 10000)

(define signal-not  (signal-lift* not  _))
(define signal-add1 (signal-lift* add1 _))

(define enable  (register #f (signal-not this-reg)))
(define wdata   (register/e 1 enable (signal-add1 this-reg)))
(define address (for/signal (wdata)
                  (modulo wdata mem-length)))

(define (run title sig)
  (displayln (format "---- ~a" title))
  (collect-garbage)
  (define init-mem-use (current-memory-use))
  (time (signal-take sig duration))
  (define final-mem-use (current-memory-use))
  (displayln (format "Memory use: initial=~aM final=~aM delta=~aM"
               (/ init-mem-use                   1e6)
               (/ final-mem-use                  1e6)
               (/ (- final-mem-use init-mem-use) 1e6))))
