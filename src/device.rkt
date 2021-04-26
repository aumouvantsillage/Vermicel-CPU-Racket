; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require "signal.rkt")

(provide (all-defined-out))

(struct device (start-address size))

(define (device-size-in-words dev)
  (ceiling (/ (device-size dev) 4)))

(define (device-end-address dev)
  (+ (device-start-address dev) (device-size dev) -1))

(define (device-accepts? dev address)
  (<= (device-start-address dev) address (device-end-address dev)))

(define (device-address* dev address)
  (modulo (- address (device-start-address dev))
          (device-size dev)))

(define (device-word-address* dev address)
  (quotient (device-address* dev address) 4))

(define (device-valid dev valid address)
  (for/signal (valid address)
    (and valid (device-accepts? dev address))))

(define (device-address dev address)
  (for/signal (address)
    (device-address* dev address)))

(define (device-word-address dev address)
  (for/signal (address)
    (device-word-address* dev address)))
