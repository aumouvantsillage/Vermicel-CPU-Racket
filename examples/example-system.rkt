; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../src/logic.rkt"
  "../src/device.rkt"
  "../src/signal.rkt"
  "../src/virgule.rkt"
  "../src/memory.rkt"
  "../src/vcd.rkt")

(provide
  ram-dev
  txt-dev
  system-run)


; The memory map of this system.
(define ram-dev (device #x00000000 65536))
(define txt-dev (device #x80000000     8))

(define-signal (text-output #:valid valid #:address address #:wstrobe wstrobe #:wdata wdata)
  #:returns (ready rdata)
  ; Only accept data written at word-aligned address.
  (define wdata-slice (match wstrobe
                        [#b0001 (unsigned-slice wdata  7 0)]
                        [#b0011 (unsigned-slice wdata 15 0)]
                        [#b1111 wdata]
                        [_      #f]))
  ; Address 0: display an integer on it own line.
  ; Address 1: display a single character.
  (when (and valid wdata-slice)
    (match address
      [0 (displayln wdata-slice)]
      [1 (display (integer->char wdata-slice))]
      [_ (void)]))
  ; This device is ready immediately and is write-only.
  (define ready valid)
  (define rdata 0))


(define (make-system program)
  ; Virgule core instance.
  (define-values (valid address wstrobe wdata)
    (virgule #:reset (signal #f)
             #:rdata (signal-defer rdata)
             #:ready (signal-defer ready)
             #:irq   (signal #f)))

  ; RAM instance.
  (define-values (ram-ready ram-rdata)
    (single-port-ram (device-size-in-words ram-dev) program
                     #:valid   (device-valid ram-dev valid address)
                     #:address (device-word-address ram-dev address)
                     #:wstrobe wstrobe
                     #:wdata   wdata))

  ; Simple text output device.
  (define-values (txt-ready txt-rdata)
    (text-output #:valid   (device-valid txt-dev valid address)
                 #:address (device-word-address txt-dev address)
                 #:wstrobe wstrobe
                 #:wdata   wdata))

  (define-values (ready rdata)
    (device-ready-rdata valid address
                        (ram-dev ram-ready ram-rdata)
                        (txt-dev txt-ready txt-rdata)))

  ; Return all visible signals as waveforms.
  (waveforms
    (valid      1)
    (ready      1)
    (address   32)
    (rdata     32)
    (wstrobe    4)
    (wdata     32)
    (ram-ready  1)
    (txt-ready  1)))


; Run a system for a given number of clock cycles.
(define (system-run program cycles out)
  (vcd (make-system program) cycles "10 ns" out))
