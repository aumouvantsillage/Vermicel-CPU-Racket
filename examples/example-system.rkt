; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../src/logic.rkt"
  "../src/device.rkt"
  "../src/signal.rkt"
  "../src/vermicel.rkt"
  "../src/memory.rkt"
  "../src/vcd.rkt")

(provide
  ram-dev
  text-dev
  tick-dev
  system-run)


; The memory map of this system.
(define ram-dev  (device #x00000000 65536))
(define text-dev (device #x80000000     8))
(define tick-dev (device #x81000000     8))

(define tick-period 50)

(define-signal (text-device #:valid valid #:address address #:wstrobe wstrobe #:wdata wdata)
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


(define (tick-device #:valid valid #:address address #:wstrobe wstrobe #:wdata wdata)
  ; Controller must write 1 at address 0 to enable counting
  ; (or odd value at even address)
  (define enable (register #f
                   (for/signal (valid address wstrobe wdata this-reg)
                     (if (and valid (odd? wstrobe) (even? address))
                       (odd? wdata)
                       this-reg))))
  (define count-max (sub1 tick-period))
  (define count-reg (register/e 0 enable
                      (for/signal (this-reg count-done)
                        (if count-done 0 (add1 this-reg)))))
  (define count-done (for/signal (enable count-reg)
                       (and enable (= count-reg count-max))))
  ; Controller must write 1 at address 1 to acknowledge IRQs
  ; (or odd value at odd address)
  (define irq (register #f
                (for/signal (valid address wstrobe count-done this-reg)
                  (cond [count-done                                #t]
                        [(and valid (odd? wstrobe) (odd? address)) #f]
                        [else                                      this-reg]))))
  ; Reading always returns the current counter value.
  (values valid count-reg irq))


(define (make-system program)
  ; Vermicel core instance.
  (define-values (valid address wstrobe wdata)
    (vermicel #:reset (signal #f)
              #:rdata (signal-defer rdata)
              #:ready (signal-defer ready)
              #:irq   (signal-defer irq)))

  ; RAM instance.
  (define-values (ram-ready ram-rdata)
    (single-port-ram (device-size-in-words ram-dev) program
                     #:valid   (device-valid ram-dev valid address)
                     #:address (device-word-address ram-dev address)
                     #:wstrobe wstrobe
                     #:wdata   wdata))

  ; Simple text output device.
  (define-values (text-ready text-rdata)
    (text-device #:valid   (device-valid text-dev valid address)
                 #:address (device-word-address text-dev address)
                 #:wstrobe wstrobe
                 #:wdata   wdata))

  ; Tick device.
  (define-values (tick-ready tick-rdata irq)
    (tick-device #:valid   (device-valid tick-dev valid address)
                 #:address (device-word-address tick-dev address)
                 #:wstrobe wstrobe
                 #:wdata   wdata))

  (define-values (ready rdata)
    (device-ready-rdata valid address
                        (ram-dev  ram-ready  ram-rdata)
                        (text-dev text-ready text-rdata)
                        (tick-dev tick-ready tick-rdata)))

  ; Return all visible signals as waveforms.
  (waveforms
    (valid      1)
    (ready      1)
    (address   32)
    (rdata     32)
    (wstrobe    4)
    (wdata     32)
    (irq        1)
    (ram-ready  1)
    (text-ready 1)
    (tick-ready 1)))


; Run a system for a given number of clock cycles.
(define (system-run program cycles out)
  (vcd (make-system program) cycles "10 ns" out))
