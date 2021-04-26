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
  out-dev
  make-system
  (struct-out system)
  system-run)

; The memory map of this system.
(define ram-dev (device #x00000000 65536))
(define out-dev (device #x80000000     8))


(struct system (valid address ready rdata wstrobe wdata
                ram-ready ram-rdata out-ready))


(define (make-system program)
  ; Virgule core instance.
  (define-values (valid address wstrobe wdata)
    (virgule #:reset (signal #f)
             #:rdata (signal-proxy rdata)
             #:ready (signal-proxy ready)
             #:irq   (signal #f)))

  ; Address decoding.
  (define ram-valid (device-valid ram-dev valid address))
  (define out-valid (device-valid out-dev valid address))

  ; RAM instance.
  (define-values (ram-ready ram-rdata)
    (single-port-ram (device-size-in-words ram-dev) program
                     #:valid   ram-valid
                     #:address (device-word-address ram-dev address)
                     #:wstrobe wstrobe
                     #:wdata   wdata))

  ; Simple text output device.
  (define out-ready (for/signal ([valid out-valid] [address (device-word-address out-dev address)] wstrobe wdata)
                      ; Only accept data written at word-aligned address.
                      (define wdata-slice
                        (match wstrobe
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
                      ; This device is ready immediately.
                      valid))

  ; The read data bus to the processor.
  ; There is currently only one readable device in this system.
  (define rdata ram-rdata)

  ; The bus ready status to the processor.
  (define ready (for/signal (ram-valid ram-ready out-valid out-ready)
                  (or (and ram-valid ram-ready)
                      (and out-valid out-ready))))

  ; Return all visible signals.
  ; This can be used to display waveforms.
  (system valid address ready rdata wstrobe wdata
          ram-ready ram-rdata out-ready))


; Run a system for a given number of clock cycles.
(define (system-run sys cycles vcd-out)
  (vcd vcd-out "10 ns" cycles
    (waveform 'valid      1 (system-valid     sys))
    (waveform 'ready      1 (system-ready     sys))
    (waveform 'address   32 (system-address   sys))
    (waveform 'rdata     32 (system-rdata     sys))
    (waveform 'wstrobe    4 (system-wstrobe   sys))
    (waveform 'wdata     32 (system-wdata     sys))
    (waveform 'ram-ready  1 (system-ram-ready sys))
    (waveform 'out-ready  1 (system-out-ready sys))))
