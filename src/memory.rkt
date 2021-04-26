; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  data/pvector
  (only-in data/collection
     nth set-nth)
  "logic.rkt"
  "signal.rkt")

(provide (all-defined-out))

; Create a single-port RAM component.
; size is the RAM capacity, in words,
; content is the initial RAM content, as a list of words,
; address is in words.
; valid implies address < size
(define (single-port-ram size [content empty] #:valid valid #:address address #:wstrobe wstrobe #:wdata wdata)
  (define initial-content (for/pvector ([n (in-range size)])
                            (if (< n (length content))
                              (nth content n)
                              0)))
  (define mem (register initial-content
                (for/signal (valid address rdata wstrobe wdata this-reg)
                  ; We don't use a register/e here.
                  ; In register/e and register/re the input signal
                  ; is evaluated even if the result is not used.
                  ; In this specific case, it would create parallel versions of
                  ; the register contents with fake write operations.
                  (if (and valid (not (zero? wstrobe)))
                    (set-nth this-reg address
                      (unsigned-concat
                        [(if (bitwise-bit-set? wstrobe 3) wdata rdata) 31 24]
                        [(if (bitwise-bit-set? wstrobe 2) wdata rdata) 23 16]
                        [(if (bitwise-bit-set? wstrobe 1) wdata rdata) 15  8]
                        [(if (bitwise-bit-set? wstrobe 0) wdata rdata)  7  0]))
                    this-reg))))
  (define rdata (for/signal (valid address mem)
                  (if valid
                    (nth mem address)
                    0)))
  ; TODO Allow to customize the latency of read/write operations.
  ; At the moment, ready = valid.
  (values valid rdata))
