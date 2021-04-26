; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../src/assembler.rkt"
  "../src/device.rkt"
  "example-system.rkt")

(define fibonacci
  (asm
    (LI  t0 (device-start-address out-dev))
    (LI  t1 1)     ; The current value of the series
    (LI  t2 1)     ; The next value of the series
    'loop
    (SW  t1 t0 0)  ; Send the current value
    (ADD t3 t2 t1) ; Compute a new value
    (MV  t1 t2)    ; Update the current value
    (MV  t2 t3)    ; Update the next value
    (J   'loop)))  ; Loop indefinitely

(define sys (make-system fibonacci))

(system-run sys 200
  (open-output-file "virgule-fibonacci.vcd"
                    #:exists 'replace))