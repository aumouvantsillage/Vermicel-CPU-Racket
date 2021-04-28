; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../src/assembler.rkt"
  "../src/device.rkt"
  "example-system.rkt")

(define tick
  (asm
    (J 'start)
    ; Interrupt service routine
    (LI t0 (device-start-address tick-dev))
    (LI t1 1)
    (SB t1 t0 4) ; Acknowledge the interrupt request.
    (LW t2 t0)   ; Read the counter value.
    (LI t0 (device-start-address text-dev))
    (SW t2 t0)   ; Display the counter value.
    (MRET)
    ; Main program
    'start
    (LI t0 (device-start-address tick-dev))
    (LI t1 1)
    (SB t1 t0) ; Enable the tick device
    (J  0)))   ; Loop indefinitely

(system-run tick 500
  (open-output-file "virgule-tick.vcd"
                    #:exists 'replace))
