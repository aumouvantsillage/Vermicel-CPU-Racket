; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../src/assembler.rkt"
  "../src/interconnect.rkt"
  "example-system.rkt")

(define hello
  (asm
    (LI    t0 (device-start-address out-dev))
    (LA    t1 'str)  ; The address of the string
    'loop
    (LBU   t2 t1 0)  ; Read a character from the string
    (BEQZ  t2 0)     ; If zero, loop indefinitely
    (SB    t2 t0 4)  ; Send the current character
    (ADDI  t1 t1 1)  ; Move to the next location in the string
    (J     'loop)    ; Loop
    'str
    (bytes->words #:asciiz #t
      #"Virgule says\n<< Hello! >>\n")))

(define sys (make-system hello))

(system-run sys 500
  (open-output-file "virgule-hello.vcd"
                    #:exists 'replace))
