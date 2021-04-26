; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../assembler.rkt")

(provide (all-defined-out))

(define test-cases
  (list
    (list (BEQ  1 2 3)  10  20 #f)
    (list (BEQ  1 2 3)  10  10 #t)
    (list (BEQ  1 2 3) -10 -20 #f)
    (list (BEQ  1 2 3) -10 -10 #t)
    (list (BNE  1 2 3)  10  20 #t)
    (list (BNE  1 2 3)  10  10 #f)
    (list (BNE  1 2 3) -10 -20 #t)
    (list (BNE  1 2 3) -10 -10 #f)
    (list (BLT  1 2 3)  10  20 #t)
    (list (BLT  1 2 3) -10  20 #t)
    (list (BLT  1 2 3)  10 -20 #f)
    (list (BLT  1 2 3)  10  10 #f)
    (list (BLT  1 2 3) -10 -10 #f)
    (list (BLT  1 2 3) -10 -20 #f)
    (list (BGE  1 2 3)  10  20 #f)
    (list (BGE  1 2 3) -10  20 #f)
    (list (BGE  1 2 3)  10 -20 #t)
    (list (BGE  1 2 3)  10  10 #t)
    (list (BGE  1 2 3) -10 -10 #t)
    (list (BGE  1 2 3) -10 -20 #t)
    (list (BLTU 1 2 3)  10  20 #t)
    (list (BLTU 1 2 3) -10  20 #f)
    (list (BLTU 1 2 3)  10 -20 #t)
    (list (BLTU 1 2 3)  10  10 #f)
    (list (BLTU 1 2 3) -10 -10 #f)
    (list (BLTU 1 2 3) -10 -20 #f)
    (list (BGEU 1 2 3)  10  20 #f)
    (list (BGEU 1 2 3) -10  20 #t)
    (list (BGEU 1 2 3)  10 -20 #f)
    (list (BGEU 1 2 3)  10  10 #t)
    (list (BGEU 1 2 3) -10 -10 #t)
    (list (BGEU 1 2 3) -10 -20 #t)))
