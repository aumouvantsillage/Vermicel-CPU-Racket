#lang racket

(require
  "../assembler.rkt")

(provide (all-defined-out))

(define test-cases
  (list
    (list (LUI  1 2)           10     20         20)
    (list (ADD  1 2 3)         10     20         30)
    (list (ADD  1 2 3)        -10    -20        -30)
    (list (SUB  1 2 3)         10     20        -10)
    (list (SUB  1 2 3)        -10    -20         10)
    (list (SLT  1 2 3)         10     20          1)
    (list (SLT  1 2 3)        -10     20          1)
    (list (SLT  1 2 3)         10    -20          0)
    (list (SLT  1 2 3)         10     10          0)
    (list (SLT  1 2 3)        -10    -10          0)
    (list (SLT  1 2 3)        -10    -20          0)
    (list (SLTU 1 2 3)         10     20          1)
    (list (SLTU 1 2 3)        -10     20          0)
    (list (SLTU 1 2 3)         10    -20          1)
    (list (SLTU 1 2 3)         10     10          0)
    (list (SLTU 1 2 3)        -10    -10          0)
    (list (SLTU 1 2 3)        -10    -20          0)
    (list (XOR  1 2 3)     #b0011 #b0101     #b0110)
    (list (OR   1 2 3)     #b0011 #b0101     #b0111)
    (list (AND  1 2 3)     #b0011 #b0101     #b0001)
    (list (SLL  1 2 3)    #x12345     12 #x12345000)
    (list (SRA  1 2 3)    #x12345     12       #x12)
    (list (SRA  1 2 3) #xF0005432     12 #xFFFF0005)
    (list (SRL  1 2 3)    #x12345     12       #x12)
    (list (SRL  1 2 3) #xF0005432     12 #x000F0005)))
