; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require "logic.rkt")

(provide (all-defined-out))

; Base opcodes.
(define opcode-load   #b0000011)
(define opcode-op-imm #b0010011)
(define opcode-auipc  #b0010111)
(define opcode-store  #b0100011)
(define opcode-op     #b0110011)
(define opcode-lui    #b0110111)
(define opcode-branch #b1100011)
(define opcode-jalr   #b1100111)
(define opcode-jal    #b1101111)
(define opcode-system #b1110011)

; funct3 opcodes.
(define funct3-jalr    #b000)
(define funct3-beq     #b000)
(define funct3-bne     #b001)
(define funct3-blt     #b100)
(define funct3-bge     #b101)
(define funct3-bltu    #b110)
(define funct3-bgeu    #b111)
(define funct3-lb-sb   #b000)
(define funct3-lh-sh   #b001)
(define funct3-lw-sw   #b010)
(define funct3-lbu     #b100)
(define funct3-lhu     #b101)
(define funct3-add-sub #b000)
(define funct3-slt     #b010)
(define funct3-sltu    #b011)
(define funct3-xor     #b100)
(define funct3-or      #b110)
(define funct3-and     #b111)
(define funct3-sll     #b001)
(define funct3-srl-sra #b101)
(define funct3-mret    #b000)

; funct7 opcodes.
(define funct7-default #b0000000)
(define funct7-sub-sra #b0100000)

; Immediate-encoded opcodes.
(define imm-mret #b001100000010)

(define (immediate-format opcode)
  (match opcode
    [(== opcode-op)                         'fmt-r]
    [(== opcode-store)                      'fmt-s]
    [(== opcode-branch)                     'fmt-b]
    [(or (== opcode-lui) (== opcode-auipc)) 'fmt-u]
    [(== opcode-jal)                        'fmt-j]
    [_                                      'fmt-i]))

(define (word->fields w)
  (define opcode (unsigned-slice w 6 0))
  (define imm (match (immediate-format opcode)
                ['fmt-i (signed-concat [w 31 20])]
                ['fmt-s (signed-concat [w 31 25] [w 11 7])]
                ['fmt-b (signed-concat [w 31] [w 7] [w 30 25] [w 11 8] [0 0])]
                ['fmt-u (signed-concat [w 31 12] [0 11 0])]
                ['fmt-j (signed-concat [w 31] [w 19 12] [w 20] [w 30 21] [0 0])]
                [_      0]))
  (values
    opcode
    (unsigned-slice w 11  7) ; rd
    (unsigned-slice w 14 12) ; funct3
    (unsigned-slice w 19 15) ; rs1
    (unsigned-slice w 24 20) ; rs2
    (unsigned-slice w 31 25) ; funct7
    imm))

(define (fields->word opcode #:rd [rd 0] #:funct3 [fn3 0] #:rs1 [rs1 0] #:rs2 [rs2 0] #:funct7 [fn7 0] #:imm [imm 0])
  ; Immediate shift instructions have a funct7 field and a shorter imm field.
  (define imm* (if (and (= opcode opcode-op-imm)
                        (or (= fn3 funct3-sll)
                            (= fn3 funct3-srl-sra)))
                 (unsigned-concat [fn7 6 0] [imm 4 0])
                 imm))

  (match (immediate-format opcode)
    ;                                31       30 25      24 21       20       19 15      14 12      11 8        7          6 0
    ['fmt-i (unsigned-concat  [imm* 11                               0] [rs1  4  0] [fn3 2  0] [rd  4          0] [opcode 6 0])]
    ['fmt-s (unsigned-concat  [imm  11           5] [rs2 4           0] [rs1  4  0] [fn3 2  0] [imm 4          0] [opcode 6 0])]
    ['fmt-b (unsigned-concat  [imm  12] [imm 10  5] [rs2 4           0] [rs1  4  0] [fn3 2  0] [imm 4 1] [imm 11] [opcode 6 0])]
    ['fmt-u (unsigned-concat  [imm  31                                                     12] [rd  4          0] [opcode 6 0])]
    ['fmt-j (unsigned-concat  [imm  20] [imm 10             1] [imm 11] [imm 19            12] [rd  4          0] [opcode 6 0])]
    [_      (unsigned-concat  [fn7   6           0] [rs2 4           0] [rs1 4   0] [fn3 2  0] [rd  4          0] [opcode 6 0])]))
