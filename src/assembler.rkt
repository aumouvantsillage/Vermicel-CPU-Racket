; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  (for-syntax
    racket/syntax
    racket/sequence
    racket/list)
  "logic.rkt"
  "opcodes.rkt")

(provide
  (except-out (all-defined-out)
    make-second-pass
    fields->word*
    define-registers
    split-imm
    pc-relative))


; Assemble a program provided as a sequence of:
; * integers, for fully encoded instructions,
; * functions, for instructions that need a second pass,
; * symbols that represent adresses.
(define (asm . lst)
  ; Some pseudo-instructions generate lists, so we need to flatten lst.
  (define lst-flat (flatten lst))
  ; Map symbols to addresses.
  (define symbol-table (for/fold ([acc (make-immutable-hash)]
                                  [addr 0]
                                  #:result acc)
                                 ([it (in-list lst-flat)])
                         (if (symbol? it)
                           (values (hash-set acc it addr) addr)
                           (values acc (+ 4 addr)))))
  ; Remove symbols from the program.
  (define instrs (filter-not symbol? lst-flat))
  ; Second pass: execute each function,
  ; passing its address and the symbol table as arguments.
  (for/list ([(it n) (in-indexed instrs)])
    (if (procedure? it)
      (it (* 4 n) symbol-table)
      it)))


; Prepare an instruction encoder for the second pass.
; The resulting function will execute fn with the actual immediate value.
; If imm was a symbol, the corresponding address is looked up in the symbol table
; and an offset with respect to the current instruction address is computed.
(define (make-second-pass imm fn)
  (λ (pc symbol-table)
     (fn (if (symbol? imm)
           (- (hash-ref symbol-table imm) pc)
           imm))))


;
; Base instruction set.
;
; This section provides an encoding function for each instruction.
; Each encoding function delegates to fields->word with the appropriate field values.
;

; A variant of fields->word* for instructions that support a second pass.
; This function is used in jump and branch instruction encoders, where the
; immediate offset can be represented by a symbol.
(define (fields->word* opcode #:rd [rd 0] #:funct3 [fn3 0] #:rs1 [rs1 0] #:rs2 [rs2 0] #:funct7 [fn7 0] #:imm [imm 0])
  (make-second-pass imm
    (λ (imm)
      (fields->word opcode #:rd rd #:funct3 fn3 #:rs1 rs1 #:rs2 rs2 #:funct7 fn7 #:imm imm))))

(define (LUI   rd  imm)         (fields->word  opcode-lui                            #:rd rd                     #:imm imm))
(define (AUIPC rd  imm)         (fields->word  opcode-auipc                          #:rd rd                     #:imm imm))
(define (JAL   rd  offset)      (fields->word* opcode-jal                            #:rd rd                     #:imm offset))
(define (JALR  rd  rs1 offset)  (fields->word* opcode-jalr   #:funct3 funct3-jalr    #:rd rd #:rs1 rs1           #:imm offset))
(define (BEQ   rs1 rs2 offset)  (fields->word* opcode-branch #:funct3 funct3-beq             #:rs1 rs1 #:rs2 rs2 #:imm offset))
(define (BNE   rs1 rs2 offset)  (fields->word* opcode-branch #:funct3 funct3-bne             #:rs1 rs1 #:rs2 rs2 #:imm offset))
(define (BLT   rs1 rs2 offset)  (fields->word* opcode-branch #:funct3 funct3-blt             #:rs1 rs1 #:rs2 rs2 #:imm offset))
(define (BGE   rs1 rs2 offset)  (fields->word* opcode-branch #:funct3 funct3-bge             #:rs1 rs1 #:rs2 rs2 #:imm offset))
(define (BLTU  rs1 rs2 offset)  (fields->word* opcode-branch #:funct3 funct3-bltu            #:rs1 rs1 #:rs2 rs2 #:imm offset))
(define (BGEU  rs1 rs2 offset)  (fields->word* opcode-branch #:funct3 funct3-bgeu            #:rs1 rs1 #:rs2 rs2 #:imm offset))
(define (LB    rd  rs1 [imm 0]) (fields->word  opcode-load   #:funct3 funct3-lb-sb   #:rd rd #:rs1 rs1           #:imm imm))
(define (LH    rd  rs1 [imm 0]) (fields->word  opcode-load   #:funct3 funct3-lh-sh   #:rd rd #:rs1 rs1           #:imm imm))
(define (LW    rd  rs1 [imm 0]) (fields->word  opcode-load   #:funct3 funct3-lw-sw   #:rd rd #:rs1 rs1           #:imm imm))
(define (LBU   rd  rs1 [imm 0]) (fields->word  opcode-load   #:funct3 funct3-lbu     #:rd rd #:rs1 rs1           #:imm imm))
(define (LHU   rd  rs1 [imm 0]) (fields->word  opcode-load   #:funct3 funct3-lhu     #:rd rd #:rs1 rs1           #:imm imm))
(define (SB    rs2 rs1 [imm 0]) (fields->word  opcode-store  #:funct3 funct3-lb-sb           #:rs1 rs1 #:rs2 rs2 #:imm imm))
(define (SH    rs2 rs1 [imm 0]) (fields->word  opcode-store  #:funct3 funct3-lh-sh           #:rs1 rs1 #:rs2 rs2 #:imm imm))
(define (SW    rs2 rs1 [imm 0]) (fields->word  opcode-store  #:funct3 funct3-lw-sw           #:rs1 rs1 #:rs2 rs2 #:imm imm))
(define (ADDI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-add-sub #:rd rd #:rs1 rs1           #:imm imm))
(define (SLLI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-sll     #:rd rd #:rs1 rs1           #:imm imm #:funct7 funct7-default))
(define (SLTI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-slt     #:rd rd #:rs1 rs1           #:imm imm))
(define (SLTIU rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-sltu    #:rd rd #:rs1 rs1           #:imm imm))
(define (XORI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-xor     #:rd rd #:rs1 rs1           #:imm imm))
(define (SRLI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-srl-sra #:rd rd #:rs1 rs1           #:imm imm #:funct7 funct7-default))
(define (SRAI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-srl-sra #:rd rd #:rs1 rs1           #:imm imm #:funct7 funct7-sub-sra))
(define (ORI   rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-or      #:rd rd #:rs1 rs1           #:imm imm))
(define (ANDI  rd rs1 imm)      (fields->word  opcode-op-imm #:funct3 funct3-and     #:rd rd #:rs1 rs1           #:imm imm))
(define (ADD   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-add-sub #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (SUB   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-add-sub #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-sub-sra))
(define (SLL   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-sll     #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (SLT   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-slt     #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (SLTU  rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-sltu    #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (XOR   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-xor     #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (SRL   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-srl-sra #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (SRA   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-srl-sra #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-sub-sra))
(define (OR    rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-or      #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (AND   rd rs1 rs2)      (fields->word  opcode-op     #:funct3 funct3-and     #:rd rd #:rs1 rs1 #:rs2 rs2           #:funct7 funct7-default))
(define (MRET)                  (fields->word  opcode-system #:funct3 funct3-mret                                #:imm imm-mret))

;
; Registers.
;
; In a program, each register is represented by its index.
; For convenience, we associate each register index with variables following
; the naming conventions of RISC-V registers.
;

; This macro generates variable definitions for register names.
; Each register is associated with two variables: one with a generic name xn
; and the other with an ABI name.
; There must be one ABI name for each register, and two ABI names cannot be
; associated with the same register index.
(define-simple-macro (define-registers abi-name ...)
  #:with (index  ...) (range (length (attribute abi-name)))
  #:with (x-name ...) (for/list ([n (in-syntax #'(index ...))])
                        (format-symbol "x~a" (syntax->datum n)))
  (begin
    (define abi-name (quote index)) ...
    (define x-name   (quote index)) ...))

(define-registers zero ra sp gp tp
                  t0 t1 t2
                  s0 s1
                  a0 a1 a2 a3 a4 a5 a6 a7
                  s2 s3 s4 s5 s6 s7 s8 s9 s10 s11
                  t3 t4 t5 t6)

; Register 8 has two ABI names
(define fp 8)

;
; Pseudo-instructions
;
; Each pseudo-instruction is translated into one or two instructions.
; The encoding function for a pseudo-instruction delegates to one or two
; instruction encoding functions above.
;

; Split an immediate value into an upper part (bits 31 to 12)
; and a lower part (bits 11 to 0).
; This function is used in pseudo-instructions that translate to a U-type
; instruction (LUI, AUIPC) followed by an I/S/B-type instruction.
(define (split-imm imm)
  (define i (signed imm 32))
  (define u (signed-concat [imm 31 12] [0 11 0]))
  (define l (signed-slice imm 11 0))
  (values i (if (negative? l) (+ #x1000 u) u) l))


(define (LI rd imm)
  (define-values (i u l) (split-imm imm))
  (cond [(= l i) (ADDI rd zero l)]
        [(= u i) (LUI  rd u)]
        [else    (list (LUI rd u) (ADDI rd rd l))]))

(define (NOP)        (ADDI zero zero 0))
(define (MV rd rs)   (ADDI rd   rs   0))
(define (NOT rd rs1) (XORI rd  rs1   -1))
(define (NEG rd rs2) (SUB  rd  zero  rs2))

(define (BEQZ rs1 offset)     (BEQ  rs1  zero offset))
(define (BNEZ rs1 offset)     (BNE  rs1  zero offset))
(define (BLTZ rs1 offset)     (BLT  rs1  zero offset))
(define (BGTZ rs2 offset)     (BLT  zero rs2  offset))
(define (BGEZ rs1 offset)     (BGE  rs1  zero offset))
(define (BLEZ rs2 offset)     (BGE  zero rs2  offset))
(define (BGT  rs2 rs1 offset) (BLT  rs1  rs2  offset))
(define (BLE  rs2 rs1 offset) (BGE  rs1  rs2  offset))
(define (BGTU rs2 rs1 offset) (BLTU rs1  rs2  offset))
(define (BLEU rs2 rs1 offset) (BGEU rs1  rs2  offset))

(define (SEQZ rd rs1) (SLTIU rd rs1  1))
(define (SNEZ rd rs2) (SLTU  rd zero rs2))
(define (SLTZ rd rs1) (SLT   rd rs1  zero))
(define (SGTZ rd rs2) (SLT   rd zero rs2))

(define (J    offset) (JAL  zero offset))
(define (JAL* offset) (JAL  ra   offset))
(define (JR   rs1)    (JALR zero rs1 0))
(define (RET)         (JR   ra))

; Prepare a pseudo-instruction for the second pass.
; Generate an AUIPC followed by another instruction encoded by fn.
; r1 is the index of the register that stores the result of AUIPC,
; r2 is another register used in the second instruction.
(define (pc-relative fn r1 r2 offset)
  (list
    (make-second-pass offset
      (λ (of)
        (match-define-values (_ u _) (split-imm of))
        (AUIPC r1 u)))
    (make-second-pass offset
      (λ (of)
        (match-define-values (_ _ l) (split-imm (+ of 4)))
        (fn r2 r1 l)))))

(define (LA   rd  offset)     (pc-relative ADDI rd  rd   offset))
(define (LB*  rd  offset)     (pc-relative LB   rd  rd   offset))
(define (LH*  rd  offset)     (pc-relative LH   rd  rd   offset))
(define (LW*  rd  offset)     (pc-relative LW   rd  rd   offset))
(define (LBU* rd  offset)     (pc-relative LBU  rd  rd   offset))
(define (LHU* rd  offset)     (pc-relative LHU  rd  rd   offset))
(define (SB*  rs2 offset rs1) (pc-relative SB   rs1 rs2  offset))
(define (SH*  rs2 offset rs1) (pc-relative SH   rs1 rs2  offset))
(define (SW*  rs2 offset rs1) (pc-relative SW   rs1 rs2  offset))
(define (CALL     offset)     (pc-relative JALR t1  ra   offset))
(define (TAIL     offset)     (pc-relative JALR t1  zero offset))

;
; Data encoding.
;

; Convert a bytes string into a list of words.
; If asciiz is true, the generated data is guaranteed to contain a null byte
; after the last non-null byte of str.
(define (bytes->words str #:asciiz [asciiz #f])
  (define lst (bytes->list str))

  ; Pad with zeros to get an whole number of words.
  ; Make sure there is a zero byte at the end of a C string.
  (define tail-len (modulo (length lst) 4))

  (define pad-len
    (cond
      ; Align to word length: will append 1 to 3 zero bytes
      [(positive? tail-len) (- 4 tail-len)]
      ; Already aligned and zero-terminated: no padding needed
      [(and (not (empty? lst)) (zero? (last lst))) 0]
      ; Already aligned but not zero-terminated C string: append 4 zero bytes
      [asciiz 4]
      ; Aligned raw bytes: no padding needed.
      [else 0]))

  (define padded-lst (append lst (make-list pad-len 0)))

  (for/list ([h (in-slice 4 padded-lst)])
    (unsigned-concat [(fourth h) 7 0]
                     [(third  h) 7 0]
                     [(second h) 7 0]
                     [(first  h) 7 0])))


(module+ test
  (require rackunit)

  (check-equal? (bytes->words #"")         empty)
  (check-equal? (bytes->words #"A")        (list #x00000041))
  (check-equal? (bytes->words #"AB")       (list #x00004241))
  (check-equal? (bytes->words #"ABC")      (list #x00434241))
  (check-equal? (bytes->words #"ABCD")     (list #x44434241))
  (check-equal? (bytes->words #"ABCDE")    (list #x44434241 #x00000045))
  (check-equal? (bytes->words #"ABCDEFGH") (list #x44434241 #x48474645))

  (check-equal? (bytes->words #""         #:asciiz #t) (list #x00000000))
  (check-equal? (bytes->words #"A"        #:asciiz #t) (list #x00000041))
  (check-equal? (bytes->words #"AB"       #:asciiz #t) (list #x00004241))
  (check-equal? (bytes->words #"ABC"      #:asciiz #t) (list #x00434241))
  (check-equal? (bytes->words #"ABCD"     #:asciiz #t) (list #x44434241 #x00000000))
  (check-equal? (bytes->words #"ABCDE"    #:asciiz #t) (list #x44434241 #x00000045))
  (check-equal? (bytes->words #"ABCDEFG"  #:asciiz #t) (list #x44434241 #x00474645))
  (check-equal? (bytes->words #"ABCDEFGH" #:asciiz #t) (list #x44434241 #x48474645 #x00000000)))
