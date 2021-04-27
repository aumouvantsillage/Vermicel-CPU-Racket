; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  data/pvector
  (only-in data/collection
     nth set-nth)
  "logic.rkt"
  "signal.rkt"
  "opcodes.rkt")

(provide
  word signed-word
  in? signal-and signal-or signal-and-not signal-if
  (struct-out instruction) instr-nop
  decoder arith-logic-unit
  register-unit branch-unit load-store-unit)


(define (word x)
  (unsigned x 32))

(define (signed-word x)
  (signed x 32))

; Architecture properties.
(define reg-count 32)
(define irq-addr   4)

(define-simple-macro (in? v (elt ...))
  (not (not (member v (list elt ...)))))

(define-signal (signal-and . lst)
  (for/and ([v (in-list lst)]) v))

(define-signal (signal-or . lst)
  (for/or ([v (in-list lst)]) v))

(define-signal (signal-and-not a b)
  (and a (not b)))

(define signal-if (signal-lift* if _ _ _))


(struct instruction (rd funct3 rs1 rs2 imm
                     alu-fn use-pc? use-imm? has-rd?
                     load? store? jump? branch? mret?))

(define instr-nop (instruction 0 0 0 0 0 'alu-add #f #f #f #f #f #f #f #f))

(define (decode-alu-fn opcode funct3 funct7)
  (match (list     opcode                             funct3              funct7)
    [(list     (== opcode-lui)                    _                   _                  ) 'alu-nop]
    [(list     (== opcode-op)                     (== funct3-add-sub) (== funct7-sub-sra)) 'alu-sub]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-slt)     _                  ) 'alu-slt]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-sltu)    _                  ) 'alu-sltu]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-xor)     _                  ) 'alu-xor]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-or)      _                  ) 'alu-or]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-and)     _                  ) 'alu-and]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-sll)     _                  ) 'alu-sll]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-srl-sra) (== funct7-sub-sra)) 'alu-sra]
    [(list (or (== opcode-op-imm) (== opcode-op)) (== funct3-srl-sra) _                  ) 'alu-srl]
    [_                                                                                     'alu-add]))


(define-signal (decoder data)
  (define-values (opcode rd funct3 rs1 rs2 funct7 imm) (word->fields data))
  (define use-pc?  (in? opcode (opcode-auipc opcode-jal opcode-branch)))
  (define use-imm? (not (= opcode opcode-op)))
  (define load?    (= opcode opcode-load))
  (define store?   (= opcode opcode-store))
  (define mret?    (and (= opcode opcode-system) (= funct3 funct3-mret) (= imm imm-mret)))
  (define jump?    (in? opcode (opcode-jal opcode-jalr)))
  (define branch?  (= opcode opcode-branch))
  (define has-rd?  (nor branch? store? (zero? rd)))
  (define alu-fn   (decode-alu-fn opcode funct3 funct7))
  (instruction rd funct3 rs1 rs2 imm
               alu-fn use-pc? use-imm? has-rd?
               load? store? jump? branch? mret?))


(define-signal (arith-logic-unit instr a b)
  (define sa (signed-word a))
  (define sb (signed-word b))
  (define sh (unsigned-slice b 5 0))
  (word (match (instruction-alu-fn instr)
          ['alu-nop  b]
          ['alu-add  (+ a b)]
          ['alu-sub  (- a b)]
          ['alu-slt  (if (< sa sb) 1 0)]
          ['alu-sltu (if (< a  b)  1 0)]
          ['alu-xor  (bitwise-xor a b)]
          ['alu-or   (bitwise-ior a b)]
          ['alu-and  (bitwise-and a b)]
          ['alu-sll  (arithmetic-shift a     sh)]
          ['alu-srl  (arithmetic-shift a  (- sh))]
          ['alu-sra  (arithmetic-shift sa (- sh))])))


(define-signal (comparator instr a b)
  (define sa (signed-word a))
  (define sb (signed-word b))
  (match (instruction-funct3 instr)
    [(== funct3-beq)  (=      a  b)]
    [(== funct3-bne)  (not (= a  b))]
    [(== funct3-blt)  (<      sa sb)]
    [(== funct3-bge)  (>=     sa sb)]
    [(== funct3-bltu) (<      a  b)]
    [(== funct3-bgeu) (>=     a  b)]
    [_                #f]))


; Reading and writing happen in different stages.
; src-instr  is used when reading xs1 and xs2
; dest-instr is used when writing xd
; This implementation uses a signal of persistent vectors
(define (register-unit #:reset reset #:enable enable
                       #:src-instr src-instr #:dest-instr dest-instr #:xd xd)
  (define x-reg (register/r (make-pvector reg-count 0) reset
                  ; We don't use a register/re here.
                  ; In register/e and register/re the input signal
                  ; is evaluated even if the result is not used.
                  ; In this specific case, it would create parallel versions of
                  ; the register contents with fake write operations.
                  (for/signal (enable dest-instr xd this-reg)
                    (if (and enable (instruction-has-rd? dest-instr))
                      (set-nth this-reg (instruction-rd dest-instr) xd)
                      this-reg))))
  (for/signal (src-instr x-reg) #:returns (xs1 xs2)
    (define xs1 (nth x-reg (instruction-rs1 src-instr)))
    (define xs2 (nth x-reg (instruction-rs2 src-instr)))))


(define (branch-unit #:reset reset #:enable enable #:irq irq
                     #:instr instr #:xs1 xs1 #:xs2 xs2 #:address address #:pc+4 pc+4)
  (define taken (comparator instr xs1 xs2))
  (define pc-target (for/signal (instr [mepc (signal-defer mepc-reg)] address taken pc+4)
                      (define aligned-address (unsigned-concat [address 31 2] [0 1 0]))
                      (cond [(instruction-mret? instr)               mepc]
                            [(instruction-jump? instr)               aligned-address]
                            [(and (instruction-branch? instr) taken) aligned-address]
                            [else                                    pc+4])))
  (define irq-state-reg (register/re #f reset enable
                          (for/signal (instr irq [state this-reg])
                            (cond [(instruction-mret? instr) #f]
                                  [irq                       #t]
                                  [else                      state]))))
  (define accept-irq (signal-and-not irq irq-state-reg))
  (define mepc-reg (register/re 0 reset (signal-and enable accept-irq)
                     pc-target))
  (signal-if accept-irq
             (signal irq-addr)
             pc-target))


(define-signal (load-store-unit #:instr instr #:address address
                                #:store-enable store-enable #:store-data store-data
                                #:rdata rdata)
  #:returns (wstrobe wdata load-data)
  (define align         (unsigned-slice address 1 0))
  ; Store.
  (define wdata (match (instruction-funct3 instr)
                  [(== funct3-lb-sb) (unsigned-concat [store-data 7 0]  [store-data 7  0] [store-data 7 0] [store-data 7 0])]
                  [(== funct3-lh-sh) (unsigned-concat [store-data 15 0] [store-data 15 0])]
                  [_                 store-data]))
  (define wstrobe (if store-enable
                    (match (instruction-funct3 instr)
                      [(or (== funct3-lb-sb) (== funct3-lbu)) (arithmetic-shift #b0001 align)]
                      [(or (== funct3-lh-sh) (== funct3-lhu)) (arithmetic-shift #b0011 align)]
                      [(== funct3-lw-sw)                      #b1111]
                      [_                                      #b0000])
                    #b0000))
  ; Load.
  (define aligned-rdata (unsigned-slice rdata 31 (* 8 align)))
  (define load-data (word (match (instruction-funct3 instr)
                            [(== funct3-lb-sb) (signed-slice   aligned-rdata  7 0)]
                            [(== funct3-lh-sh) (signed-slice   aligned-rdata 15 0)]
                            [(== funct3-lbu)   (unsigned-slice aligned-rdata  7 0)]
                            [(== funct3-lhu)   (unsigned-slice aligned-rdata 15 0)]
                            [_                                 aligned-rdata]))))


(module+ test
  (require
    rackunit
    (prefix-in dec/ "tests/decoder.rkt")
    (prefix-in alu/ "tests/arith-logic-unit.rkt")
    (prefix-in cmp/ "tests/comparator.rkt")
    (prefix-in reg/ "tests/register-unit.rkt")
    "assembler.rkt")


  (define (fake-asm data)
    (if (procedure? data)
      (data #f #f)
      data))

  (define (test-decoder)
    (define test-count (length dec/test-cases))

    (define lst-data     (map fake-asm (map first dec/test-cases)))
    (define lst-expected               (map rest  dec/test-cases))

    (define sig-instr  (decoder (list->signal lst-data)))
    (define lst-result (signal-take sig-instr test-count))

    (define field-accessors
      (list instruction-rd instruction-funct3 instruction-rs1 instruction-rs2
            instruction-imm instruction-alu-fn
            instruction-use-pc? instruction-use-imm? instruction-has-rd?
            instruction-load? instruction-store?
            instruction-jump? instruction-branch? instruction-mret?))

    (for ([n test-count]
          [r (in-list lst-result)]
          [x (in-list lst-expected)])
      (for ([field-ref (in-list field-accessors)]
            [v         (in-list x)]
            #:when (not (equal? 'any v)))
        (test-equal? (format "Decoder #~a: ~a" n (object-name field-ref))
                     (field-ref r) v))))


  (define (test-arith-logic-unit)
    (define test-count (length alu/test-cases))

    (define lst-data     (map fake-asm (map first  alu/test-cases)))
    (define lst-a        (map word     (map second alu/test-cases)))
    (define lst-b        (map word     (map third  alu/test-cases)))
    (define lst-expected (map word     (map fourth alu/test-cases)))

    (define sig-instr  (decoder (list->signal lst-data)))
    (define lst-instr  (signal-take sig-instr test-count))

    (define sig-result (arith-logic-unit sig-instr
                                         (list->signal lst-a)
                                         (list->signal lst-b)))
    (define lst-result (signal-take sig-result test-count))

    (for ([n test-count]
          [i (in-list lst-instr)]
          [a (in-list lst-a)]
          [b (in-list lst-b)]
          [r (in-list lst-result)]
          [x (in-list lst-expected)])
      (test-equal? (format "ALU #~a: ~a ~a ~a" n (instruction-alu-fn i) a b)
        r x)))


  (define (test-comparator)
    (define test-count (length cmp/test-cases))

    (define lst-data     (map fake-asm (map first  cmp/test-cases)))
    (define lst-a        (map word     (map second cmp/test-cases)))
    (define lst-b        (map word     (map third  cmp/test-cases)))
    (define lst-expected               (map fourth cmp/test-cases))

    (define sig-instr  (decoder (list->signal lst-data)))
    (define lst-instr  (signal-take sig-instr test-count))

    (define sig-result (comparator sig-instr
                                   (list->signal lst-a)
                                   (list->signal lst-b)))
    (define lst-result (signal-take sig-result test-count))

    (for ([n test-count]
          [i (in-list lst-instr)]
          [a (in-list lst-a)]
          [b (in-list lst-b)]
          [r (in-list lst-result)]
          [x (in-list lst-expected)])
      (test-equal? (format "Comparator #~a: ~a ~a ~a" n (instruction-alu-fn i) a b)
        r x)))


  (define (test-register-unit)
    (define test-count (length reg/test-cases))

    (define lst-src-data     (map fake-asm (map first  reg/test-cases)))
    (define lst-dest-data    (map fake-asm (map second reg/test-cases)))
    (define lst-enable                     (map third  reg/test-cases))
    (define lst-xd           (map word     (map fourth reg/test-cases)))
    (define lst-expected-xs1 (map word     (map fifth  reg/test-cases)))
    (define lst-expected-xs2 (map word     (map sixth  reg/test-cases)))

    (define sig-src-instr (decoder (list->signal lst-src-data)))
    (define lst-src-instr (signal-take sig-src-instr test-count))

    (define sig-dest-instr (decoder (list->signal lst-dest-data)))
    (define lst-dest-instr (signal-take sig-dest-instr test-count))

    (define-values (sig-xs1 sig-xs2) (register-unit #:reset      (signal #f)
                                                    #:enable     (list->signal lst-enable)
                                                    #:src-instr  (list->signal lst-src-instr)
                                                    #:dest-instr (list->signal lst-dest-instr)
                                                    #:xd         (list->signal lst-xd)))
    (define lst-result-xs1 (signal-take sig-xs1 test-count))
    (define lst-result-xs2 (signal-take sig-xs2 test-count))

    (for ([n  test-count]
          [si (in-list lst-src-instr)]
          [di (in-list lst-dest-instr)]
          [r1 (in-list lst-result-xs1)]
          [r2 (in-list lst-result-xs2)]
          [x1 (in-list lst-expected-xs1)]
          [x2 (in-list lst-expected-xs2)])
      (test-equal? (format "Register unit #~a: rs1=~a" n (instruction-rs1 si))
        r1 x1)
      (test-equal? (format "Register unit #~a: rs2=~a" n (instruction-rs2 si))
        r2 x2)))


  (define (test-branch-unit)
    void)


  (define (test-load-store-unit)
    void)


  (test-decoder)
  (test-arith-logic-unit)
  (test-comparator)
  (test-register-unit)
  (test-branch-unit)
  (test-load-store-unit))
