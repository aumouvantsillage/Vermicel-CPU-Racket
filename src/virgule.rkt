; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "signal.rkt"
  "datapath-components.rkt")

(provide
  virgule)

; An implementation of a minimal RISC-V (RV32I) core.
; The interface is inspired by the PicoRV32
; The sequencer is a state machine.
(define (virgule #:reset reset #:rdata rdata #:ready ready #:irq irq)
  ;
  ; Sequencer.
  ;

  (define state-reg (register/r 'state-fetch reset
                      (for/signal (instr ready [state this-reg])
                        (match state
                          ['state-fetch     (if ready 'state-decode state)]
                          ['state-decode    'state-execute]
                          ['state-execute   (cond [(instruction-load?   instr) 'state-load]
                                                  [(instruction-store?  instr) 'state-store]
                                                  [(instruction-has-rd? instr) 'state-writeback]
                                                  [else                        'state-fetch])]
                          ['state-load      (if ready 'state-writeback state)]
                          ['state-store     (if ready 'state-fetch state)]
                          ['state-writeback 'state-fetch]))))

  (define (state-equal? sym)
    (for/signal (state-reg)
      (equal? state-reg sym)))

  (define fetch-en     (state-equal? 'state-fetch))
  (define decode-en    (state-equal? 'state-decode))
  (define execute-en   (state-equal? 'state-execute))
  (define load-en      (state-equal? 'state-load))
  (define store-en     (state-equal? 'state-store))
  (define writeback-en (state-equal? 'state-writeback))

  ;
  ; Instruction decoding:
  ; decode, read registers, select ALU operands.
  ;

  (define instr     (decoder (signal-defer rdata-reg)))
  (define instr-reg (register/e instr-nop decode-en instr))

  (define-values (xs1 xs2) (register-unit #:reset      reset
                                          #:enable     writeback-en
                                          #:src-instr  instr
                                          #:dest-instr instr-reg
                                          #:xd         (signal-defer xd)))

  (define xs1-reg (register/e 0 decode-en xs1))
  (define xs2-reg (register/e 0 decode-en xs2))

  (define alu-a-reg (register/e 0 decode-en
                      (for/signal (instr xs1 pc-reg)
                        (if (instruction-use-pc? instr)
                          pc-reg
                          xs1))))
  (define alu-b-reg (register/e 0 decode-en
                      (for/signal (instr xs2)
                        (if (instruction-use-imm? instr)
                          (instruction-imm instr)
                          xs2))))

  ;
  ; Instruction execution:
  ; compute ALU and comparator results, compute branch address,
  ; update program counter.
  ;

  (define alu-result     (arith-logic-unit instr-reg alu-a-reg alu-b-reg))
  (define alu-result-reg (register/e 0 execute-en alu-result))

  (define pc+4 (>> add4 pc-reg))
  (define pc-reg (register/re 0 reset execute-en
                   (branch-unit #:reset   reset
                                #:enable  execute-en
                                #:irq     irq
                                #:instr   instr-reg
                                #:xs1     xs1-reg
                                #:xs2     xs2-reg
                                #:address alu-result
                                #:pc+4    pc+4)))
  (define pc+4-reg (register/e 0 execute-en pc+4))

  ;
  ; Memory access:
  ; align data to/from memory, drive control outputs.
  ;

  (define rdata-reg (register/e 0 (>> and valid ready) rdata))
  (define valid     (>> or fetch-en store-en load-en))
  (define address   (>> if fetch-en pc-reg alu-result-reg))

  (define-values (wstrobe wdata load-data)
    (load-store-unit #:instr        instr-reg
                     #:address      alu-result-reg
                     #:store-enable store-en
                     #:store-data   xs2-reg
                     #:rdata        rdata-reg))

  ;
  ; Register update.
  ;

  (define xd (for/signal (instr-reg load-data pc+4-reg alu-result-reg)
               (cond [(instruction-load? instr-reg) load-data]
                     [(instruction-jump? instr-reg) pc+4-reg]
                     [else                          alu-result-reg])))

  (values valid address wstrobe wdata))


(module+ test
  (require rackunit)
  (require "tests/virgule.rkt")

  (define (fake-asm data)
    (if (procedure? data)
      (data #f #f)
      (word data)))

  (define test-count (length test-cases))

  (define lst-rdata    (map fake-asm (map first  test-cases)))
  (define lst-ready                  (map second test-cases))
  (define lst-irq                    (map third  test-cases))
  (define lst-expected (for/list ([c (in-list test-cases)])
                         (drop c 3)))

  (define-values (sig-valid sig-address sig-wstrobe sig-wdata)
    (virgule #:reset (signal #f)
             #:rdata (list->signal lst-rdata)
             #:ready (list->signal lst-ready)
             #:irq   (list->signal lst-irq)))
  (define lst-result (map list
                       (signal-take sig-valid   test-count)
                       (signal-take sig-address test-count)
                       (signal-take sig-wstrobe test-count)
                       (signal-take sig-wdata   test-count)))

  (define labels '(valid address wstrobe wdata))

  (for ([n test-count]
        [r (in-list lst-result)]
        [x (in-list lst-expected)])
    (for ([l  (in-list labels)]
          [rv (in-list r)]
          [xv (in-list x)]
          #:when (not (equal? 'any xv)))
      (test-equal? (format "Virgule #~a: ~a" n l) rv xv))))
