; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "signal.rkt"
  "datapath-components.rkt")

(provide
  virgule-pipelined)

; An implementation of a minimal RISC-V (RV32I) core.
; The interface is inspired by the PicoRV32
; The sequencer is a state machine.
(define (virgule-pipelined #:reset reset #:ro-rdata ro-rdata #:ro-ready ro-ready
                           #:rw-rdata rw-rdata #:rw-ready rw-ready #:irq irq)

  ;
  ; Pipeline control.
  ;

  ; The pipeline is updated at each clock edge when there is no
  ; memory access in a waiting state.
  (define tick (for/signal (ro-valid ro-ready rw-valid rw-ready)
                 (nor (and ro-valid (not ro-ready))
                      (and rw-valid (not rw-ready)))))

  ;
  ; Fetch stage.
  ;

  ; Register the result of a fetch operation in case ro-ready
  ; is asserted before a concurrent load or store.
  (define ro-done (>> and ro-valid ro-ready))
  (define ro-done-reg (register/r #f (>> or reset tick) ro-done))
  (define ro-rdata-reg (register/e 0 ro-done ro-rdata))

  (define fetch-en (>> and-not tick fetch-stall))

  (define fetch-pc+4 (>> add4 fetch-pc-reg))
  (define fetch-pc-reg (register/re 0 reset fetch-en
                         (>> if execute-will-jump
                           execute-pc-next
                           fetch-pc+4)))

  ; Registers for the decode stage.
  (define decode-pc-reg (register/re 0 reset fetch-en fetch-pc-reg))
  (define decode-rdata-reg (register/re opcode-nop reset fetch-en
                             (for/signal (execute-will-jump ro-done ro-rdata ro-rdata-reg)
                               (cond [execute-will-jump opcode-nop]
                                     [ro-done           ro-rdata]
                                     [else              ro-rdata-reg]))))

  ; We can fetch a new instruction when the fetch stage is not stalled
  ; and there is no pending instruction in ro-rdata-reg
  (define ro-valid (>> nor ro-done-reg fetch-stall))
  (define ro-address fetch-pc-reg)

  ;
  ; Decode stage.
  ;

  (define decode-instr (decoder decode-rdata-reg))

  (define-values (decode-xs1 decode-xs2)
    (register-unit #:reset      reset
                   #:enable     (signal-defer writeback-en)
                   #:src-instr  decode-instr
                   #:dest-instr (signal-defer writeback-instr-reg)
                   #:xd         (signal-defer writeback-xd-reg)))

  ; A data dependency is detected when the destination register of a previous
  ; instruction that is still in the pipeline is a source register of the
  ; instruction in the Decode stage.
  ; If the instruction in the Execute stage is a memory load, the Fetch stage
  ; is stalled for one cycle.
  (define fetch-stall (for/signal ([di decode-instr] [xi execute-instr-reg])
                        (and (instruction-has-rd? xi)
                             (instruction-load? xi)
                             (or (= (instruction-rd xi) (instruction-rs1 di))
                                 (= (instruction-rd xi) (instruction-rs2 di))))))

  ; Data forwarding.
  (define-values (decode-xs1-fwd decode-xs2-fwd)
    (for/signal ([di decode-instr] [xi execute-instr-reg] [li load-store-instr-reg] [wi writeback-instr-reg]
                 execute-xd load-store-xd writeback-xd-reg decode-xs1 decode-xs2)
      #:return (xs1-fwd xs2-fwd)
      (define rs1 (instruction-rs1 di))
      (define rs2 (instruction-rs2 di))
      (define xs1-fwd
        (cond [(and (instruction-has-rd? xi) (= rs1 (instruction-rd xi)) (not instruction-load? xi)) execute-xd]
              [(and (instruction-has-rd? li) (= rs1 (instruction-rd li))                             load-store-xd)]
              [(and (instruction-has-rd? wi) (= rs1 (instruction-rd wi))                             writeback-xd-reg)]
              [else                                                                                  decode-xs1]))
      (define xs2-fwd
        (cond [(and (instruction-has-rd? xi) (= rs2 (instruction-rd xi)) (not instruction-load? xi)) execute-xd]
              [(and (instruction-has-rd? li) (= rs2 (instruction-rd li))                             load-store-xd)]
              [(and (instruction-has-rd? wi) (= rs2 (instruction-rd wi))                             writeback-xd-reg)]
              [else                                                                                  decode-xs2]))))

  (define execute-xs1-reg (register/e 0 tick decode-xs1-fwd))
  (define execute-xs2-reg (register/e 0 tick decode-xs2-fwd))

  (define execute-alu-a-reg (register/e 0 tick
                              (for/signal (decode-instr decode-xs1-fwd decode-pc-reg)
                                (if (instruction-use-pc? decode-instr)
                                  decode-pc-reg
                                  decode-xs1-fwd))))
  (define execute-alu-b-reg (register/e 0 tick
                              (for/signal (decode-instr decode-xs2-fwd)
                                (if (instruction-use-imm? decode-instr)
                                  (instruction-imm decode-instr)
                                  decode-xs2-fwd))))

  (define execute-pc+4-reg (register/e 0 tick (>> add4 decode-pc-reg)))

  (define execute-instr-reg (register/re instr-nop reset tick
                              (for/signal (fetch-stall execute-will-jump decode-instr)
                                (if (or fetch-stall execute-will-jump)
                                  instr-nop
                                  decode-instr))))

  ;
  ; Execute stage.
  ;

  (values ro-valid ro-address rw-valid rw-address wstrobe wdata))
