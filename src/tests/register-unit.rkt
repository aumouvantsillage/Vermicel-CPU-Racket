#lang racket

(require
  "../assembler.rkt")

(provide test-cases)

(define (write-value n)
  (* #x1000 (add1 n)))

(define (read-value n)
  (if (zero? n)
    0
    (write-value n)))

(define (write-test-case rd)
  ;     src-instr       dest-instr   enable xd               xs1              xs2
  (list (ADD 0 rd  rd)  (ADD rd 0 0) #t     (write-value rd) 0                0))

(define (read-test-case rs1 rs2)
  ;     src-instr       dest-instr   enable xd               xs1              xs2
  (list (ADD 0 rs1 rs2) (ADD 0  0 0) #f     0                (read-value rs1) (read-value rs2)))

(define write-test-cases
  (for/list ([n 32])
    (write-test-case n)))

(define read-test-cases
  (apply append
    (for/list ([n 32])
      (list
        (read-test-case n 0)
        (read-test-case 0 n)
        (read-test-case n (- 31 n))))))

(define test-cases
  (append write-test-cases read-test-cases))
