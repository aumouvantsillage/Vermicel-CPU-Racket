#lang racket

(require
  "../assembler.rkt")

(provide (all-defined-out))

(define test-cases
  (list
    ;     rdata            ready irq valid address    wstrobe wdata          state
    (list 0                #f    #f  #t    #x00000000 #b0000  'any)          ; F
    (list (LUI 4 #xA000)   #t    #f  #t    #x00000000 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x4 = #x0000A000

    (list (ADDI 5 0 #x96)  #t    #f  #t    #x00000004 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x5 = #x00000096

    (list (SW   5 4 #x100) #t    #f  #t    #x00000008 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #f    #f  #t    #x0000A100 #b1111  #x00000096)    ; S
    (list 0                #t    #f  #t    #x0000A100 #b1111  #x00000096)    ; S

    (list (SH   5 4 #x100) #t    #f  #t    #x0000000C #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b0011  #x00960096)    ; S

    (list (SH   5 4 #x102) #t    #f  #t    #x00000010 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A102 #b1100  #x00960096)    ; S

    (list (SB   5 4 #x100) #t    #f  #t    #x00000014 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b0001  #x96969696)    ; S

    (list (SB   5 4 #x101) #t    #f  #t    #x00000018 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A101 #b0010  #x96969696)    ; S

    (list (SB   5 4 #x102) #t    #f  #t    #x0000001C #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A102 #b0100  #x96969696)    ; S

    (list (SB   5 4 #x103) #t    #f  #t    #x00000020 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A103 #b1000  #x96969696)    ; S

    (list (LW   6 4 #x100) #t    #f  #t    #x00000024 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #f    #f  #t    #x0000A100 #b0000  'any)          ; L
    (list #x8C15F3E4       #t    #f  #t    #x0000A100 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x6 = #x8C15F3E4

    (list (SW   6 4 #x100) #t    #f  #t    #x00000028 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #x8C15F3E4)    ; S

    (list (LH   7 4 #x100) #t    #f  #t    #x0000002C #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A100 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x7 = #xFFFFF3E4

    (list (SW   7 4 #x100) #t    #f  #t    #x00000030 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #xFFFFF3E4)    ; S

    (list (LH   8 4 #x102) #t    #f  #t    #x00000034 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A102 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x8 = #xFFFF8C15

    (list (SW   8 4 #x100) #t    #f  #t    #x00000038 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #xFFFF8C15)    ; S

    (list (LHU  9 4 #x100) #t    #f  #t    #x0000003C #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A100 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x9 = #x0000F3E4

    (list (SW   9 4 #x100) #t    #f  #t    #x00000040 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #x0000F3E4)    ; S

    (list (LHU 10 4 #x102) #t    #f  #t    #x00000044 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A102 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x10 = #x00008C15

    (list (SW  10 4 #x100) #t    #f  #t    #x00000048 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #x00008C15)  ; S

    (list (LB  11 4 #x100) #t    #f  #t    #x0000004C #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A100 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x11 = #xFFFFFFE4

    (list (SW  11 4 #x100) #t    #f  #t    #x00000050 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #xFFFFFFE4)    ; S

    (list (LB  12 4 #x101) #t    #f  #t    #x00000054 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A101 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x12 = #xFFFFFFF3

    (list (SW  12 4 #x100) #t    #f  #t    #x00000058 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #xFFFFFFF3)    ; S

    (list (LB  13 4 #x102) #t    #f  #t    #x0000005C #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A102 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x13 = #x00000015

    (list (SW  13 4 #x100) #t    #f  #t    #x00000060 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #x00000015)    ; S

    (list (LB  14 4 #x103) #t    #f  #t    #x00000064 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list #x8C15F3E4       #t    #f  #t    #x0000A103 #b0000  'any)          ; L
    (list 0                #f    #f  #f    'any       'any    'any)          ; R

    ; At this point, x14 = #xFFFFFF8C

    (list (SW  14 4 #x100) #t    #f  #t    #x00000068 #b0000  'any)          ; F
    (list 0                #f    #f  #f    'any       'any    'any)          ; D
    (list 0                #f    #f  #f    'any       'any    'any)          ; E
    (list 0                #t    #f  #t    #x0000A100 #b1111  #xFFFFFF8C)))    ; S
