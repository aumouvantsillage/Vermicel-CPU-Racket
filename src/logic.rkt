; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  (for-syntax racket/syntax))

(provide
  unsigned unsigned-slice unsigned-concat
  signed signed-slice signed-concat
  integer->bit-string)

; Returns the minimum bit width to store the integer `v`
; as an unsigned logic value.
(define (min-unsigned-width v)
  (cond [(zero?     v) 0]
        [(positive? v) (exact-floor (add1 (log v 2)))]
        [else          (add1 (min-unsigned-width (sub1 (- v))))]))

; Returns the minimum bit width to store the integer `v`
; as an signed logic value.
(define (min-signed-width v)
  (define w (min-unsigned-width v))
  (if (negative? v) w (add1 w)))

; Returns a slice of a value.
; left is the index of the most significant bit to keep in the result.
; right is the index of the least significant bit to keep in the result.
; This function does not sign-extend the result.
(define (unsigned-slice v left [right left])
  (bitwise-bit-field v right (add1 left)))

; Returns a value with the w rightmost bits of v.
; This function does not sign-extend the result.
(define (unsigned v w)
  (unsigned-slice v (sub1 w) 0))

; Returns a sign-extended slice of a value.
; left is the index of the most significant bit to keep in the result.
; right is the index of the least significant bit to keep in the result.
(define (signed-slice v left [right left])
  (define s (unsigned-slice v left right))
  (if (bitwise-bit-set? v left)
    (bitwise-ior s (arithmetic-shift -1 (- left right)))
    s))

; Returns a signed logic value with the given width.
(define (signed v w)
  (signed-slice v (sub1 w) 0))

; Macros to concatenate a sequence of slices.
; Each slice is defined as a list [val left right] or [val left]
; When right is missing, it defaults to left.
(begin-for-syntax
  (define-syntax-class slice-item
    (pattern [val left (~optional right #:defaults ([right #'left]))])))

(define-syntax-parser logic-concat
  [(_ slicer sl ...+ it:slice-item)
   ; Concatenate `sl ...` recursively.
   ; Shift the result left by the width of `it`.
   ; Combine the result with `it` as an unsigned slice.
   #'(bitwise-ior
       (arithmetic-shift (logic-concat slicer sl ...) (add1 (- it.left it.right)))
       (unsigned-slice it.val it.left it.right))]
  [(_ slicer it:slice-item)
   ; Slice the leftmost slice with the given slicing function.
   #'(slicer it.val it.left it.right)])

; Concatenate a sequence of slices.
; Sign-extend the result.
(define-simple-macro (unsigned-concat it ...)
  (logic-concat unsigned-slice it ...))

; Concatenate a sequence of slices.
; Sign-extend the result.
(define-simple-macro (signed-concat it ...)
  (logic-concat signed-slice it ...))

(define (integer->bit-string size v)
  (list->string (for/list ([n (in-range (sub1 size) -1 -1)])
                  (if (bitwise-bit-set? v n) #\1 #\0))))

; ------------------------------------------------------------------------------
; Tests
; ------------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ; Construction
  (define (test-unsigned-width v w)
    (test-equal? (format "Min width of ~au is ~a" v w) (min-unsigned-width v) w))

  (test-unsigned-width 0    0)
  (test-unsigned-width 1    1)
  (test-unsigned-width 2    2)
  (test-unsigned-width 3    2)
  (test-unsigned-width 4    3)
  (test-unsigned-width 128  8)
  (test-unsigned-width 255  8)
  (test-unsigned-width 256  9)
  (test-unsigned-width -1   1)
  (test-unsigned-width -2   2)
  (test-unsigned-width -3   3)
  (test-unsigned-width -4   3)
  (test-unsigned-width -5   4)
  (test-unsigned-width -127 8)
  (test-unsigned-width -128 8)
  (test-unsigned-width -129 9)

  (define (test-signed-width v w)
    (test-equal? (format "Min width of ~as is ~a" v w) (min-signed-width v) w))

  (test-signed-width 0    1)
  (test-signed-width 1    2)
  (test-signed-width 2    3)
  (test-signed-width 3    3)
  (test-signed-width 4    4)
  (test-signed-width 128  9)
  (test-signed-width 255  9)
  (test-signed-width 256  10)
  (test-signed-width -1   1)
  (test-signed-width -2   2)
  (test-signed-width -3   3)
  (test-signed-width -4   3)
  (test-signed-width -5   4)
  (test-signed-width -127 8)
  (test-signed-width -128 8)
  (test-signed-width -129 9)

  (test-equal? "u'#x0A[3..0] = 10" (unsigned       #x0A 4)   10)
  (test-equal? "s'#x0A[3..0] = -6" (signed         #x0A 4)   -6)
  (test-equal? "u'#x05[3..0] =  5" (unsigned       #x05 4)    5)
  (test-equal? "s'#x05[3..0] =  5" (signed         #x05 4)    5)

  (test-equal? "u'#xA0[7..4] = 10" (unsigned-slice #xA0 7 4) 10)
  (test-equal? "s'#xA0[7..4] = -6" (signed-slice   #xA0 7 4) -6)
  (test-equal? "u'#x50[7..4] =  5" (unsigned-slice #x50 7 4)  5)
  (test-equal? "s'#x50[7..4] =  5" (signed-slice   #x50 7 4)  5)

  (check-equal? (unsigned-concat [#xA0 7 5] [#x04 2] [#x05 3 0]) #xB5)
  (check-equal? (unsigned-concat [#x60 7 5] [#x04 2] [#x05 3 0]) #x75)
  (check-equal? (signed-concat   [#xA0 7 5] [#x04 2] [#x05 3 0]) (- #xB5 #x100))
  (check-equal? (signed-concat   [#x60 7 5] [#x04 2] [#x05 3 0]) #x75)
  (check-equal? (signed-concat   [#x60 3]   [#x04 2] [#x05 3 0]) #x15)
  (check-equal? (signed-concat   [#x60 5]   [#x04 2] [#x05 3 0]) (- #x15 #x20)))
