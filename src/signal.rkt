; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/stxparam
  (for-syntax
    syntax/parse/lib/function-header))

(provide
  signal?
  signal-defer
  signal-cons
  signal-first
  signal-rest
  signal-take
  list->signal
  signal
  signal-lift
  signal-lift*
  signal-λ
  define-signal
  for/signal
  register
  register/r
  register/e
  register/re
  this-reg
  signal-bundle
  signal-unbundle
  signal-bundle-vector
  >>)

(struct signal (body)
  #:name             private-signal
  #:constructor-name private-signal
  #:property         prop:procedure (struct-field-index body))

; Wrap the given body in a promise.
; body ... is supposed to evaluate to a truthy value.
(define-simple-macro (signal-delay body ...)
  (private-signal
    (let ([res #f])                    ; Will store the value of the promise.
      (thunk
        (unless res                    ; If the promise has not been forced,
          (set! res (begin body ...))) ; compute its value and store it.
        res))))                        ; Return the stored value.

; Evaluate a signal.
(define-simple-macro (signal-force sig)
  (sig))                               ; Call the λ created by signal-delay.

; Delay the evaluation of a signal.
; This can be used when we need to reference a signal that is constructed later.
(define-simple-macro (signal-defer sig)
  (thunk (signal-force sig)))

; Construct a signal with a given value, followed by the given signal.
(define-simple-macro (signal-cons val sig)
  (signal-delay (cons val sig)))

; Evaluate the first sample of a signal.
(define (signal-first sig)
  (car (signal-force sig))) ; Returns the left element of the pair.

; Get a signal that starts at the second sample of the given signal.
(define (signal-rest sig)
  (cdr (signal-force sig))) ; Returns the right element of the pair.

; Returns a list of the first n samples of a signal.
(define (signal-take sig n)
  (if (positive? n)
    (cons                                       ; Make a list with:
      (signal-first sig)                        ; the value of the first sample,
      (signal-take (signal-rest sig) (sub1 n))) ; the next n-1 sample values.
    empty))

; Convert a list to a signal.
; The last element of lst is repeated indefinitely.
(define (list->signal lst)
  (define rst (rest lst))
  (define sig (signal-cons              ; Create a signal:
                (first lst)             ; with the first element of the list,
                (if (empty? rst)        ; and if there are no more samples,
                  sig                   ; cycle over the current signal,
                  (list->signal rst)))) ; else, create a signal with the rest of the list.
  sig)

; Create a signal with the given values.
; The last value is repeated indefinitely.
(define-simple-macro (signal val ...)
  (list->signal (list val ...))) ; Pass the arguments as a list to list->signal.


;
; Lifting.
;

; Convert f into a function that operates on signals.
; f must be a function.
; The resulting function takes any number of arguments.
(define (signal-lift f)
  (define (f^ . sig-lst)                      ; The lifted version of f takes any number of arguments.
    (signal-cons                              ; It will return a signal:
      (apply f  (map signal-first sig-lst))   ; with f applied to the first sample of each argument,
      (apply f^ (map signal-rest  sig-lst)))) ; and the lifted f applied to the rest of each argument.
  f^)

; Convert f into a function that operates on signals.
; f is not required to be a function.
; The resulting function takes the specified number of arguments.
(define-simple-macro (signal-lift* f arg ...)
  #:with (tmp ...) (generate-temporaries #'(arg ...)) ; Make a unique name for each argument.
  (letrec ([f^ (λ (tmp ...)                           ; The lifted version of f takes the given number of arguments.
                 (signal-cons                         ; It will return a signal:
                   (f  (signal-first tmp) ...)        ; with f applied to the first sample of each argument,
                   (f^ (signal-rest  tmp) ...)))])    ; and the lifted f applied to the rest of each argument.
    f^))


(begin-for-syntax
  (define-splicing-syntax-class signal-returns-clause
    (pattern (~seq #:returns (ret ...+))
      #:attr ret-len #`#,(length (attribute ret)))))

; Create an anonymous function that operates on signals.
(define-syntax-parser signal-λ
  [(signal-λ args :signal-returns-clause body ...)
   #'(compose1
       (curryr signal-unbundle ret-len)
       (signal-λ args
         body ...
         (list ret ...)))]
  ; Lift a λ with the given list of arguments.
  [(signal-λ (sig:id ...) body ...)
   #'(signal-lift* (λ (sig ...) body ...) sig ...)]
  ; Lift a λ that accepts any number of arguments.
  [(signal-λ sig-lst:id body ...)
   #'(signal-lift (λ sig-lst body ...))]
  ; Lift a λ that accepts keyword arguments.
  ; This form will not work with a rest argument.
  [(signal-λ f:formals body ...)
   #'(λ f (for/signal f.params body ...))])

; Define a signal or a function that operates on signals.
(define-syntax-parser define-signal
  ; Define a variable that contains a signal.
  [(define-signal name:id val ...)
   #'(define name (signal val ...))]
  ; Define a function with the given list of arguments.
  [(define-signal (name:id sig:id ...) body ...)
   #'(define name (signal-λ (sig ...) body ...))]
  ; Define a function that accepts any number of arguments.
  [(define-signal (name:id . sig-lst:id) body ...)
   #'(define name (signal-λ sig-lst body ...))]
  ; Define a function that accepts keyword arguments.
  ; This form will not work with a rest argument.
  [(define-signal h:function-header body ...)
   #'(define h (for/signal h.params body ...))])

; Apply the given body to the elements of one or more signals.
(begin-for-syntax
  (define-syntax-class signal-for-clause
    (pattern [var:id sig])
    (pattern var:id #:attr sig #'var)))

(define-simple-macro (for/signal (c:signal-for-clause ...) body ...)
  ; Create a lifted λ and apply it immediately to the given signals.
  ((signal-λ (c.var ...) body ...) (signal-defer c.sig) ...))


;
; Registers.
;

(define-syntax-parameter this-reg
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside register")))

; Register the given signal.
; The initial value of the result is q0.
; The expression can refer to the result as `this-reg`.
(define-simple-macro (register q0 expr)
  (letrec ([res (signal-cons q0
                  (syntax-parameterize ([this-reg (make-rename-transformer #'res)])
                    expr))])
    res))

; Register with synchronous reset.
; The resulting signal receives q0 each time sig-r is true.
(define-simple-macro (register/r q0 sig-r sig-d)
  (register q0 (for/signal ([r sig-r] [d sig-d])
                 (if r q0 d))))

; Register with enable.
; The resulting signal receives sig-d each time sig-e is true.
(define-simple-macro (register/e q0 sig-e sig-d)
  (register q0 (for/signal ([e sig-e] [d sig-d] [q this-reg])
                 (if e d q))))

; Register with synchronous reset and enable.
(define-simple-macro (register/re q0 sig-r sig-e sig-d)
  (register q0 (for/signal ([r sig-r] [e sig-e] [d sig-d] [q this-reg])
                 (cond [r    q0]
                       [e    d]
                       [else q]))))

; Convert one or more signals into a signal where each element is a list.
(define signal-bundle (signal-lift list))

; For signals with list values. Return signals with the first or the rest
; of each sample.
(define signal-first^ (signal-lift* first _))
(define signal-rest^  (signal-lift* rest  _))

; Convert a signal of lists into a list of signals.
; Return the resulting signals as values.
(define (signal-unbundle sig len)
  (apply values
    (let loop ([s sig] [n len])
      (if (positive? n)
        (cons (signal-first^ s)
              (loop (signal-rest^ s) (sub1 n)))
        empty))))

; Convert a vector of signals into a signal of vectors.
(define (signal-bundle-vector vec)
  (signal-cons
    (vector-map signal-first vec)
    (signal-bundle-vector (vector-map signal-rest vec))))


;
; More syntactic sugar.
;

(define-simple-macro (>> name:id arg ...)
  ((signal-lift* name arg ...) (signal-defer arg) ...))


;
; Tests.
;

(module+ test
  (require rackunit)

  (letrec ([s (signal-cons 42 s)])
    (check-pred signal? s)
    (check-equal? (signal-first s) 42)
    (check-equal? (signal-rest s) s)
    (check-equal? (signal-take s 3) (list 42 42 42)))

  (let ([s (list->signal (list 10 20 30))])
    (check-pred signal? s)
    (check-equal? (signal-first s) 10)
    (check-pred signal? (signal-rest s))
    (check-equal? (signal-first (signal-rest s)) 20)
    (check-equal? (signal-take s 5) (list 10 20 30 30 30)))

  (let ([s (signal 42)])
    (check-equal? (signal-take s 3) (list 42 42 42)))

  (let ([s (signal 10 20 30)])
    (check-equal? (signal-take s 5) (list 10 20 30 30 30)))

  (let ()
    (define-signal s 42)
    (check-equal? (signal-take s 3) (list 42 42 42)))

  (let ()
    (define-signal s 10 20 30)
    (check-equal? (signal-take s 5) (list 10 20 30 30 30)))

  (let* ([signal+ (signal-lift +)]
         [s1 (signal   1   2   3)]
         [s2 (signal  10  20  30)]
         [s3 (signal 100 200 300)]
         [s4 (signal+ s1 s2 s3)])
    (check-equal? (signal-take s4 3) (list 111 222 333)))

  (let* ([signal-if (signal-lift* if _ _ _)]
         [s1 (signal  #f  #t  #f  #t)]
         [s2 (signal  10  20  30  40)]
         [s3 (signal 100 200 300 400)]
         [s4 (signal-if s1 s2 s3)])
    (check-equal? (signal-take s4 4) (list 100 20 300 40)))

  (let* ([signal+ (signal-λ lst (apply + lst))]
         [s1 (signal   1   2   3)]
         [s2 (signal  10  20  30)]
         [s3 (signal 100 200 300)]
         [s4 (signal+ s1 s2 s3)])
    (check-equal? (signal-take s4 3) (list 111 222 333)))

  (let* ([signal-if (signal-λ (c t e) (if c t e))]
         [s1 (signal  #f  #t  #f  #t)]
         [s2 (signal  10  20  30  40)]
         [s3 (signal 100 200 300 400)]
         [s4 (signal-if s1 s2 s3)])
    (check-equal? (signal-take s4 4) (list 100 20 300 40)))

  (let ()
    (define-signal (signal+ . lst)
      (apply + lst))
    (define s1 (signal   1   2   3))
    (define s2 (signal  10  20  30))
    (define s3 (signal 100 200 300))
    (define s4 (signal+ s1 s2 s3))
    (check-equal? (signal-take s4 3) (list 111 222 333)))

  (let ()
    (define-signal (signal-if c t e)
      (if c t e))
    (define s1 (signal  #f  #t  #f  #t))
    (define s2 (signal  10  20  30  40))
    (define s3 (signal 100 200 300 400))
    (define s4 (signal-if s1 s2 s3))
    (check-equal? (signal-take s4 4) (list 100 20 300 40)))

  (let* ([s1 (signal  #f  #t  #f  #t)]
         [s2 (signal  10  20  30  40)]
         [s3 (signal 100 200 300 400)]
         [s4 (for/signal ([c s1] [t s2] [e s3])
               (if c t e))])
    (check-equal? (signal-take s4 4) (list 100 20 300 40)))

  (let* ([s1 (signal  #f  #t  #f  #t)]
         [s2 (signal  10  20  30  40)]
         [s3 (signal 100 200 300 400)]
         [s4 (for/signal (s1 s2 s3)
               (if s1 s2 s3))])
    (check-equal? (signal-take s4 4) (list 100 20 300 40)))

  (let ([s (register 10 (signal 20 30 40))])
    (check-equal? (signal-take s 4) (list 10 20 30 40)))

  (let ([s (register 10 (for/signal (this-reg) (add1 this-reg)))])
    (check-equal? (signal-take s 4) (list 10 11 12 13)))

  (let* ([s1 (signal #f #f #t #f)]
         [s2 (register/r 10 s1 (signal 20 30 40 50 60))])
    (check-equal? (signal-take s2 6) (list 10 20 30 10 50 60)))

  (let* ([s1 (signal #f #t #f #t #f)]
         [s2 (register/e 10 s1 (signal 20 30 40 50 60))])
    (check-equal? (signal-take s2 6) (list 10 10 30 30 50 50)))

  (let* ([s1 (signal #f #f #t #f #f #t #f)]
         [s2 (signal #f #t #f #t #f #t #f)]
         [s3 (register/re 10 s1 s2 (signal 20 30 40 50 60 70))])
    (check-equal? (signal-take s3 7) (list 10 10 30 10 50 50 10)))

  (let* ([s1 (signal   1   2   3)]
         [s2 (signal  10  20  30)]
         [s3 (signal 100 200 300)]
         [s4 (signal-bundle s1 s2 s3)])
    (check-equal? (signal-take s4 3) (list (list 1 10 100) (list 2 20 200) (list 3 30 300))))

  (let*-values ([(s1)       (signal (list 1 10 100) (list 2 20 200) (list 3 30 300))]
                [(s2 s3 s4) (signal-unbundle s1 3)])
    (check-equal? (signal-take s2 3) (list   1   2   3))
    (check-equal? (signal-take s3 3) (list  10  20  30))
    (check-equal? (signal-take s4 3) (list 100 200 300)))

  (let*-values ([(signal-inc3) (signal-λ lst #:returns (x y z)
                                 (define x (add1 (first  lst)))
                                 (define y (add1 (second lst)))
                                 (define z (add1 (third  lst))))]
                [(s1) (signal   1   2   3)]
                [(s2) (signal  10  20  30)]
                [(s3) (signal 100 200 300)]
                [(s4 s5 s6) (signal-inc3 s1 s2 s3)])
    (check-equal? (signal-take s4 3) (list   2   3   4))
    (check-equal? (signal-take s5 3) (list  11  21  31))
    (check-equal? (signal-take s6 3) (list 101 201 301)))

  (let*-values ([(signal-inc3) (signal-λ (a b c) #:returns (x y z)
                                 (define x (add1 a))
                                 (define y (add1 b))
                                 (define z (add1 c)))]
                [(s1) (signal   1   2   3)]
                [(s2) (signal  10  20  30)]
                [(s3) (signal 100 200 300)]
                [(s4 s5 s6) (signal-inc3 s1 s2 s3)])
    (check-equal? (signal-take s4 3) (list   2   3   4))
    (check-equal? (signal-take s5 3) (list  11  21  31))
    (check-equal? (signal-take s6 3) (list 101 201 301)))

  (let ()
    (define-signal (signal-inc3 . lst) #:returns (x y z)
                     (define x (add1 (first  lst)))
                     (define y (add1 (second lst)))
                     (define z (add1 (third  lst))))
    (define s1 (signal   1   2   3))
    (define s2 (signal  10  20  30))
    (define s3 (signal 100 200 300))
    (define-values (s4 s5 s6) (signal-inc3 s1 s2 s3))
    (check-equal? (signal-take s4 3) (list   2   3   4))
    (check-equal? (signal-take s5 3) (list  11  21  31))
    (check-equal? (signal-take s6 3) (list 101 201 301)))

  (let ()
    (define-signal (signal-inc3 a b c) #:returns (x y z)
                     (define x (add1 a))
                     (define y (add1 b))
                     (define z (add1 c)))
    (define s1 (signal   1   2   3))
    (define s2 (signal  10  20  30))
    (define s3 (signal 100 200 300))
    (define-values (s4 s5 s6) (signal-inc3 s1 s2 s3))
    (check-equal? (signal-take s4 3) (list   2   3   4))
    (check-equal? (signal-take s5 3) (list  11  21  31))
    (check-equal? (signal-take s6 3) (list 101 201 301)))

  (let*-values ([(s1) (signal   1   2   3)]
                [(s2) (signal  10  20  30)]
                [(s3) (signal 100 200 300)]
                [(s4 s5 s6) (for/signal (s1 s2 s3) #:returns (x y z)
                              (define x (add1 s1))
                              (define y (add1 s2))
                              (define z (add1 s3)))])
    (check-equal? (signal-take s4 3) (list   2   3   4))
    (check-equal? (signal-take s5 3) (list  11  21  31))
    (check-equal? (signal-take s6 3) (list 101 201 301)))

  (let ()
    (define signal-inc3 (signal-λ (a #:b b [c (signal 100 200 300)] #:d [d (signal 1000 2000 3000)])
                          #:returns (w x y z)
                          (define w (add1 a))
                          (define x (add1 b))
                          (define y (add1 c))
                          (define z (add1 d))))
    (define s1 (signal   1   2   3))
    (define s2 (signal  10  20  30))
    (define-values (s3 s4 s5 s6) (signal-inc3 s1 #:b s2))
    (check-equal? (signal-take s3 3) (list    2    3    4))
    (check-equal? (signal-take s4 3) (list   11   21   31))
    (check-equal? (signal-take s5 3) (list  101  201  301))
    (check-equal? (signal-take s6 3) (list 1001 2001 3001)))

  (let ()
    (define-signal (signal-inc3 a #:b b [c (signal 100 200 300)] #:d [d (signal 1000 2000 3000)])
      #:returns (w x y z)
      (define w (add1 a))
      (define x (add1 b))
      (define y (add1 c))
      (define z (add1 d)))
    (define s1 (signal   1   2   3))
    (define s2 (signal  10  20  30))
    (define-values (s3 s4 s5 s6) (signal-inc3 s1 #:b s2))
    (check-equal? (signal-take s3 3) (list    2    3    4))
    (check-equal? (signal-take s4 3) (list   11   21   31))
    (check-equal? (signal-take s5 3) (list  101  201  301))
    (check-equal? (signal-take s6 3) (list 1001 2001 3001))))
