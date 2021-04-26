; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse
  syntax/parse/define
  racket/stxparam
  "signal.rkt"
  (for-syntax
    racket/syntax
    racket/sequence))

(provide
  define-component
  this-instance
  define/connect)

(define-syntax-parameter this-instance
  (λ (stx) (raise-syntax-error (syntax-e stx) "can only be used inside define-component")))

(begin-for-syntax
  (define-syntax-class port-decl
    #:datum-literals [input output]
    (pattern [mode:input  name:id])
    (pattern [mode:output name:id])
    (pattern name:id #:attr mode #'input)))

(define-simple-macro (define-component (name port:port-decl ...) body ...)
  ; Create temporary names to hide the default struct constructor.
  #:with (struct-name ctor-name) (generate-temporaries #'(name name))
  ; Collect the getter names for all ports in the struct.
  #:with (port-ref ...) (for/list ([pn (in-syntax #'(port.name ...))])
                          (format-id pn "~a-~a" #'name pn))
  ; Collect the input port names.
  #:with (input-name ...) (for/list ([pm (in-syntax #'(port.mode ...))]
                                     [pn (in-syntax #'(port.name ...))]
                                     #:when (equal? 'input (syntax-e pm)))
                            pn)
  ; Collect the setter names for input ports in the struct.
  #:with (input-field-set ...) (for/list ([pn (in-syntax #'(input-name ...))])
                                 (format-id pn "set-~a-~a!" #'name pn))
  ; Create keywords for input ports.
  #:with (input-kw ...) (for/list ([pn (in-syntax #'(input-name ...))])
                          (string->keyword (symbol->string (syntax-e pn))))
  ; Collect the input port names.
  #:with (output-name ...) (for/list ([pm (in-syntax #'(port.mode ...))]
                                      [pn (in-syntax #'(port.name ...))]
                                      #:when (equal? 'output (syntax-e pm)))
                             pn)
  ; Collect the setter names for output ports in the struct.
  #:with (output-field-set ...) (for/list ([pn (in-syntax #'(output-name ...))])
                                  (format-id pn "set-~a-~a!" #'name pn))
  ; Create a custom setter name for each port.
  #:with (output-port-set ...) (for/list ([pn (in-syntax #'(output-name ...))])
                                 (format-id pn "set-~a!" pn))
  (begin
    ; Create a struct with the same name as the component, and one field per port.
    ; Hide the default constructor name.
    (struct name ([port.name #:auto] ...)
      #:mutable
      #:name struct-name
      #:constructor-name ctor-name)
    ; This function will operate as a constructor for the above struct type.
    (define (name (~@ input-kw [input-name #f]) ...)
      ; Create a struct instance.
      (define self (ctor-name))
      ; Assign signals passed as arguments.
      (when input-name
        (input-field-set self input-name)) ...
      ; Create local setters for output ports.
      (define (output-port-set sig)
        (output-field-set self sig)) ...
      ; Create local proxies for ports that are not associated with signals yet.
      (let ([port.name (or (port-ref self) (signal-proxy (port-ref self)))] ...)
        (syntax-parameterize ([this-instance (make-rename-transformer #'self)])
          body ...))
      self)))


(define-simple-macro (define/connect inst-name comp-name [input-args ...] [(~seq output-kw output-var-name) ...])
  #:with (accessor-name ...) (for/list ([kw (in-syntax #'(output-kw ...))])
                               (define port-name (keyword->string (syntax-e kw)))
                               (format-id kw "~a-~a" #'comp-name port-name))
  (begin
    (define inst-name       (comp-name input-args ...))
    (define output-var-name (accessor-name inst-name)) ...))


(module+ test
  (require rackunit)

  (define-signal (signal-add a b)
    (+ a b))

  (define-component (c a [input b] [output y])
    (set-y! (signal-add a b)))

  (define/connect c-inst c [#:a (signal 5)
                            #:b (signal 20)]
                           [#:y y])

  (check-equal? (signal-take (c-a c-inst) 1) (list 5))
  (check-equal? (signal-take (c-b c-inst) 1) (list 20))
  (check-equal? (signal-take (c-y c-inst) 1) (list 25))
  (check-equal? (signal-take y 1)            (list 25)))
