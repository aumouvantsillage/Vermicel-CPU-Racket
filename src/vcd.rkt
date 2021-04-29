; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  "logic.rkt"
  "signal.rkt")

(provide
  waveforms
  (struct-out waveform)
  vcd)

(struct waveform (name size sig))

(define-simple-macro (waveforms (name size) ...)
  (list
    (waveform 'name size name)
    ...))

(define (vcd wavs duration ts [out (current-output-port)])
  ; Generate short names for signals.
  (define short-names (for/list ([n (in-range (length wavs))])
                        (format "s~a" n)))

  ; VCD header.
  (fprintf out "$timescale ~a $end\n" ts)
  (for ([w (in-list wavs)]
        [s (in-list short-names)])
    (fprintf out "$var wire ~a ~a ~a $end\n" (waveform-size w) s (waveform-name w)))
  (fprintf out "$enddefinitions $end\n")

  ; Value changes.
  (for/fold ([sigs (map waveform-sig wavs)]
             [prev (map void wavs)] ; This will force a value change at t=0
             #:result (void))
            ([t (in-range duration)])
    ; Read the current value of all signals.
    (define current (map signal-first sigs))

    ; If at least one signal changed.
    (unless (equal? current prev)
      ; Output a timestamp
      (fprintf out "#~a\n" t)

      ; Output value changes.
      (for ([n (in-list short-names)]
            [s (in-list (map waveform-size wavs))]
            [c (in-list current)]
            [p (in-list prev)]
            #:when (not (equal? c p)))
        (define v (cond [(number? c) (integer->bit-string s c)]
                        [c           1]
                        [else        0]))
        (define fmt (if (= s 1)
                      "~a~a\n"
                      "b~a ~a\n"))
        (fprintf out fmt v n)))

    ; Continue with the rest of the signals.
    (values
      (map signal-rest sigs)
      current))

  ; Last timestamp
  (fprintf out "#~a\n" duration))
