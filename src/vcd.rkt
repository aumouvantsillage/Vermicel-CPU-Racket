; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "logic.rkt"
  "signal.rkt")

(provide
  (struct-out waveform)
  vcd)

(struct waveform (name size sig))

(define (vcd out ts duration . wavs)
  ; Generate short names for signals.
  (define short-names (for/list ([n (in-range (length wavs))])
                        (format "s~a" n)))

  ; VCD header.
  (displayln (format "$timescale ~a $end" ts) out)
  (for ([w (in-list wavs)]
        [s (in-list short-names)])
    (displayln (format "$var wire ~a ~a ~a $end"
                 (waveform-size w) s (waveform-name w)) out))
  (displayln "$enddefinitions $end" out)

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
      (displayln (format "#~a" t) out)

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
                      "~a~a"
                      "b~a ~a"))
        (displayln (format fmt v n) out)))

    ; Continue with the rest of the signals.
    (values
      (map signal-rest sigs)
      current))

  ; Last timestamp
  (displayln (format "#~a" duration) out))
