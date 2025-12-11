#lang racket

(provide solve)

(require "utils.rkt"
         racket/fixnum)

(struct Machine
  (lights buttons joltage)
  #:transparent)

(define (solve in)
  (define machines
    (for/list ([line (in-list (port->lines in))])
      (match-define (list lights-str buttons-str ... joltage-str)
                    (string-split line #px"[\\[\\] ]" #:repeat? #t))
      (define lights
        (map (curry char=? #\#)
             (string->list lights-str)))
      (define buttons
        (for/list ([button (in-list buttons-str)])
          (read-sep-numbers button #px"[,()]")))
      (define joltage (read-sep-numbers joltage-str #px"[,{}]"))
      (Machine lights buttons joltage)))

  (define ans1 (apply + (parallel-map part1 machines)))
  (define ans2 (apply + (map part2 machines)))
  (values ans1 ans2))

(define (part1 m)
  (define buttons (Machine-buttons m))
  (define lights (Machine-lights m))
  (define bn (length buttons))
  (define ln (length lights))
  (define inf (add1 bn))

  (for/min inf ([bitset (in-range (expt 2 bn))])
    (define cur-lights
      (for*/fold ([cur-lights (make-list ln #f)])
                 ([i (in-range bn)]
                  #:when (bitwise-bit-set? bitset i)
                  [b (in-list (list-ref buttons i))])
        (list-update cur-lights b not)))
    (if (equal? cur-lights lights)
        (fxpopcount bitset)
        inf)))

(define (part2 m)
  (define buttons (Machine-buttons m))
  (define joltage (Machine-joltage m))
  (solve-part2 buttons joltage))

(module rosette-solver rosette
  (provide solve-part2)

  (require rosette/solver/smt/z3)

  (current-solver (z3 #:path "/usr/bin/z3"))

  (define (solve-part2 buttons joltage)
    (define bn (length buttons))
    (define jn (length joltage))

    (define (build-result buttons answers)
      (for/fold ([res (make-list jn 0)])
                ([b (in-list buttons)]
                 [x (in-list answers)])
        (for/fold ([res res])
                  ([c (in-list b)])
          (list-update res c (λ (old) (+ old x))))))

    (define answers
      (for/list ([i (in-range bn)])
        (define-symbolic* x integer?)
        x))

    (define model
      (optimize
        #:minimize (list (apply + answers))
        #:guarantee
        (begin
          (for ([x answers])
            (assume (<= 0 x)))
          (for ([r (in-list (build-result buttons answers))]
                [j (in-list joltage)])
            (assume (= r j))))))

    (evaluate (apply + answers) model)))

(require 'rosette-solver)

