#lang racket

(require "lib.rkt")
(require data/queue)

(define (list->queue lst)
  (for/fold ([q (make-queue)])
            ([v lst])
    (enqueue! q v)
    q))

(struct Monkey (items operation test iftrue iffalse inspection) #:mutable)

(define (make-monkey)
  (Monkey (make-queue) identity 0 0 0 0))

(define (read-ops ops)
  (match ops
    [(list "new" "=" "old" "*" "old") (λ (x) (* x x))]
    [(list "new" "=" "old" "+" "old") (λ (x) (+ x x))]
    [(list "new" "=" "old" "*" val) (λ (x) (* x (string->number val)))]
    [(list "new" "=" "old" "+" val) (λ (x) (+ x (string->number val)))]))

(define (read-monkeys lines)
  (let ([monkeys '()])
    (for ([line lines])
      (let ([ws (string-split line)])
        (match ws
          [(list "Monkey" _) (set! monkeys (cons (make-monkey) monkeys))]
          [(list* "Starting" _ items) (~> items
                                          (map (λ (it) (string->number (string-trim it ","))) %)
                                          (list->queue %)
                                          (set-Monkey-items! (car monkeys) %))]
          [(list* "Operation:" ops) (set-Monkey-operation! (car monkeys)
                                                           (read-ops ops))]
          [(list "Test:" _ ... test) (set-Monkey-test! (car monkeys)
                                                       (string->number test))]
          [(list "If" "true:" _ ... iftrue) (set-Monkey-iftrue! (car monkeys)
                                                                (string->number iftrue))]
          [(list "If" "false:" _ ... iffalse) (set-Monkey-iffalse! (car monkeys)
                                                                   (string->number iffalse))]
          [_ null])))
    (list->vector (reverse monkeys))))

(define (rounds1! monkeys manager)
  (for ([m monkeys])
    (for ([it (queue->list (Monkey-items m))])
      (set-Monkey-inspection! m (add1 (Monkey-inspection m)))
      (let ([new (manager ((Monkey-operation m) it))])
        (if (divisible new (Monkey-test m))
            (enqueue! (Monkey-items (aref monkeys (Monkey-iftrue m))) new)
            (enqueue! (Monkey-items (aref monkeys (Monkey-iffalse m))) new))))
    (set-Monkey-items! m (make-queue))))

(define (rounds! monkeys k manager)
  (for ([_ k])
    (rounds1! monkeys manager))

  (let ([sorted (~> (vector->list monkeys)
                    (map Monkey-inspection %)
                    (sort % >))])
    (* (first sorted)
       (second sorted))))

(define (day11)
  (let* ([lines (read-lines)]
         [monkeys1 (read-monkeys lines)]
         [monkeys2 (read-monkeys lines)])
    (println (rounds! monkeys1 20 (λ (x) (quotient x 3))))
    (let ([lcm-test (apply lcm (map Monkey-test (vector->list monkeys2)))])
      (println (rounds! monkeys2 10000 (λ (x) (modulo x lcm-test)))))))

(time (day11))

