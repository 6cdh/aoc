#lang racket

(require "lib.rkt")

(define (next-pos pos n len)
  (let ([Δ (remainder n (sub1 len))])
    (cond [(< (- pos) Δ (- len pos))
           (+ pos Δ)]
          [(<= Δ (- pos))
           (+ pos Δ (sub1 len))]
          [else
           (+ pos Δ (- (sub1 len)))])))

(define (move! i cur-poses arr)
  (let* ([len (vector-length cur-poses)]
         [pos (aref cur-poses i)]
         [val (aref arr i)]
         [new-pos (next-pos pos val len)])
    (when (not (= pos new-pos))
      (cond [(< pos new-pos)
             (for ([(p j) (in-indexed cur-poses)])
               (when ($cmp pos < p <= new-pos)
                 (aupd! cur-poses j sub1)))]
            [(> pos new-pos)
             (for ([(p j) (in-indexed cur-poses)])
               (when ($cmp new-pos <= p < pos)
                 (aupd! cur-poses j add1)))]))
    (aset! cur-poses i new-pos)
    cur-poses))

(define (mix arr k)
  (let* ([len (vector-length arr)]
         [cur-poses (build-vector len values)])
    (for* ([_ k]
           [i len])
      (move! i cur-poses arr))

    (let ([mixed (make-vector len 0)])
      (for ([(index i) (in-indexed cur-poses)])
        (aset! mixed index (aref arr i)))
      mixed)))

(define (next-nth-pos pos n len)
  ($ pos + (modulo n len) modulo len))

(define (build-answer mixed)
  (let ([pos0 (vector-member 0 mixed)])
    (for/sum ([k '(1000 2000 3000)])
      (aref mixed (next-nth-pos pos0 k (vector-length mixed))))))

(define (day20)
  (let ([arr (list->vector (map string->number (read-lines)))])
    (println (build-answer (mix arr 1)))
    (println (build-answer (mix (vector-map (λ (x) (* x 811589153)) arr) 10)))))

(time (day20))
