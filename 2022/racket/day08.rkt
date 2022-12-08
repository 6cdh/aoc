#lang racket

(require "lib.rkt")

(define (day08)
  (let* ([lines (read-lines)]
         [forest (for/vector ([line lines])
                   (for/vector ([c line])
                     (char->integer c)))]
         [m (vector-length forest)]
         [n (vector-length (aref forest 0))])

    (define (fields-of i j)
      (let ([row (vector->list (aref forest i))]
            [col (for/list ([row forest])
                   (aref row j))])
        (list (reverse (take row j)) ; left
              (drop row (add1 j)) ; right
              (reverse (take col i)) ; up
              (drop col (add1 i))))) ; down

    (define (score-of tall d)
      (let ([rem (memf (λ (x) (>= x tall)) d)])
        (if rem
            ($ (length d) - (length rem) + 1)
            (length d))))

    (let ([cnt 0]
          [max-score 0])
      (for* ([i m]
             [j n])
        (let* ([tall (aref forest i j)]
               [fields (fields-of i j)]
               [visible (or (= i 0) (= i m)
                            (= j 0) (= j n)
                            (for/or ([d fields])
                              (> tall (maximum (cons 0 d)))))]
               [score (for/product ([d fields])
                        (score-of tall d))])
          (when visible
            (set! cnt (add1 cnt)))
          (set! max-score (max max-score score))))
      (println cnt)
      (println max-score))))

(time (day08))

