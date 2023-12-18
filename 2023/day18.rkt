#lang racket

(require "lib.rkt")

(define (read-input)
  (for/list ([line (read-lines)])
    (match (string-split line #px"[ ()]" #:repeat? #t)
      [(list dir steps color)
       (list (string-ref dir 0) (string->number steps) color)])))

(define Pos cons)
(define Pos-x car)
(define Pos-y cdr)

(define Dir cons)
(define Dir-x car)
(define Dir-y cdr)

(define (pos+ pos dir steps)
  (Pos (+ (Pos-x pos) (* steps (Dir-x dir)))
       (+ (Pos-y pos) (* steps (Dir-y dir)))))

(define Left (Dir 0 -1))
(define Right (Dir 0 1))
(define Up (Dir -1 0))
(define Down (Dir 1 0))

(define (build-vertexes plans)
  (for/fold ([vertexes '()]
             [pos (Pos 0 0)]
             #:result (cons (Pos 0 0) vertexes))
            ([plan plans])
    (values (cons pos vertexes)
            (match plan
              [(list #\L steps _)
               (pos+ pos Left steps)]
              [(list #\R steps _)
               (pos+ pos Right steps)]
              [(list #\U steps _)
               (pos+ pos Up steps)]
              [(list #\D steps _)
               (pos+ pos Down steps)]))))

(define (build-new-plans plans)
  (for/list ([plan plans])
    (define hex (third plan))
    (list (match (string-ref hex 6)
            [#\0 #\R]
            [#\1 #\D]
            [#\2 #\L]
            [#\3 #\U])
          (string->number (substring hex 1 6) 16)
          hex)))

(define (shoelace-area points)
  (abs (/ (for/sum ([p1 points]
                    [p2 (rest points)])
            (- (* (Pos-x p1) (Pos-y p2))
               (* (Pos-y p1) (Pos-x p2))))
          2)))

(define (count-area points)
  ;; https://en.wikipedia.org/wiki/Pick%27s_theorem
  (define interiors (shoelace-area points))

  (define boundaries
    (+ 4
       (for/sum ([p1 points]
                 [p2 (rest points)])
         (+ (absdiff (Pos-x p1) (Pos-x p2))
            (absdiff (Pos-y p1) (Pos-y p2))))))

  (+ interiors
     (/ boundaries 2)
     -1))

(define (main)
  (define plans (read-input))
  (println (count-area (build-vertexes plans)))

  (define new-plans (build-new-plans plans))
  (println (count-area (build-vertexes new-plans))))

(main)
