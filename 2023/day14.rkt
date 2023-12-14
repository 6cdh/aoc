#lang racket

(require "lib.rkt")

(define (row-roll-left row)
  (~> xs
      (list->string row)
      (regexp-match* #px"#?[^#]*" xs)
      (map string->list xs)
      (map (sort-in-order "#O.") xs)
      (append* xs)))

(define row-roll-right
  (compose reverse row-roll-left reverse))

(define (roll platform dir)
  (match dir
    ['west (map row-roll-left platform)]
    ['east (map row-roll-right platform)]
    ['north (reverse-2d-list (map row-roll-left (reverse-2d-list platform)))]
    ['south (reverse-2d-list (map row-roll-right (reverse-2d-list platform)))]))

(define (roll-north platform)
  (roll platform 'north))

(define (one-cycle platform)
  (~> platform
      (roll platform 'north)
      (roll platform 'west)
      (roll platform 'south)
      (roll platform 'east)))

;; loop discover
;; try to find loop and calculate result, otherwise fallback to brute force
(define cycles
  (let ([result->cycle-id (make-hash)]
        [cycle-id->result (make-hash)])
    (λ (platform cycle-id)
      (define new-platform (one-cycle platform))
      (cond [(= cycle-id 1) new-platform]
            [(hash-has-key? result->cycle-id new-platform)
             (define loop-start (hash-ref result->cycle-id new-platform))
             (define loop-len (- loop-start cycle-id))
             (define remain (modulo cycle-id loop-len))
             (if (= 0 remain)
                 (hash-ref cycle-id->result (add1 cycle-id))
                 (hash-ref cycle-id->result (- (+ loop-start 1) remain)))]
            [else (hash-set! result->cycle-id new-platform cycle-id)
                  (hash-set! cycle-id->result cycle-id new-platform)
                  (cycles new-platform (sub1 cycle-id))]))))

(define (total-load platform)
  (for/sum ([row (reverse platform)]
            [i (in-naturals 1)])
    (* i (count (λ (c) (char=? c #\O)) row))))

(define (main)
  (define platform (map string->list (read-lines)))
  (println (total-load (roll-north platform)))
  (println (total-load (cycles platform #e1e9))))

(main)
