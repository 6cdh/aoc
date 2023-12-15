#lang racket

(require "lib.rkt")

(define (roll-row row order)
  (~> xs
      (list->string row)
      (string-split xs "#" #:trim? #f)
      (map string->list xs)
      (map (λ (lst) (sort lst order)) xs)
      (map list->string xs)
      (string->list (string-join xs "#"))))

(define (roll-row-to-left row)
  ; 'O' > '.'
  (roll-row row char>?))

(define (row-roll-to-right row)
  ; '.' < 'O'
  (roll-row row char<?))

(define (roll platform dir)
  (match dir
    ['west (map roll-row-to-left platform)]
    ['east (map row-roll-to-right platform)]
    ['north (transpose (map roll-row-to-left (transpose platform)))]
    ['south (transpose (map row-roll-to-right (transpose platform)))]))

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
  (let ([platform->cycle-id (make-hash)]
        [cycle-id->platform (make-hash)])
    (λ (platform cycle-id)
      (define new-platform (one-cycle platform))
      (cond [(= cycle-id 1) new-platform]
            [(hash-has-key? platform->cycle-id new-platform)
             (define loop-start (hash-ref platform->cycle-id new-platform))
             (define loop-len (- loop-start cycle-id))
             (define remain (modulo cycle-id loop-len))
             (if (= 0 remain)
                 (hash-ref cycle-id->platform (add1 cycle-id))
                 (hash-ref cycle-id->platform (- (+ loop-start 1) remain)))]
            [else (hash-set! platform->cycle-id new-platform cycle-id)
                  (hash-set! cycle-id->platform cycle-id new-platform)
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
