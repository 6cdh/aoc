#lang racket

(require "lib.rkt")

(define Point cons)
(define Point-x car)
(define Point-y cdr)

(define (find-all-poses vecvec c)
  (for*/list ([(row x) (in-indexed vecvec)]
              [(val y) (in-indexed row)]
              #:when (char=? c val))
    (Point x y)))

(define (roll-dir platform dx dy)
  (define platform-vec (list2d->vector2d platform))
  (define m (vector-length platform-vec))
  (define n (vector-length (aref platform-vec 0)))

  (define (platform-get pos)
    (aref platform-vec (Point-x pos) (Point-y pos)))

  (define (platform-set! pos val)
    (aset! platform-vec (Point-x pos) (Point-y pos) val))

  (define (valid? pos)
    (and (< -1 (Point-x pos) m) (< -1 (Point-y pos) n)))

  (define (move pos)
    (define new-pos (Point (+ dx (Point-x pos)) (+ dy (Point-y pos))))
    (if (not (valid? new-pos))
        pos
        (match (platform-get new-pos)
          [#\# pos]
          [#\. (move new-pos)]
          [_
           (if (equal? (move-round-rock! new-pos) new-pos)
               pos
               (move new-pos))])))

  (define (move-round-rock! pos)
    (when (char=? #\O (platform-get pos))
      (platform-set! pos #\.)
      (define final-pos (move pos))
      (platform-set! final-pos #\O)
      final-pos))

  (for-each move-round-rock! (find-all-poses platform-vec #\O))
  (vector2d->list2d platform-vec))

(define (roll-north platform)
  (roll-dir platform -1 0))

(define (cycle1 platform)
  (~> platform
      (roll-dir platform -1 0)
      (roll-dir platform 0 -1)
      (roll-dir platform 1 0)
      (roll-dir platform 0 1)))

;; loop discover
;; try to find loop and calculate result, otherwise fallback to brute force
(define cycles
  (let ([result->cycle-id (make-hash)]
        [cycle-id->result (make-hash)])
    (λ (platform cycle-id)
      (define new-platform (cycle1 platform))
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
