#lang racket

(require "lib.rkt")

(define (read-input)
  (for/list ([line (read-lines)])
    (match (string-split line #px"[ ,]")
      [(cons springs groups)
       (list springs (map string->number groups))])))

(define (count-ways springs groups)
  (define springs-vec (string->vector springs))
  (define m (vector-length springs-vec))
  (define groups-vec (list->vector groups))
  (define n (vector-length groups-vec))

  (define (skip-to-next-group spring-idx group-idx)
    (cond [(>= spring-idx m)
           (boolean->number (= group-idx n))]
          [else
           (match (aref springs-vec spring-idx)
             [#\. (skip-to-next-group (add1 spring-idx) group-idx)]
             [#\# (match-one-group spring-idx group-idx)]
             [#\? (+ (skip-to-next-group (add1 spring-idx) group-idx)
                     (match-one-group spring-idx group-idx))])]))

  (define (match-one-group spring-idx group-idx)
    (cond [(= group-idx n) 0]
          [else
           (define end-idx (+ spring-idx (aref groups-vec group-idx)))
           (if (and (for/and ([i (in-range spring-idx end-idx)])
                      (and (< i m) (not (char=? #\. (aref springs-vec i)))))
                    (or (= end-idx m)
                        (not (char=? #\# (aref springs-vec end-idx)))))
               (skip-to-next-group (add1 end-idx) (add1 group-idx))
               0)]))

  (cachef-hash! skip-to-next-group)
  (cachef-hash! match-one-group)

  (skip-to-next-group 0 0))

(define (main)
  (define records (read-input))
  (println (for/sum ([rec records])
             (count-ways (first rec) (second rec))))

  (println (for/sum ([rec records])
             (count-ways (string-join (make-list 5 (first rec)) "?")
                         (repeat (second rec) 5)))))

(main)

