#lang racket

(require "lib.rkt")

;; read BEGIN

(define (read-input)
  (match (read-and-split-in-empty-line)
    [(list seeds-str converter-strs ...)
     (list (map string->number (cdr (string-split seeds-str " ")))
           (map build-converter converter-strs))]
    [_ (error "invalid input")]))

(define (build-converter str)
  (match (read-lines (open-input-string str))
    [(list _name datas ...)
     (for/list ([data datas])
       (map string->number (string-split data " ")))]
    [_ (error "invalid map input")]))

;; read END

;; range BEGIN

(struct Range
  (start len)
  #:transparent)

(define (Range-end rng)
  (+ (Range-start rng) (Range-len rng)))

(define (range-empty? rng)
  (<= (Range-len rng) 0))

(define (range-intersect rng1 rng2)
  (define start (max (Range-start rng1) (Range-start rng2)))
  (define len (- (min (Range-end rng1) (Range-end rng2)) start))
  (Range start (max len 0)))

;; merge and remove empty ranges
(define (simplify-ranges rngs)
  (define sorted-rngs
    (sort (filter-not range-empty? rngs) < #:key Range-start))
  (for/fold ([result '()])
            ([rng sorted-rngs])
    (match result
      ['() (list rng)]
      [(list last-rng _ ...)
       (if (< (Range-end last-rng) (Range-start rng))
           (cons rng result)
           (cons (Range (Range-start last-rng)
                        (- (max (Range-end last-rng) (Range-end rng))
                           (Range-start last-rng)))
                 (cdr result)))])))

(define (range-subtract bigger smaller)
  (if (range-empty? smaller)
      (list bigger)
      (simplify-ranges
       (list (Range (Range-start bigger) (- (Range-start smaller) (Range-start bigger)))
             (Range (Range-end smaller) (- (Range-end bigger) (Range-end smaller)))))))

;; range END

;; convert a list of ranges through a list of converters
(define (convert-ranges converters rngs)
  (define result '())

  (for ([rng rngs])
    (define rest-rngs (list rng))
    ;; start with a set of ranges,
    ;; for each rule, try to match it, collect the hitted ranges,
    ;; and use the missed ranges for next rule
    (for ([rule converters])
      (match-define (list dst src len) rule)
      (define missed-rngs '())
      (for ([r rest-rngs])
        (define hit (range-intersect r (Range src len)))
        (define miss (range-subtract r hit))
        (list-push-head! result (Range (+ dst (- (Range-start hit) src)) (Range-len hit)))
        (list-append-head! missed-rngs miss))
      (set! rest-rngs missed-rngs))
    (list-append-head! result rest-rngs))
  (simplify-ranges result))

(define (solve converters seed-ranges)
  (minimum
   (map Range-start
        (for/fold ([rngs (simplify-ranges seed-ranges)])
                  ([m converters])
          (convert-ranges m rngs)))))

(define (main)
  (match-define (list seeds converters) (read-input))
  (println (solve converters (map (λ (s) (Range s 1)) seeds)))
  (println (solve converters (map (λ (args) (apply Range args))
                                  (chunks seeds 2)))))

(main)

