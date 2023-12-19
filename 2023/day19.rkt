#lang racket

(require "lib.rkt")

(struct Rule
  (matcher next)
  #:transparent)

;; Interval BEGIN

(struct Interval
  (min max))

(define (interval-set>= intv val)
  (struct-copy Interval
               intv
               [min (max (Interval-min intv) val)]))

(define (interval-set<= intv val)
  (struct-copy Interval
               intv
               [max (min (Interval-max intv) val)]))

(define (interval-set> interval val)
  (interval-set>= interval (add1 val)))

(define (interval-set< interval val)
  (interval-set<= interval (sub1 val)))

;; Interval END

;; comparator BEGIN

(define (comparator< old expect [result '()])
  (match result
    ['() (< old expect)]
    [#t (interval-set< old expect)]
    [#f (interval-set>= old expect)]))

(define (comparator> old expect [result '()])
  (match result
    ['() (> old expect)]
    [#t (interval-set> old expect)]
    [#f (interval-set<= old expect)]))

(define (get-comparator str)
  (match str
    [">" comparator>]
    ["<" comparator<]))

;; comparator END

;; read BEGIN

(define (read-rule str)
  (match str
    [(regexp #px"(.)(.*?)([0-9]+):(.*)" (list _ key str val next))
     (Rule (list (get-comparator str)
                 key
                 (string->number val))
           next)]
    [_ error "unexpected condition:" str]))

(define (read-rules rules-str end)
  (append (map read-rule rules-str)
          (list (Rule #t end))))

(define (read-workflows lines)
  (for/hash ([line lines])
    (match (string-split line #px"[{},]")
      [(list name rules ... end)
       (values name (read-rules rules end))])))

(define (read-part line)
  (match (string-split line #px"[{},]")
    [parts-str
     (for/hash ([p parts-str])
       (match (string-split p "=")
         [(list key val) (values key (string->number val))]))]))

(define (read-input)
  (define blocks (read-and-split-in-empty-line))
  (define workflows (read-workflows (read-lines (open-input-string (first blocks)))))
  (define parts (map read-part (read-lines (open-input-string (second blocks)))))
  (list workflows parts))

;; read END

;; part 1 BEGIN

(define (match? matcher part)
  (match matcher
    [#t #t]
    [(list comparator key val)
     (comparator (hash-ref part key) val)]))

(define (accept? workflows part name)
  (for*/first ([rule (hash-ref workflows name)]
               [matcher (in-value (Rule-matcher rule))]
               [next (in-value (Rule-next rule))]
               #:when (match? matcher part))
    (match next
      ["A" #t]
      ["R" #f]
      [next (accept? workflows part next)])))

;; part 1 END

;; part 2 BEGIN

(define (count-combinations sol)
  (for/product ([intv (hash-values sol)])
    (max 0 (add1 (absdiff (Interval-min intv) (Interval-max intv))))))

(define (probe workflows name solution)
  (define (go-next next sol)
    (match next
      ["A" (count-combinations sol)]
      ["R" 0]
      [next (probe-rules (hash-ref workflows next) sol)]))

  (define (probe-rules rules sol)
    (match rules
      [(cons (Rule #t next) _)
       (go-next next sol)]
      [(cons (Rule (list comparator key val) next) rest)
       (+ (go-next next (hash-update sol key (λ (old) (comparator old val #t))))
          (probe-rules rest (hash-update sol key (λ (old) (comparator old val #f)))))]))

  (probe-rules (hash-ref workflows name) solution))

;; part 2 END

(define (main)
  (match-define (list workflows parts) (read-input))

  (println (for/sum ([part parts]
                     #:when (accept? workflows part "in"))
             (sum (hash-values part))))
  (println (probe workflows "in"
                  (hash "x" (Interval 1 4000)
                        "m" (Interval 1 4000)
                        "a" (Interval 1 4000)
                        "s" (Interval 1 4000)))))

(main)
