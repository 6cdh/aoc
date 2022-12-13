#lang racket

(require "lib.rkt")

;; parser BEGIN

;; val = int | list;
;; int = ('0'..'9')+;
;; list = "[", (val, ",")*, [val] "]";

(define (parse-number str i)
  (cond [(or (= i (string-length str))
             (not (char-numeric? (string-ref str i))))
         #f]
        [else
         (let* ([c (string-ref str i)]
                [v (char->number c)]
                [rem (parse-number str (add1 i))])
           (if (eq? #f rem)
               (cons v (add1 i))
               (cons (+ (* 10 v) (car rem))
                     (cdr rem))))]))

(define (parse-list str i)
  (define (parse-loop i)
    (match (string-ref str i)
      [#\space (parse-loop (add1 i))]
      [#\, (parse-loop (add1 i))]
      [#\] (cons '() (add1 i))]
      [_ (match-let* ([(cons v pos) (parse-val str i)]
                      [(cons remv rempos) (parse-loop pos)])
           (cons (cons v remv) rempos))]))
  (if (char=? #\[ (string-ref str i))
      (parse-loop (add1 i))
      #f))

(define (parse-val str i)
  (or (parse-number str i)
      (parse-list str i)))

;; parser END

;; comparator BEGIN

(define (parse-input)
  (let ([lines (read-lines)])
    (for/list ([line lines]
               #:when (not (equal? "" line)))
      (car (parse-val line 0)))))

(define (compare a b)
  (cond [(< a b) '<]
        [(= a b) '=]
        [(> a b) '>]))

(define (my< a b)
  (cond [(and (number? a) (number? b)) (compare a b)]
        [(number? a) (my< (list a) b)]
        [(number? b) (my< a (list b))]
        [(and (null? a) (null? b)) '=]
        [(null? a) '<]
        [(null? b) '>]
        [else
         (let ([cmp1 (my< (car a) (car b))])
           (if (not (eq? cmp1 '=))
               cmp1
               (my< (cdr a) (cdr b))))]))

;; comparator END

(define (puzzle1 packets)
  (for/sum ([(p i) (in-indexed (chunks 2 packets))])
    (if (eq? '< (my< (first p) (second p)))
        (add1 i)
        0)))

(define (puzzle2 packets)
  (let* ([divider1 '((2))]
         [divider2 '((6))]
         [pkts (append (list '((2)) '((6))) packets)]
         [sorted (list->vector (sort pkts (λ (a b) (eq? '< (my< a b)))))])
    (* (add1 (vector-member divider1 sorted))
       (add1 (vector-member divider2 sorted)))))

(define (day13)
  (let ([packets (parse-input)])
    (println (puzzle1 packets))
    (println (puzzle2 packets))))

(time (day13))



