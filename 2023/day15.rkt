#lang racket

(require "lib.rkt")

(define (box-hash str)
  (for/fold ([cur 0])
            ([c str])
    (~> cur
        (+ cur (char->integer c))
        (* cur 17)
        (modulo cur 256))))

(define (remove! box->labels label->focal label)
  (hash-update! box->labels (box-hash label)
                (λ (old)
                  (hash-remove! label->focal label)
                  (remove label old))
                '()))

(define (add! box->labels label->focal label focal)
  (hash-update! box->labels (box-hash label)
                (λ (old)
                  (hash-set! label->focal label focal)
                  (if (member label old)
                      old
                      (cons label old)))
                '()))

(define (operate box->labels label->focal str)
  (match (string->list str)
    [(list label-chars ... #\= focal-chars ...)
     (add! box->labels label->focal
           (list->string label-chars)
           (string->number (list->string focal-chars)))]
    [(list label-chars ... #\-)
     (remove! box->labels label->focal (list->string label-chars))]))

(define (part2 input)
  (define box->labels (make-hash))
  (define label->focal (make-hash))

  (for ([str input])
    (operate box->labels label->focal str))

  (for*/sum ([(box labels) box->labels]
             [(label slot) (in-indexed (reverse labels))]
             [focal (in-value (hash-ref label->focal label))])
    (* (add1 box) (add1 slot) focal)))

(define (main)
  (define input (string-split (read-line) ","))
  (println (sum (map box-hash input)))
  (println (part2 input)))

(main)
