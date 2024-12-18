#lang racket

(struct Computer
  (a b c pointer program output)
  #:transparent)

(define (parse)
  (define lines (port->lines))
  (Computer (parse-register (first lines))
            (parse-register (second lines))
            (parse-register (third lines))
            0
            (parse-program (fifth lines))
            '()))

(define (parse2)
  (define lines (port->lines))
  (Computer 'a
            (parse-register (second lines))
            (parse-register (third lines))
            0
            (parse-program (fifth lines))
            '()))

(define (parse-register str)
  (string->number (second (regexp-match #px"Register .: (\\d+)" str))))

(define (parse-program str)
  (list->vector (map string->number (cdr (string-split str #px": |,")))))

(define (combo computer)
  (match (vector-ref (Computer-program computer) (add1 (Computer-pointer computer)))
    [(and (or 0 1 2 3) x) x]
    [4 (Computer-a computer)]
    [5 (Computer-b computer)]
    [6 (Computer-c computer)]
    [x (error 'combo "unknown combo: ~v" x)]))

(define (literal computer)
  (vector-ref (Computer-program computer) (add1 (Computer-pointer computer))))

(define (ast computer)
  (cond
    [(= (vector-length (Computer-program computer))
        (Computer-pointer computer))
     computer]
    [else
     (rec computer)]))

(define (adv computer)
  `(/ ,(Computer-a computer) (<< 1 ,(combo computer))))

(define (rec computer)
  (match (vector-ref (Computer-program computer) (Computer-pointer computer))
    [0 (ast (struct-copy Computer computer
                         [a (adv computer)]
                         [pointer (+ 2 (Computer-pointer computer))]))]
    [1 (ast (struct-copy Computer computer
                         [b `(xor ,(Computer-b computer) ,(literal computer))]
                         [pointer (+ 2 (Computer-pointer computer))]))]
    [2 (ast (struct-copy Computer computer
                         [b `(mod ,(combo computer) 8)]
                         [pointer (+ 2 (Computer-pointer computer))]))]
    [3
     (define a (eval1 (Computer-a computer)))
     (cond [(not (number? a))
            (list
              (ast (struct-copy Computer computer
                                [pointer (+ 2 (Computer-pointer computer))]))
              (ast (struct-copy Computer computer
                                [pointer (literal computer)])))
            ]
           [(= a 0)
            (ast (struct-copy Computer computer
                              [a a]
                              [pointer (+ 2 (Computer-pointer computer))]))]
           [else
            (ast (struct-copy Computer computer
                              [a a]
                              [pointer (literal computer)]))])]
    [4 (ast (struct-copy Computer computer
                         [b `(xor ,(Computer-b computer) ,(Computer-c computer))]
                         [pointer (+ 2 (Computer-pointer computer))]))]
    [5 (ast (struct-copy Computer computer
                         [output (cons `(mod ,(combo computer) 8) (Computer-output computer))]
                         [pointer (+ 2 (Computer-pointer computer))]))]
    [6 (ast (struct-copy Computer computer
                         [b (adv computer)]
                         [pointer (+ 2 (Computer-pointer computer))]))]
    [7 (ast (struct-copy Computer computer
                         [c (adv computer)]
                         [pointer (+ 2 (Computer-pointer computer))]))]))

(define (eval1 ast [A 'a])
  (match ast
    [`(/ ,a ,b) (quotient (eval1 a A) (eval1 b A))]
    [`(<< ,a ,b) (arithmetic-shift (eval1 a A) (eval1 b A))]
    [`(mod ,a ,b) (modulo (eval1 a A) (eval1 b A))]
    [`(xor ,a ,b) (bitwise-xor (eval1 a A) (eval1 b A))]
    ['a A]
    [x #:when (number? x) x]
    [x (error 'eval "unknown ast: ~v" x)]
    ))

(define computer (ast (parse2)))
(for/list ([o (reverse (Computer-output computer))])
  (eval1 o))
(displayln computer)

