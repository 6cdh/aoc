#lang racket

(require "lib.rkt")

(define (parse-stack lines)
  (let* ([m (sub1 (length
                   (takef lines (λ (line)
                                  (not (equal? line ""))))))]
         [n ($ (string-length (car lines)) + 1 / 4)]
         [stacks (make-array n '())])
    (for* ([line (reverse (take lines m))]
           [i n])
      (let ([c (string-ref line ($ i * 4 + 1))])
        (when (char-upper-case? c)
          (aupd! stacks i (λ (old) (cons c old))))))
    stacks))

(define (parse-procs lines)
  (let ([text (cdr (memf (curry equal? "") lines))])
    (for/list ([line text])
      (let ([words (string-split line)])
        (list (string->number (list-ref words 1))
              (sub1 (string->number (list-ref words 3)))
              (sub1 (string->number (list-ref words 5))))))))

(define (rearrange stacks procs transformer)
  (for ([p procs])
    (match-let ([(list k from to) p])
      (let ([moved (transformer (take (aref stacks from) k))])
        (aupd! stacks from (λ (old) (drop old k)))
        (aupd! stacks to (λ (old) (append moved old))))))
  (apply string (map car (vector->list stacks))))

(define (day05)
  (let* ([lines (read-lines)]
         [procs (parse-procs lines)])
    (displayln (rearrange (parse-stack lines) procs reverse))
    (displayln (rearrange (parse-stack lines) procs identity))))

(time (day05))




