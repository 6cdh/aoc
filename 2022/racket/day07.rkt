#lang racket

(require "lib.rkt")

(define (read-fs)
  (let ([lines (read-lines)]
        ;; fs : (Hash string (or Int fs))
        [fs (make-hash (list (cons "/" (make-hash))))]
        [pwd '("/")])
    (for ([line lines])
      (match (string-split line)
        [(list "$" "cd" "/") (set! pwd '("/"))]
        [(list "$" "cd" "..") (set! pwd (cdr pwd))]
        [(list "$" "cd" dir) (set! pwd (cons dir pwd))]
        [(list "$" "ls") (void)]
        [(list "dir" dir) (insert! fs (reverse pwd) dir (make-hash))]
        [(list size file) (insert! fs (reverse pwd) file (string->number size))]))
    fs))

(define (insert! fs pwd name val)
  (if (null? pwd)
      (hash-set! fs name val)
      (insert! (hash-ref fs (car pwd))
               (cdr pwd)
               name val)))

(define (for-each-dir-size fs fn)
  (if (number? fs)
      fs
      (let ([size (for/sum ([(_ val) fs])
                    (for-each-dir-size val fn))])
        (fn size)
        size)))

(define (day07)
  (let ([fs (read-fs)])
    (let ([ans 0])
      (for-each-dir-size fs
                         (λ (s) (when (<= s #e1e5)
                                  (set! ans (+ ans s)))))
      (println ans))
    (let* ([root (for-each-dir-size fs identity)]
           [at-least-free (- #e3e7 (- #e7e7 root))]
           [ans root])
      (for-each-dir-size fs
                         (λ (s) (when (>= s at-least-free)
                                  (set! ans (min ans s)))))
      (println ans))))

(time (day07))



