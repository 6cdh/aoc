#lang racket

(require "lib.rkt")

(define (read-fs)
  (let ([lines (read-lines)]
        [fs (make-hash (list (cons "/" (make-hash))))]
        [pwd '("/")])
    (for ([line lines])
      (match (string-ref line 0)
        [#\$ (when (char=? #\c (string-ref line 2))
               (match (substring line 5)
                 ["/" (set! pwd '("/"))]
                 [".." (set! pwd (cdr pwd))]
                 [dir (set! pwd (cons dir pwd))]))]
        [#\d (let ([name (second (string-split line))])
               (insert! fs (reverse pwd) name (make-hash)))]
        [_ (let* ([ws (string-split line)]
                  [size (string->number (first ws))]
                  [name (second ws)])
             (insert! fs (reverse pwd) name size))]))
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
      (let ([size 0])
        (for ([(_ val) fs])
          (set! size (+ size (for-each-dir-size val fn))))
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
      (for-each-dir-size fs (λ (s) (when (>= s at-least-free)
                                     (set! ans (min ans s)))))
      (println ans))))

(time (day07))


