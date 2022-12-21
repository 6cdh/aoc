#lang racket

(require "lib.rkt")

(define (readinput)
  (let ([lines (read-lines)])
    (for/hash ([line lines])
      (match (string-split line)
        [(list name dep1 op dep2) (values (substring name 0 (sub1 (string-length name)))
                                          (list op dep1 dep2))]
        [(list name val) (values (substring name 0 (sub1 (string-length name)))
                                 (string->number val))]))))

(define (solve-relation x op y result)
  (let ([to-result (list (list "+" +)
                         (list "-" -)
                         (list "*" *)
                         (list "/" quotient)
                         (list "=" =))]
        [to-x (list (list "+" -)
                    (list "-" +)
                    (list "*" quotient)
                    (list "/" *)
                    (list "=" (λ (_ y) y)))]
        [to-y (list (list "+" -)
                    (list "-" (λ (r x) (- x r)))
                    (list "*" quotient)
                    (list "/" (λ (r x) (quotient x r)))
                    (list "=" (λ (_ x) x)))])
    (match* (x y result)
      [('() '() _) null]
      [(_ '() '()) null]
      [('() _ '()) null]
      [('() y r) ((second (assoc op to-x)) r y)]
      [(x '() r) ((second (assoc op to-y)) r x)]
      [(x y '()) ((second (assoc op to-result)) x y)])))

(define (solve-equations name expect monkeys)
  (let ([action (hash-ref monkeys name null)])
    (cond [(null? action) expect]
          [(number? action) action]
          [(null? expect)
           (solve-relation (solve-equations (second action) null monkeys)
                           (first action)
                           (solve-equations (third action) null monkeys)
                           null)]
          [else
           (let ([ans1 (solve-equations (second action) null monkeys)]
                 [ans2 (solve-equations (third action) null monkeys)])
             (match* (ans1 ans2)
               [('() '()) (error "NO!!! both operands depend on `humn`")]
               [('() ans2) (solve-equations (second action)
                                            (solve-relation ans1 (first action) ans2 expect)
                                            monkeys)]
               [(ans1 '()) (solve-equations (third action)
                                            (solve-relation ans1 (first action) ans2 expect)
                                            monkeys)]))])))

(define (day21)
  (let ([monkeys (readinput)])
    (println (solve-equations "root" null monkeys))
    (println (solve-equations "root" #t
                              (~> (hash-remove monkeys "humn")
                                  (hash-update % "root"
                                               (λ (old) (list-set old 0 "="))))))))

(time (day21))
