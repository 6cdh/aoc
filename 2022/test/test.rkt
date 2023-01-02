#lang racket

(require rackunit)

(define (run-test case)
  (let ([input (cdr (assoc 'input case))]
        [run (cdr (assoc 'run case))]
        [output (cdr (assoc 'output case))])
    (define result (with-output-to-string
                     (λ ()
                       (system (string-join (append run (list "<") input)
                                            " ")))))
    (let ([res (string-split result "\n")])
      (for ([r res]
            [out output])
        (check-equal? r out)))))

(let* ([filename (command-line
                  #:program "tester"
                  #:args (filename)
                  filename)])
  (with-input-from-file filename
    (λ ()
      (let* ([cases (read)]
             [n (length cases)])
        (for ([case cases]
              [i n])
          (run-test case)
          (display (format "\rcases ~a/~a" (add1 i) n)))
        (newline)))))

