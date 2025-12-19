#lang racket

(require advent-of-code/meta
         advent-of-code)

(struct CLI-Arg
  [day mode])

(define (get-solver day)
  (define name (format "day~a.rkt" (~r day #:min-width 2 #:pad-string "0")))
  (dynamic-require name 'solve))

(define (run arg)
  (match-define (CLI-Arg day mode) arg)
  (define solve (get-solver day))
  (collect-garbage)
  (time (execute-mode day mode solve)))

(define (execute-mode day mode solve)
  (match mode
    ['test
     (define-values (ans1 ans2) (solve (current-input-port)))
     (displayln ans1)
     (displayln ans2)]
    ['ready
     (define session (find-session))
     (define in (open-aoc-input session 2025 day #:cache #t))
     (define-values (ans1 ans2) (solve in))
     (displayln ans1)
     (displayln ans2)]
    ['submit
     (define session (find-session))
     (define in (open-aoc-input session 2025 day #:cache #t))
     (define-values (ans1 ans2) (solve in))
     (define submit-part1-before?
       (if (void? ans1)
           (begin
             (displayln "You didn't solve part 1.")
             #f)
           (filter-response (aoc-submit session 2025 day 1 ans1) 'part1)))
     (define submit-part2-before?
       (if (void? ans2)
           (begin
             (displayln "You didn't solve part 2.")
             #f)
           (filter-response (aoc-submit session 2025 day 2 ans2) 'part2)))
     (when (and submit-part1-before? submit-part2-before?)
       (displayln "You solved both parts. Nothing to submit."))]))

(define (filter-response resp name)
  (define completed-str "You don't seem to be solving the right level.")
  (cond [(string-contains? resp completed-str)
         #t]
        [else
         (displayln (format "Submitted ~a." name))
         (displayln resp)
         #f]))

(define (parse-int str mini maxi name)
  (define n (string->number str))
  (if (and (exact-integer? n) (<= mini n maxi))
      n
      (error (string->symbol (format "parse-~a" name))
             "invalid ~a ~a [~a <= ~a <= ~a]"
             name str mini name maxi)))

(define (parse-day x)
  (parse-int x 1 12 'day))

(define (print-help-and-exit)
  (displayln "Usage: racket aoc.rkt <day> <mode>")
  (displayln "")
  (displayln "Arguments:")
  (displayln "  <day>   The day of the Advent of Code puzzle (1-12).")
  (displayln "  <mode>  The execution mode:")
  (displayln "          - 'test' or '-t': Run with test data from stdin and print to stdout.")
  (displayln "          - 'ready' or '-r': Run with your input and print to stdout, but don't submit.")
  (displayln "          - 'submit' or '-s': Run with your input and submit.")
  (displayln "")
  (displayln "Options:")
  (displayln "  -h, --help, help  Show help message and exit.")
  (displayln "")
  (displayln "Examples:")
  (displayln "  racket aoc.rkt 1 test   ; Run day 1 with test mode")
  (displayln "  racket aoc.rkt 2 -s     ; Run day 2, and submit the answers for incompleted parts.")
  (exit))

(define (parse-command-line-args)
  (match (current-command-line-arguments)
    [(vector (or "-h" "--help" "help"))
     (print-help-and-exit)]
    [(vector)
     (print-help-and-exit)]
    [(vector day (or "-t" "test"))
     (CLI-Arg (parse-day day) 'test)]
    [(vector day (or "-r" "ready"))
     (CLI-Arg (parse-day day) 'ready)]
    [(vector day (or "-s" "submit"))
     (CLI-Arg (parse-day day) 'submit)]
    [_
     (displayln
       (format "unknown arguments: ~a"
               (string-join
                 (vector->list
                   (current-command-line-arguments))
                 " ")))
     (print-help-and-exit)]))

(run (parse-command-line-args))

