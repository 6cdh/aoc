#lang racket

;; ===== manage script =====
;; aoc cookie get
;; aoc cookie set
;; aoc fetch 1
;; aoc run 1

(require net/url)

(define cookie-file ".cookie")

(define (cookie-get)
  (if (file-exists? cookie-file)
      (file->string cookie-file)
      (error "cookie not set")))

(define (cookie-set! cookie)
  (display-to-file cookie cookie-file
                   #:exists 'replace))

(define (format-day day)
  (cond [(< day 10) (format "day0~a" day)]
        [else (format "day~a" (number->string day))]))

(define (check-day! day)
  (when (not (and (integer? day) (<= 1 day 25)))
    (error "invalid day:" day)))

(define (fetch day)
  (define (fetch-raw day)
    (port->string
     (get-pure-port (string->url (format "https://adventofcode.com/2023/day/~a/input" day))
                    (list (format "Cookie: ~a" (cookie-get))))))

  (check-day! day)
  (let ([data (fetch-raw day)])
    (display-to-file data
                     (format "input/~a.txt" (format-day day))
                     #:exists 'replace)
    (displayln (format "fetched ~a lines of data"
                       (count (λ (c) (eq? c #\newline)) (string->list data))))))

(define (run day)
  (check-day! day)
  (system (format "racket ~a.rkt < input/~a.txt" (format-day day) (format-day day))))

(define (main)
  (match (current-command-line-arguments)
    [(vector "cookie" "get") (displayln (cookie-get))]
    [(vector "cookie" "set" cookie) (cookie-set! cookie)]
    [(vector "fetch" day) (fetch (string->number day))]
    [(vector "run" day) (run (string->number day))]
    [_ (displayln (string-join '("usage: aoc [command]"
                                 "command:"
                                 "  cookie set"
                                 "  cookie get"
                                 "  fetch day") "\n"))])
  (void))

(main)


