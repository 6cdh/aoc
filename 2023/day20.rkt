#lang racket

(require "lib.rkt"
         rebellion/type/enum)

;; network BEGIN

(define-enum-type Pulse
  (Low High))

(define-enum-type ModuleType
  (Flip-Flop
   Conjunction
   Broadcast
   Undefined))

(struct Module
  (type state)
  #:mutable
  #:transparent)

(define (make-flip-flop)
  (Module Flip-Flop #f))

(define (flip-flop mod input from)
  (match input
    [(== High) null]
    [(== Low)
     (set-Module-state! mod (not (Module-state mod)))
     (if (Module-state mod) High Low)]))

(define (make-conjunction n)
  (Module Conjunction (list (mutable-set) n)))

(define (conjunction mod input from)
  (define s (first (Module-state mod)))
  (define n (second (Module-state mod)))
  (match input
    [(== High) (set-add! s from)]
    [(== Low) (set-remove! s from)])
  (if (= n (set-count s)) Low High))

(define (make-broadcast)
  (Module Broadcast null))

(define (broadcast mod input from)
  input)

(define (make-undefined)
  (Module Undefined null))

(define (process mod input from)
  (match (Module-type mod)
    [(== Flip-Flop) (flip-flop mod input from)]
    [(== Conjunction) (conjunction mod input from)]
    [(== Broadcast) (broadcast mod input from)]
    [_ null]))

;; network END

;; read BEGIN

(struct Node
  (mod tos)
  #:transparent)

(define (read-network lines)
  (define conjunctions (make-hash))
  (define network (make-hash))
  (for ([line lines])
    (match line
      [(regexp #px"broadcaster -> (.*)" (list _ tos))
       (hash-set! network "broadcaster"
                  (Node (make-broadcast)
                        (string-split tos ", ")))]
      [(regexp #px"%(.*) -> (.*)" (list _ name tos))
       (hash-set! network name
                  (Node (make-flip-flop)
                        (string-split tos ", ")))]
      [(regexp #px"&(.*) -> (.*)" (list _ name tos))
       (hash-set! conjunctions name 0)
       (hash-set! network name
                  (Node #f (string-split tos ", ")))]))
  (define undefined-modules (mutable-set))
  (for* ([(_ node) network]
         [to (Node-tos node)])
    (set-add! undefined-modules to)
    (when (hash-has-key? conjunctions to)
      (hash-update! conjunctions to add1)))
  (for ([(name from-cnt) conjunctions])
    (hash-update! network name
                  (λ (node) (Node (make-conjunction from-cnt) (Node-tos node)))))
  (for ([um undefined-modules]
        #:when (not (hash-has-key? network um)))
    (hash-set! network um (Node (make-undefined) '())))
  network)

(define (add-button network)
  (hash-set! network "button"
             (Node (make-broadcast) '("broadcaster")))
  network)

;; read END

(struct Signal
  (name pulse from)
  #:transparent)

;; part 1 BEGIN

(define (count-pulses network n)
  (define highs 0)
  (define lows 0)

  (define (send-pulse signals)
    (when (not (null? signals))
      (define new-signals '())
      (for ([signal signals])
        (match signal
          [(Signal name pulse from)
           (define node (hash-ref network name))
           (define output (process (Node-mod node) pulse from))
           (define tos (Node-tos node))

           (match output
             [(== Low) (set! lows (+ lows (length tos)))]
             [(== High) (set! highs (+ highs (length tos)))]
             [_ (void)])
           (when (not (null? output))
             (for ([to tos])
               (list-push-head! new-signals (Signal to output name))))]))
      (send-pulse new-signals)))

  (for ([_ n])
    (send-pulse (list (Signal "button" Low "hand"))))
  (* highs lows))

;; part 1 END

;; part 2 BEGIN

;; helper function for generate and print graphviz diagram
;; can be view at https://edotor.net/
(define (draw-network network)
  (displayln "digraph circuit {")
  (displayln "rankdir=LR;")

  (displayln (format "node [shape=square] rx;"))
  (for ([(name node) network])
    (match (Module-type (Node-mod node))
      [(== Flip-Flop)
       (displayln (format "node [shape=circle] ~a;" name))]
      [(== Conjunction)
       (displayln (format "node [shape=doublecircle] ~a;" name))]
      [_ (void)]))

  (for* ([(name node) network]
         [to (Node-tos node)])
    (displayln (format "~a -> ~a" name to)))

  (displayln "}"))

(define (find-second-last network rx)
  (for/first ([(name node) network]
              #:when (equal? (list rx) (Node-tos node)))
    name))

(define (extract-network network start end)
  (define small-network (make-hash))

  (define (rec name)
    (hash-set! small-network name (hash-ref network name))
    (for ([next (Node-tos (hash-ref network name))]
          #:when (and (not (hash-has-key? small-network next))
                      (not (string=? next end))))
      (rec next)))

  (rec start)

  small-network)

(define (small-network/fewest-press network start input output)
  (define (send-pulse signals)
    (if (null? signals)
        #f
        (let ()
          (define new-signals '())
          (define end? #f)
          (for ([signal signals])
            (match signal
              [(Signal name pulse from)
               #:when (hash-has-key? network name)

               (define node (hash-ref network name))
               (define output (process (Node-mod node) pulse from))
               (define tos (Node-tos node))

               (when (not (null? output))
                 (for ([to tos])
                   (list-push-head! new-signals (Signal to output name))))]
              [(Signal name pulse from)
               (when (eq? output pulse)
                 (set! end? #t))]))
          (if end?
              #t
              (send-pulse new-signals)))))

  (define (run-and-output-expected?)
    (send-pulse (list (Signal start input "broadcaster"))))

  (for/first ([i (in-naturals 1)]
              #:when (run-and-output-expected?))
    i))

;; By using `draw-network` function, we discover that
;; the network consists of several independent small networks,
;; these subnetworks connect to the broadcaster and the last
;; common conjunction module, which connects to `rx`.
;; So we find the fewest press of these subnetworks, and calculate
;; the lcm of them.
;; each subnetwork has around 3000 fewest press for my input.
(define (fewest-press network)
  (when (not (hash-has-key? network "rx"))
    (error "no `rx` module"))

  (define second-last (find-second-last network "rx"))
  (apply lcm
         (for/list ([start (Node-tos (hash-ref network "broadcaster"))])
           (small-network/fewest-press
            (extract-network network start second-last)
            start
            Low
            High))))

;; part 2 END

(define (main)
  (define lines (read-lines))
  (define network (add-button (read-network lines)))
  (println (count-pulses network 1000))

  (define network2 (add-button (read-network lines)))
  (println (fewest-press network2)))

(main)

