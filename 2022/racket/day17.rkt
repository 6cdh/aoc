#lang racket

(require "lib.rkt")

(struct Stone [h w pos])

(define (move1 pos dir)
  (map + pos dir))

(define (overlaps? pos stone room)
  (for/or ([p (Stone-pos stone)])
    (let ([p1 (move1 pos p)])
      (or (< (second p1) 0)
          (>= (second p1) 7)
          (>= (first p1) (length room))
          (and (>= (first p1) 0)
               (eq? #\# (~> room
                            (list-ref % (first p1))
                            (string-ref % (second p1)))))))))

(define (move pos dir stone room)
  (let ([new-pos (move1 pos dir)])
    (if (overlaps? new-pos stone room)
        (cons pos #f)
        (cons new-pos #t))))

(define (simulate jets stones queries)
  (define n (maximum queries))
  (define (next-j j)
    (modulo (add1 j) (string-length jets)))

  (define (draw! j stone room)
    (define (rec pos j room)
      (match-let* ([(cons pos1 _) (match (string-ref jets j)
                                    [#\> (move pos '(0 1) stone room)]
                                    [#\< (move pos '(0 -1) stone room)]
                                    [jet (error "unexpected jet" jet)])]
                   [(cons pos2 succ) (move pos1 '(1 0) stone room)])
        (cond [succ
               (rec pos2 (next-j j) room)]
              [else
               ; TODO
               (let* ([extra (max 0 (- (first pos1)))]
                      [new-room (append (build-list extra
                                                    (λ _ (make-string 7 #\.)))
                                        room)])
                 (for ([p (Stone-pos stone)])
                   (let ([p1 (move1 pos1 p)])
                     (~> (list-ref new-room (+ extra (first p1)))
                         (string-set! % (second p1) #\#))))
                 (cons (next-j j) new-room))])))
    (rec (list (- 0 (Stone-h stone) 3) 2) j room))

  (define (faster-simulate i j s lens room mem matched?)
    (match-let* ([(cons new-j new-room) (draw! j (list-ref stones s) room)]
                 [memkey (list j s (car new-room))])
      (cond [(= i n) (for/list ([q queries])
                       (list-ref lens (- n q)))]
            [(and matched? (hash-has-key? mem memkey))
             (let* ([cycle-from (hash-ref mem memkey)]
                    [cycle-to (sub1 i)]
                    [cycle-len (- i cycle-from)])
               (for/list ([q queries])
                 (let-values ([(repeats rem) (quotient/remainder
                                              (- q (sub1 cycle-from)) cycle-len)]
                              [(height) (length lens)])
                   (define (height-of i)
                     (list-ref lens (- height i)))
                   ;; some math to get lens[q] that is the height after `q` times simulations
                   ;; Let's say Δq = lens[q] - lens[q-1], then
                   ;; lens[q] = Δ1 + Δ(from-1) + repeats * (Δ(from) + ... + Δ(to))
                   ;;                          + Δ(from) + ... + Δ(from+rem-1)
                   ;;         = lens[from-1] + repeats * (lens[to] - lens[from-1])
                   ;;                        + lens[from+rem-1] - lens[from-1]
                   (+ (height-of (sub1 cycle-from))
                      (* repeats (- (height-of cycle-to) (height-of (sub1 cycle-from))))
                      (- (height-of (+ cycle-from rem -1))
                         (height-of (sub1 cycle-from)))))))]
            [else
             (faster-simulate (add1 i)
                              new-j
                              (modulo (add1 s) (length stones))
                              (cons (length new-room) lens)
                              new-room
                              (hash-set mem memkey i)
                              (hash-has-key? mem memkey))])))
  (faster-simulate 0 0 0 '() '() (hash) #f))

(define (day17)
  (let ([jets (read-line)]
        [stones (list (Stone 1 4 '((0 0) (0 1) (0 2) (0 3)))
                      (Stone 3 3 '((0 1) (1 0) (1 1) (1 2) (2 1)))
                      (Stone 3 3 '((0 2) (1 2) (2 0) (2 1) (2 2)))
                      (Stone 4 1 '((0 0) (1 0) (2 0) (3 0)))
                      (Stone 2 2 '((0 0) (0 1) (1 0) (1 1))))])
    (for ([res (simulate jets stones '(2022 #e1e12))])
      (println res))))

(time (day17))

