#lang racket

(require "lib.rkt")

;; Brick BEGIN

(struct Brick
  (start end)
  #:transparent)

(struct Pos
  (x y z)
  #:transparent)

(define (Brick-endz b)
  (Pos-z (Brick-end b)))

(define (Brick-startz b)
  (Pos-z (Brick-start b)))

(define (Brick-startx b)
  (min (Pos-x (Brick-start b))
       (Pos-x (Brick-end b))))

(define (Brick-endx b)
  (max (Pos-x (Brick-start b))
       (Pos-x (Brick-end b))))

(define (Brick-starty b)
  (min (Pos-y (Brick-start b))
       (Pos-y (Brick-end b))))

(define (Brick-endy b)
  (max (Pos-y (Brick-start b))
       (Pos-y (Brick-end b))))

(define (Pos-str x y z)
  (Pos (string->number x)
       (string->number y)
       (string->number z)))

(define (upward-Brick pos1 pos2)
  (if (<= (Pos-z pos1) (Pos-z pos2))
      (Brick pos1 pos2)
      (Brick pos2 pos1)))

;; Brick END

(define (read-input)
  (for/list ([line (read-lines)])
    (match (string-split line #px"[~,]")
      [(list x1 y1 z1 x2 y2 z2)
       (upward-Brick (Pos-str x1 y1 z1) (Pos-str x2 y2 z2))])))

(define (sort-bricks/bottom->top bricks)
  (sort bricks < #:key Brick-startz))

(define (can-support? lower higher)
  (and (>= (- (min (Brick-endx lower) (Brick-endx higher))
              (max (Brick-startx lower) (Brick-startx higher)))
           0)
       (>= (- (min (Brick-endy lower) (Brick-endy higher))
              (max (Brick-starty lower) (Brick-starty higher)))
           0)))

(define (fall sorted-bricks)
  (define final-bricks '())
  (for ([b sorted-bricks])
    (define new-startz 1)
    (for ([pb final-bricks]
          #:when (can-support? pb b))
      (set! new-startz (max new-startz (add1 (Brick-endz pb)))))
    (list-push-head! final-bricks
                     (Brick (Pos (Pos-x (Brick-start b))
                                 (Pos-y (Brick-start b))
                                 new-startz)
                            (Pos (Pos-x (Brick-end b))
                                 (Pos-y (Brick-end b))
                                 (+ new-startz (- (Brick-endz b) (Brick-startz b)))))))
  final-bricks)

(define (find-supporters sorted-bricks)
  (define brick->aboves (make-hash))
  (define brick->belows (make-hash))
  (for ([b sorted-bricks])
    (hash-set! brick->aboves b '())
    (hash-set! brick->belows b '())
    (for ([pb sorted-bricks]
          #:when (and (= (Brick-endz pb) (sub1 (Brick-startz b)))
                      (can-support? pb b)))
      (hash-update! brick->aboves pb (λ (old) (cons b old)))
      (hash-update! brick->belows b (λ (old) (cons pb old)))))
  (list brick->belows brick->aboves))

(define (count-removable brick->belows brick->aboves)
  (for/count ([(_ aboves) brick->aboves])
    (or (null? aboves)
        (for/and ([above aboves])
          (>= (length (hash-ref brick->belows above)) 2)))))

(define (try-falls b brick->belows brick->aboves)
  (define falled (mutable-set))

  (define (bfs bricks)
    (define higher-bricks (mutable-set))
    (for* ([b bricks]
           [above (hash-ref brick->aboves b)]
           #:when (andmap (λ (below) (set-member? falled below))
                          (hash-ref brick->belows above)))
      (set-add! falled above)
      (set-add! higher-bricks above))
    (when (not (set-empty? higher-bricks))
      (bfs higher-bricks)))

  (set-add! falled b)
  (bfs (set b))
  (sub1 (set-count falled)))

(define (count-falls brick->belows brick->aboves)
  (for/sum ([b (hash-keys brick->aboves)])
    (try-falls b brick->belows brick->aboves)))

(define (main)
  (define bricks (read-input))
  (match-define (list brick->belows brick->aboves)
    (find-supporters (sort-bricks/bottom->top (fall (sort-bricks/bottom->top bricks)))))
  (println (count-removable brick->belows brick->aboves))
  (println (count-falls brick->belows brick->aboves)))

(main)

