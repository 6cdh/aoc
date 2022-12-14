#lang racket

(provide make-array*
         make-array
         array-length
         array-size
         aref*
         aref
         aset*!
         aset!
         aupd*!
         aupd!
         array-copy)

(struct Array (dims vals))

(define (dims->index dims spec)
  (let ([k (length dims)])
    (+ (last dims)
       (for/sum ([dim dims]
                 [n spec]
                 [_ (sub1 k)])
         (* dim n)))))

(define-syntax-rule (make-array* dims ... init)
  (make-array (list dims ...) init))

(define (make-array dims init)
  (let ([size (foldl * 1 dims)])
    (Array dims (make-vector size init))))

(define (array-size arr dim)
  (list-ref (Array-dims arr) (sub1 dim)))

(define (array-length arr)
  (vector-length (Array-vals arr)))

(define (aref arr dims)
  (let ([index (dims->index dims (Array-dims arr))])
    (vector-ref (Array-vals arr) index)))

(define-syntax-rule (aref* arr dims ...)
  (aref arr (list dims ...)))

(define-syntax-rule (aset*! arr dims ... val)
  (aset! arr (list dims ...) val))

(define (aset! arr dims val)
  (let ([index (dims->index dims (Array-dims arr))])
    (vector-set! (Array-vals arr) index val)))

(define-syntax-rule (aupd*! arr dims ... updater)
  (aupd! arr (list dims ...) updater))

(define (aupd! arr dims updater)
  (aset! arr dims (updater (aref arr dims))))

(define (array-copy arr)
  (Array (Array-dims arr) (vector-copy (Array-vals arr))))

