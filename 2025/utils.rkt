#lang racket/base

(provide parallel-run)

(define-syntax-rule (parallel-run expr)
  (thread #:pool 'own #:keep 'results
          (lambda () expr)))

