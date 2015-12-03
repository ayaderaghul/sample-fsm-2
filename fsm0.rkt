#! /usr/bin/env racket -tm
#lang racket

(provide main)

(require "population.rkt" "utilities.rkt" "scan.rkt" plot)

(plot-new-window? #t)

(define (main)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define ps
    (time
     (simulation->lines
      (evolve (build-random-population 100) 5000 10 20 1))))
  (define h3 (function (lambda (x) 3) #:color "blue"))
  (define h1 (function (lambda (x) 1) #:color "red"))
  (plot (list h3 h1 ps) #:y-min 0.0 #:y-max 4.0))

(define (simulation->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define (evolve population cycles speed rounds-per-match mutation)
  (cond
    [(zero? cycles) '()]
    [else (define p2 (match-up* population rounds-per-match))
          (define pp (population-payoffs p2))
          (define p3 (regenerate p2 speed))
          (mutate* p3 mutation)
          (out-rank cycles p3 10 "rank")
          (cons (relative-average pp rounds-per-match)
                (evolve p3 (- cycles 1) speed rounds-per-match mutation))]))

(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))
