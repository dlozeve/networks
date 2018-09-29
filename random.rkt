#lang racket/base

(require graph)

(provide erdos-renyi)

(define (erdos-renyi n p)
  (let ([g (undirected-graph null)])
    (for ([i (in-range n)])
      (add-vertex! g i))
    (for* ([i (in-range n)]
	   [j (in-range n)])
      (when (< (random) p) (add-edge! g i j)))
    g))

