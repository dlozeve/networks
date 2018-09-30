#lang racket/base

(require graph
	 math/distributions)

(provide erdos-renyi
	 barabasi-albert)

(define (erdos-renyi n p)
  (define g (undirected-graph null))
  (for ([i (in-range n)])
    (add-vertex! g i))
  (for* ([i (in-range n)]
	 [j (in-range n)])
    (when (< (random) p) (add-edge! g i j)))
  g)

(define (degree g v)
  (length (get-neighbors g v)))

(define (barabasi-albert n m)
  (define g (undirected-graph
	     (for*/list ([i (in-range m)]
			 [j (in-range m)])
	       (list i j))))
  (for ([i (in-range m n)])
    (let* ([degrees (for/list ([v (in-vertices g)]) (degree g v))]
	   [neighbors (sample (discrete-dist (get-vertices g) degrees) m)])
      (for ([n neighbors])
	(add-edge! g i n))))
  g)
