#lang racket/base

(require graph
	 math/distributions
	 math/matrix
	 racket/list)

(provide erdos-renyi
	 barabasi-albert
	 stochastic-block-model
	 in-out-matrix
	 regular-partition)

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

(define (stochastic-block-model n partition edge-prob)
  (define g (undirected-graph null))
  (for ([i (in-range n)])
    (add-vertex! g i))
  (for* ([i (in-range n)]
	 [j (in-range i)])
    (let ([community-i (index-where partition (lambda (l) (member i l)))]
	  [community-j (index-where partition (lambda (l) (member j l)))])
      (when (< (random) (matrix-ref edge-prob community-i community-j))
	(add-edge! g i j))))
  g)

(define (in-out-matrix r p-in p-out)
  (build-matrix r r
		(lambda (i j) (if (= i j) p-in p-out))))

(define (regular-partition l r)
  (if (> r 1)
      (let* ([n (length l)]
	     [size (quotient n r)])
	(cons (take l size) (regular-partition (drop l size) (- r 1))))
      (list l)))
