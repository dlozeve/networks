#lang racket/base

(require racket/match)
(require graph)

(provide fruchterman-reingold)

(define (norm x y)
  (sqrt (+ (* x x) (* y y))))

(define (attractive-force k x)
  (/ (* x x) k))

(define (repulsive-force k x)
  (/ (* k k) x))

(define (fruchterman-reingold g iterations)
  (define-vertex-property g pos-x) ; Position
  (define-vertex-property g pos-y) ; Position
  (define-vertex-property g disp-x #:init 0) ; Displacement
  (define-vertex-property g disp-y #:init 0) ; Displacement
  ;; Initialize with random positions
  (for ([v (in-vertices g)])
    (pos-x-set! v (random))
    (pos-y-set! v (random)))
  (let* ([k (sqrt (/ 1 (length (get-vertices g))))]) ; Optimal distance between vertices
    (for* ([i (in-range iterations)]
	   [temp (in-range 0.1 0 (/ 0.1 iterations))]) ; Linear cooldown (initial temperature = 0.1)
      ;; Calculate the repulsive forces
      (for ([v (in-vertices g)])
	(for ([u (in-vertices g)])
	  (unless (vertex=? g u v)
	    (let* ([delta-x (- (pos-x v) (pos-x u))] ; Distance between vertices u and v
		   [delta-y (- (pos-y v) (pos-y u))]
		   [delta-norm (norm delta-x delta-y)]
		   [repulsion (repulsive-force k delta-norm)])
	      (disp-x-set! v (+ (disp-x v) (* repulsion (/ delta-x delta-norm))))
	      (disp-y-set! v (+ (disp-y v) (* repulsion (/ delta-y delta-norm))))))))
      ;; Calculate the attractive forces
      (for ([e (in-edges g)])
	(match-let ([(list u v) e])
	  (unless (vertex=? g u v)
	    (let* ([delta-x (- (pos-x v) (pos-x u))]
		   [delta-y (- (pos-y v) (pos-y u))]
		   [delta-norm (norm delta-x delta-y)]
		   [attraction (attractive-force k delta-norm)])
	      (disp-x-set! v (- (disp-x v) (* attraction (/ delta-x delta-norm))))
	      (disp-y-set! v (- (disp-y v) (* attraction (/ delta-y delta-norm))))
	      (disp-x-set! u (- (disp-x u) (* attraction (/ delta-x delta-norm))))
	      (disp-y-set! u (- (disp-y u) (* attraction (/ delta-y delta-norm))))))))
      ;; Limit max displacement to temperature temp and prevent from
      ;; displacement outside the frame
      (for ([v (in-vertices g)])
	(let ([disp-norm (norm (disp-x v) (disp-y v))])
	  (pos-x-set! v (+ (pos-x v) (* (/ (disp-x v) disp-norm) (min disp-norm temp))))
	  (pos-y-set! v (+ (pos-y v) (* (/ (disp-y v) disp-norm) (min disp-norm temp))))
	  (pos-x-set! v (min 1 (max 0 (pos-x v))))
	  (pos-y-set! v (min 1 (max 0 (pos-y v)))))))
    ;; Return the positions in a single hashtable
    (for/hash ([v (in-vertices g)])
      (values v (vector (pos-x v) (pos-y v))))))
