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

(define (fruchterman-reingold g width height iterations)
  (define-vertex-property g pos-x) ; Position
  (define-vertex-property g pos-y) ; Position
  (define-vertex-property g disp-x #:init 0) ; Displacement
  (define-vertex-property g disp-y #:init 0) ; Displacement
  ;; Initialize with random positions
  (for ([v (in-vertices g)])
    (pos-x-set! v (- (* (random) width) (/ width 2)))
    (pos-y-set! v (- (* (random) width) (/ width 2))))
  (let* ([area (* width height)] ; Area of the frame
	 [k (sqrt (/ area (length (get-vertices g))))]) ; Optimal distance between vertices
    (for* ([i (in-range iterations)]
	   [initial-temp (min (/ width 10) (/ height 10))] ; Initial temperature
	   [temp (in-range initial-temp 0 (/ initial-temp iterations))]) ; Linear cooldown
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
	  (pos-x-set! v (min (/ width 2) (max (- (/ width 2)) (pos-x v))))
	  (pos-y-set! v (min (/ width 2) (max (- (/ width 2)) (pos-y v)))))))
    ;; Return the positions in a single hashtable
    (for/hash ([v (in-vertices g)])
      (values v (vector (pos-x v) (pos-y v))))))
