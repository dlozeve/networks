#lang racket/gui

(require racket/match
	 graph)
(require "random.rkt"
	 "layout.rkt")

(define (draw-layout dc g layout)
  (let-values ([(width height) (send dc get-size)])
    (for ([(k v) layout])
      (send dc set-pen "black" 10 'solid)
      (send dc draw-point (* width (vector-ref v 0)) (* height (vector-ref v 1))))
    (for ([e (in-edges g)])
      (match-let ([(list u v) e])
	(send dc set-pen "black" 1 'solid)
	(send dc draw-line
	      (* width (vector-ref (hash-ref layout u) 0)) (* height (vector-ref (hash-ref layout u) 1))
	      (* width (vector-ref (hash-ref layout v) 0)) (* height (vector-ref (hash-ref layout v) 1)))))))

(define iterations 1000000)
(define g (barabasi-albert 50 2))
(define layout (fruchterman-reingold g iterations))

(define frame (new frame%
		   [label "Graph"]
		   [width 1000]
		   [height 1000]))

(define drawing-canvas%
  (class canvas%
    (define/override (on-event event)
      (void))
    (define/override (on-char event)
      (match (send event get-key-code)
	[#\q (send frame show #f)]
	[#\n
	 (set! g (barabasi-albert 50 2))
	 (set! layout (fruchterman-reingold g iterations))
	 (send this on-paint)]
	[#\r
	 (set! layout (fruchterman-reingold g iterations))
	 (send this on-paint)]
	[x (writeln x)]))
    (super-new)))

(new drawing-canvas%
     [parent frame]
     [paint-callback (lambda (canvas dc)
		       (send dc set-smoothing 'smoothed)
		       (send dc clear)
		       (draw-layout dc g layout))])

(send frame show #t)
