#lang racket
(require racket/trace)
(require errortrace)

; mayhaps assoc? or hashmap!
(define *out-map* (make-hash))



(define (add-out out fun)
  ; 1. check if will collide
    (define (signify-collision-in-out-map out)
	  (error "out-map collision with symbol: " out)
	)
    (cond
	  [
	    (hash-ref *out-map* out '())
	    (signify-collision-in-out-map out)
	  ]
	)
  ; 2. transform symbols into strings(or maybe don't?)

  ; ... . add to out-map
  (hash-set! *out-map* out fun)
)


(add-out
    'o1
    '(or (and
            i1
            !i2
        )
        i3
        i4
    )
)


