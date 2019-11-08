#lang racket
(require racket/trace)
(require errortrace)


(define (assert condition)
	
  )

; mayhaps assoc? or hashmap!
; TODO: remove global state
(define *out-map* (make-hash))
(define *list-input* (list))
(define *set-output* (make-hash))
(define *list-flag* (list)) ; flags shall be defined by the program in some cases, when minimizing functions


(struct parsed-outfun (out-symbol boolfun inputs-list truthtable))
(define (make-parsed-outfun out-symbol boolfun)
	(assert (symbol? out-symbol))
	(assert (valid-boolfun? boolfun))
)

; TODO: truth table
(struct truth-table (count-input vector-input vector-value))
(define (number->vector-bool n)
	(cond [(not (integer? n)) (error "not integer:" n)] )
	(define vector-bool '()) ; TODO

	vector-bool
)


(define (function->list-variables f)
	(define flat (flatten f))
	(define varlist (filter flat variable?))
	(define cleaned (remove-duplicates varlist))
	(define sorted (sort cleaned))

	sorted
)


(define (new-truth-table f)
	(error "unfinished")
	(define list-input (function->list-variables f))
	(define vector-values '())


	(truth-table (count list-input) (list->vector list-input) vector-values)
)


{define list-base-functions
	'(
		!
		not; TODO: should `not`|`!` be a special operator with special treatment? probably not, but maybe it would be better
		and
		or
		nor
		nand
		xor
	)
}

(define list-axioms
	'((= (and #f #f) #f)
	  (= (or  #f #f) #f)
	  (= (and #t #t) #t)
	  (= (or  #t #t) #t)
	  (= (or  #f #t) (or #t #f) #t)
	  (= (and #f #t) (and #t #f) #f)
	  (= (not #t) #f)
	  (= (not #f) #t)
	)
)


(define (add-input symbol)
	(cond 
	  [
	   (not (member symbol *list-input*))
	   (set! *list-input* (append *list-input* (list symbol) ) )
	  ]
	)
)


(define (add-output output-list)
	;TODO
	*set-output* 
)


(define (add-out out fun)
  ; 1. check if will collide
    (cond
	  [
	    (not (variable? out))
		(error "out has to be a variable. instead is:" out)
	  ]
	  [
	    (hash-ref *out-map* out #f)
		(error "add-out collision with symbol: " out)
	  ]
    )
  ; 2. transform symbols into strings(or maybe don't?)

  ; ... . add to out-map
  (hash-set! *out-map* out fun)
)
(define (test-add-out-collision)
	(add-out 'colision-test '1)
	(add-out 'colision-test '1)
)


(define (define-outs outs)
	(define outs-list (list))
	; TODO

	outs-list
)



(define (funs-equivalent? f1 f2)
    (error "unfinished")
    (cond
      [
		(eq? f1 f2)
	  ]
    )
)


(define (base-function? x)
	(and
	  (symbol? x)
	  (member x list-base-functions)
	)
)


(define (variable? x)
	(and
		(symbol? x)
		(not (base-function? x))
	)
)

(define (valid-function? x)
	(error "unfinished")
)







(add-out
  'a1
  'b1
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



;; tests

;(test-add-out-collision)
