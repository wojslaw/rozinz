#lang racket
(require racket/trace)
(require racket/contract)
(require errortrace)


#|
plan:
1. validate input
2. build list of inputs and outputs(with their function)
3. find repeating terms
|#

(define (assert condition message)
	(cond
	  ( (not condition)
		(error "assertion fail " message)
	  )
	)
)
(define (make-hash-2lists keys vals)
  (make-hash (map cons keys vals)) )


; mayhaps assoc? or hashmap!
; TODO: remove global state
(define *out-map* (make-hash))
(define *list-input* (list))
(define *set-output* (make-hash))
(define *list-flag* (list)) ; flags shall be defined by the program in some cases, when minimizing functions



;todo: function that spits out a boolvector for given number, instead of this monstrosity
(define boolvec-vector (vector
	(vector          #f) ;0 ;  0
	(vector          #t) ;1 ;  1
	(vector       #t #f)    ;  2
	(vector       #t #t) ;2 ;  3
	(vector    #t #f #f)    ;  4
	(vector    #t #f #t)    ;  5
	(vector    #t #t #f)    ;  6
	(vector    #t #t #t) ;3 ;  7
	(vector #t #f #f #f)    ;  8
	(vector #t #f #f #t)    ;  9
	(vector #t #f #t #f)    ; 10
	(vector #t #f #t #t)    ; 11
	(vector #t #t #f #f)    ; 12
	(vector #t #t #f #t)    ; 13
	(vector #t #t #t #f)    ; 14
	(vector #t #t #t #t) ;4 ; 15
))

(define boolvecvec_1 (vector
	(vector          #f) ;0 ;  0
	(vector          #t) ;1 ;  1
))

(define boolvecvec_2 (vector
	(vector          #f) ;0 ;  0
	(vector          #t) ;1 ;  1
	(vector       #t #f)    ;  2
	(vector       #t #t) ;2 ;  3
))

(define boolvecvec_3 (vector
	(vector          #f) ;0 ;  0
	(vector          #t) ;1 ;  1
	(vector       #t #f)    ;  2
	(vector       #t #t) ;2 ;  3
	(vector    #t #f #f)    ;  4
	(vector    #t #f #t)    ;  5
	(vector    #t #t #f)    ;  6
	(vector    #t #t #t) ;3 ;  7
))

(define boolvecvec_4 (vector
	(vector          #f) ;0 ;  0
	(vector          #t) ;1 ;  1
	(vector       #t #f)    ;  2
	(vector       #t #t) ;2 ;  3
	(vector    #t #f #f)    ;  4
	(vector    #t #f #t)    ;  5
	(vector    #t #t #f)    ;  6
	(vector    #t #t #t) ;3 ;  7
	(vector #t #f #f #f)    ;  8
	(vector #t #f #f #t)    ;  9
	(vector #t #f #t #f)    ; 10
	(vector #t #f #t #t)    ; 11
	(vector #t #t #f #f)    ; 12
	(vector #t #t #f #t)    ; 13
	(vector #t #t #t #f)    ; 14
	(vector #t #t #t #t) ;4 ; 15
))




(define (make-numlist n)
  (define (nlm n)
     (cond
       ((< n 0) '())
       (else (cons n (nlm (- n 1))
                   )
       )
     )
  )
  (reverse (nlm n))
)


(define (translate-in-tree  tree  hashmap)
  (cond
   ((list? tree)
    (map (lambda (t) (translate-in-tree t hashmap)) tree)
   )
   ((and
      (symbol? tree)
      (hash-has-key? hashmap tree ) )
    (hash-ref hashmap tree "ERROR")
   )
   (else tree)
  )
)
(translate-in-tree
  '(i1
     (i1 i2)
     i1
     (i1 (i2 i3)) )
  (make-hash-2lists '(i1 i2 i3) '(1 2 3)) )




(define (numerize-boolfun boolfun inputs-list) ;todo
  (define l (length inputs-list))
  (define inputs-map (hash inputs-list
						   (make-numlist (length inputs-list)) ))
	(translate-in-tree boolfun inputs-map)
	(error "todo (numerize-boolfun boolfun inputs-list)")
)
(define (number->boolvecvec n) ;todo
	(error "todo (number->boolvecvec n)")
)
(define (evaluate-numerized-boolfun boolfun boolvec) ;todo
	(error "todo (number->boolvecvec n)")
)
; TODO: truth table
(define (build-boolfunvector boolfun inputs-list)
;;boolfun: the tree which contains definition of boolean function
;;inputs-list: this will help in transforming inputs in boolfun into positions
	(define numerized (numerize-boolfun boolfun inputs-list))
	(define bvv (number->boolvecvec (count inputs-list)))
	(vector-map
	  (lambda (boolvec) (evaluate-numerized-boolfun numerized boolvec))
	  bvv
	)
)


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

(provide (contract-out
  [base-function?
	 (symbol? . -> .
			  boolean?) ]
))
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





(provide (contract-out
		   [gather-input (list? . -> . list?)]
		   [validate-input (list? . -> . list?)]
		   [valid-input-symbol? (symbol? . -> . boolean?)]
))



(define (gather-input input-info)
  ;; TODO: gather from a file or something, instead of being preprogrammed
  ;; gather from a file? for now, let's just spit out a bunch of ASTs
  '{
	(o1
	  (or  i1 i2 i3)
	)
	(o2
	  (and i1 i2 i3)
	)
	(o3
	  (and i1
		(or i1 i2 i3)
		i3)
	)
  }
)


(define (valid-boolvar-symbol? s)
	(define s (symbol->string s))
	(define s-1st (string-ref s 0))
	(define s-tail (substring s 1))
	(define first-correct?
	  (or
	    (eq? s-1st #\i)
	    (eq? s-1st #\o)
	    (eq? s-1st #\f) )
	)
	(define numbering-correct?
		(string->number s-tail)
	)
	(and
	  (first-correct?)
	  (numbering-correct?) )
)

(define (valid-input-symbol? s)
	(cond
		((member s list-base-functions)
		  s)
		
		(else #f)
	)
)


(define (validate-input in)
	(format #t "TODO: actually validate input in (validate-input)")
	;TODO: validate stuff
	in
)


(define (build-list-ins input)
	(remove-duplicates (flatten input))
)


(struct parsed-outfun (out-symbol boolfun ins truthtable))
(define (make-parsed-out definition)
;;definition is a list of length 2:
; 1. symbol? : designator of out
; 2. list? : tree, which is definition of boolean function
  (define out-symbol (list-ref definition 0))
  (define boolfun (list-ref definition 1))
  (define list-ins (build-list-ins boolfun))
  (parsed-outfun
	out-symbol ;out-symbol
	boolfun ;boolfun
	list-ins ;list-ins
	(build-boolfunvector boolfun list-ins) ;truthtable
))
(define (build-list-outs input)
  ;TODO
  (format #t "TODO: code for (build-list-outs ...)")
  input
)



(define (optimize-outs input)
  ;TODO
  (format #t "TODO: code for (optimize-outs ...)")
)




(define (main1)
	(define input-list (gather-input)) ;TODO: gather input
	(define valid-input (validate-input input-list))
	(define list-ins  (build-list-ins  valid-input))
	(define list-outs (build-list-outs valid-input))
	(define finished-product (optimize-outs list-outs list-ins))
	finished-product
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
