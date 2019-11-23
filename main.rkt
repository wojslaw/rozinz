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


; TODO: stringify symbols on input, and fix the avalanche that will ensue

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



;;;; the following translation procedures could have been
;abstracted better: by having 1 (translate-in-tree ) procedure
;that translates depending on what you supply as a dictionary,
;be it vector or hashmap
(define (translate-in-tree  tree  hashmap)
  (cond
   ((list? tree)
    (map (lambda (t) (translate-in-tree t hashmap)) tree)
   )
   ((and
      (string? tree)
      (hash-has-key? hashmap tree) )
    (hash-ref hashmap tree "ERROR")
   )
   (else tree)
  )
)


(define (translate-in-indexed-tree-using-vector  tree  transvec)
;  (printf "(translate ~A  ~A)~%" tree transvec)
  (cond
   ((list? tree)
;    (display (printf got list: ~A~%" tree))
    (map (lambda (t) (translate-in-indexed-tree-using-vector t transvec))
         tree)
   )
    ((and
       (number? tree)
       (> (vector-length transvec) tree) ) ; todo: signal error if requested too high index
;     (display (printf found index ~A in vector ~A ~%" tree transvec ))
     (vector-ref transvec tree)
    )
   (else tree) ;(translate-if-index tree transvec))
  )
)





(define (numerize-boolfun boolfun inputs-list) ;todo
  (define l (length inputs-list))
  (define inputs-map (hash inputs-list
						   (make-numlist (length inputs-list)) ))
	(translate-in-tree boolfun inputs-map)
;	(error "todo (numerize-boolfun boolfun inputs-list)")
)
(define (number->boolvecvec n) ;todo
	(error "todo (number->boolvecvec n)")
)

(define (evaluate-base-boolean-function fun arglist)
	'()
;	(case
;		
;		(else (error "unexpected "))
;	)
)

(define (evaluate-booltree bt)
;assumes that the tree has proper form
; that is: only lists beginning with a proper base function symbol, and everything else is a boolean
; todo: error checking
	(cond
	  ((and (list? bt) (string? (car bt)))
	   (evaluate-base-boolean-function (car bt) (cdr bt)) )
	  (else bt)
	)
)

(define (evaluate-numerized-boolfun boolfun boolvec) ;todo
  (define booltree (translate-in-indexed-tree-using-vector boolfun boolvec))
  (printf "made booltre~%from: ~A~%     ~A~% to: ~A~%" boolfun boolvec booltree)
  (evaluate-booltree booltree)
  ;(eval 
)
(printf "the following should be #t: ")
;(and #t #t)
;(eval (list 'and #t #t))
(evaluate-numerized-boolfun
  '(and 0 1 (or 1 2))
  (list->vector (list #t #t #f)) )


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


(define (number->vector-bool n)  ; TODO
  (error "todo number->vector-bool")
	(cond [(not (integer? n)) (error "not integer:" n)] )
	(define vector-bool '())

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



(define list-axioms ;todo: finish this list
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
		(equal? f1 f2)
	  ]
    )
)

(provide (contract-out
  [base-function?
	 (string? . -> .
			  boolean?) ]
))
(define (base-function? x)
	(and
	  (string? x)
	  (member x list-base-functions)
	)
)


(define (variable? x)
	(and
		(string? x)
		(not (base-function? x))
	)
)

(define (valid-function? x)
	(error "unfinished")
)





(provide (contract-out
		   [gather-input (list? . -> . list?)]
		   [validate-input (list? . -> . list?)]
		  ; [valid-input-symbol? (symbol? . -> . boolean?)]
))



(define (gather-input input-info)
  ;; TODO: gather from a file or something, instead of being preprogrammed
  ;; gather from a file? for now, let's just spit out a bunch of ASTs
  '(
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
  )
)


(define (valid-boolvar-string? s)
	(define s-1st (string-ref s 0))
	(define s-tail (substring s 1))
	(define first-correct?
	  (or
	    (equal? s-1st #\i)
	    (equal? s-1st #\o)
	    (equal? s-1st #\f) )
	)
	(define numbering-correct?
		(string->number s-tail)
	)
	(and
	  (first-correct?)
	  (numbering-correct?) )
)





(define (stringify-recursively t)
  (cond
	((symbol? t)
	 (symbol->string t)
	)
	((string? t)
	 t
	)
	((list? t)
	 (map stringify-recursively t)
	)
	(else (error "unexpected input while stringify-recursively" t))
	)
)


(define (validate-input in)
	(printf "TODO: actually validate input in (validate-input)~%")
	;TODO: validate stuff
	(stringify-recursively in)
)




(define (valid-input-string? s)
  (and
	(string? s)
	(equal?
	  (string-ref s 0)
	  #\i )
	(string->number (substring s 1))
  )
)


(define (build-list-ins input)
; TODO: remove non-input tokens
;	(printf "~%(build-list-ins ~A)~%" input)
;	(printf "~%flattened, deduped ~A~%" (remove-duplicates (flatten input)))
	(filter
	  valid-input-string? ; valid-input-symbol?
	  (remove-duplicates (flatten input))) )



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
  (printf "TODO: code for (build-list-outs ...)~%")
  input
)



(define (optimize-outs outs-list)
  ;TODO
  (printf "TODO: code for (optimize-outs ...)")
)




(module+ main
	(define input-list (gather-input '())) ;TODO: gather input

	(define valid-input (validate-input input-list)) ;now we are working with strings

	(define list-ins  (build-list-ins  valid-input))
	(define list-outs (build-list-outs valid-input))
	(define finished-product (optimize-outs list-outs))

	(printf "~%  valid-input:~A~%"
			valid-input)
	(printf "~%  list-ins:~A~%"
			list-ins )
	(printf "~%  list-outs:~A~% "
			list-outs )
	(printf "~%finished product: ~A~%"
			finished-product )
)





;; tests
;
(module+ test-translate-in-tree
	(translate-in-tree
	  '(i1
		 (i1 i2)
		 i1
		 (i1 (i2 i3)) )
	  (make-hash-2lists '(i1 i2 i3) '(1 2 3)) )
)



;(test-add-out-collision)
