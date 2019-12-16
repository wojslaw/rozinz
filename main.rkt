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


(define (number->binary n)
	(cond (not (integer? n)) (error "not integer when trying to make binary representation" n))
	(format "~b" n)
)

(define (digit->bool n)
    (cond
      ((equal? n 0) #f)
      ((equal? n #\0) #f)
      ((equal? n "0") #f)
      ((equal? n 1) #t)
      ((equal? n #\1) #t)
      ((equal? n "1") #t)
	  (else (error "unexpected" n))
    )
)

(define (number->boolvec n)
	;(newline)
	(define binary-string (number->binary n))
	;(display binary-string)
	;(newline)
	(define list-characters (string->list binary-string))
	;(display list-characters)
	;(newline)
	(define list-booleans (map digit->bool list-characters))
	;(display list-booleans)
	;(newline)
	(list->vector list-booleans)
)

;(printf "~%    6 = ~A~%" (number->boolvec 6))
;(printf "   13 = ~A~%" (number->boolvec 13))
;(printf "   15 = ~A~%" (number->boolvec 15))


(define (make-number-sequence n) ; (0 1 2 3 4 ... n)
  (define (recurse-ascend-number seq n)
    (cond
      ((equal? seq null)
       (recurse-ascend-number
         (cons n null)
         (- n 1)
       )
      )
      ((> (car seq) 0)
        (cons (recurse-ascend-number seq (- n 1)))
        seq
      )
      (else
          seq
      )
    )
  )
  (recurse-ascend-number null n)
)



(define (number->boolvecvec n)
; FIXME
; currently it has 2 glaring issues that require fixing:
; FIXED 1. goes 1 value lower than expected
;	e.g. when supplying 4, it only goes up to #(#t #t), instead of up to #(#t #f #f)
; 2. it doesn't create vectors which have sufficient 
;	e.g. when supplying 4, the first vector is #(#f) instead of #(#f #f #f)
	(define numbersequence  (list->vector (range (+ 1 n))))
	(vector-map number->boolvec numbersequence)
)


(define (generate-boolvecvec veclen)
;;;; generate a vector of vectors of boolean, up to `n` values
;;;; e.g. n=3 will generate #(#(#f #f #f) #(#f #f #t) ... #(#t #t #t))
	(cond
	  ((equal? veclen 0)
		(error "must be integer greater than 0"))
	)
	
	(define number-list (range (expt 2  veclen ))) ; `(integer? (expt 2 64))` returns `#t`, so should be okay
	(define string-list (map (lambda (n) (format "~b" n)) number-list))
	(define list-charlists
	  (map
		string->list
		string-list ) )
	(define list-booleanlists
		(map
		  (lambda (cl)
			(map digit->bool cl)
		  )
		  list-charlists) )


	(define (prepend-#f-to-list desired-length bool-list)
		(cond
		  ( (= desired-length (length bool-list))
			bool-list
		  )
		  (else
			(prepend-#f-to-list desired-length (cons #f bool-list))
		  )
		)
	)
	
	(define list-filled-booleanlists
	  (map
	    (lambda (bl) (prepend-#f-to-list veclen  bl))
		list-booleanlists ) )


	list-filled-booleanlists
)

;(printf "~ngenerate boolvecvec 1:~n")
;(print (generate-boolvecvec 1))
;(printf "~ngenerate boolvecvec 2:~n")
;(print (generate-boolvecvec 2))
;(printf "~ngenerate boolvecvec 3:~n")
;(print (generate-boolvecvec 3))
;(printf "~ngenerate boolvecvec 4:~n")
;(print (generate-boolvecvec 4))




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
  (printf "(evaluate-numerized-boolfun ~%  ~A~%  ~A~%) = ~A~%" boolfun boolvec booltree)
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
(define (build-truthtable boolfun inputs-list)
;; boolfun: the tree which contains definition of boolean function
;; inputs-list: this will help in transforming inputs in boolfun into positions
	(define numerized (numerize-boolfun boolfun inputs-list))
	(define bvv (number->boolvecvec (length inputs-list)))
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



(struct outfun
  ( out-symbol
    boolfun
    list-inputs
    truthtable ) )
(define (make-outfun definition)
;;definition is a list of length 2:
; 1. symbol? : designator of out
; 2. list? : tree, which is definition of boolean function
  (define out-symbol (list-ref definition 0))
  (define boolfun (list-ref definition 1))
  (define list-ins (build-list-ins boolfun))
  (outfun
	out-symbol ;out-symbol
	boolfun ;boolfun
	list-ins ;list-ins
	(build-truthtable boolfun list-ins) ;truthtable
))
(define (build-list-outs input)
;;;; this shall build a list of outputs, which have the structure defined by
; `(struct outfun ... )`
;
  ;TODO
  (printf "TODO: code for (build-list-outs ...)~%")
  (map make-outfun input)
)



(define (optimize-outs outs-list)
  ;TODO
  (printf "TODO: code for (optimize-outs ...)")
)




(module+ main
	(define input-list (gather-input '())) ;TODO: gather input
	(define valid-input (validate-input input-list)) ;now we are working with strings
	(printf "~%  valid-input: ~%")
	(display valid-input)
	(newline)

	(define list-ins  (build-list-ins  valid-input))
	(printf "~%  list-ins:~%")
	(display list-ins)
	(newline)

	(define list-outs (build-list-outs valid-input))
	(printf "~%  list-outs:~%")
	(for
	  ((out list-outs))
	  (write out)
	  (newline) )
	(newline)

	(define finished-product (optimize-outs list-outs))

	(printf "~%finished product:~%")
	(display finished-product)
	(newline)
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
