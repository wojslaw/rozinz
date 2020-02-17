#lang racket
(require racket/trace)
(require racket/contract)
(require errortrace)

(define verbose? #t)
(define (if-verbose stuff)
  ;TODO make it work as expected
  ;trzeba by tu jakieś makro zrobić
  (cond ((eq? verbose? #t) stuff))
)
(cond (verbose?
	(printf "~%[[verbose on]]~%"))
)
#|
what I ended with:
1. managed to make work the part, which evaluates the boolfuns.
generating vectors of vectors of booleans works :3
numerizing boolfuns  works :3
evaluating the boolfun  works :3
|#


#|
plan:
1. validate input
2. build list of inputs and outputs(with their function)
3. find repeating terms
|#

#|
TODO
1. decouple taking boolfun-tree/outsymbol from definition


TODO
'global numerization'
1. create a 'global' list/vector of inputs
2. numerize functions with numbers according to this global list/vector
this should make the logic during optimization easier, because instead of looking for symbols, i can just compare numbers!



IDEA
'hold numerized boolfun'
for the sake of easier work, maybe outfun could hold a numerized boolean function tree?

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



;(define (number->boolvecvec n)
;; FIXME
;; currently it has 2 glaring issues that require fixing:
;; FIXED 1. goes 1 value lower than expected
;;	e.g. when supplying 4, it only goes up to #(#t #t), instead of up to #(#t #f #f)
;; 2. it doesn't create vectors which have sufficient 
;;	e.g. when supplying 4, the first vector is #(#f) instead of #(#f #f #f)
;	(define numbersequence  (list->vector (range (+ 1 n))))
;	(vector-map number->boolvec numbersequence)
;)


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


	(list->vector (map list->vector list-filled-booleanlists))
)




;;;; the following translation procedures could have been
;abstracted better: by having 1 (translate-in-tree ) procedure
;that translates depending on what you supply as a dictionary,
;be it vector or hashmap
(define (translate-in-tree  tree  hashmap)
;  (if-verbose (printf "(translate-in-tree ~A ~A) = " tree hashmap))
  (define result
      (cond
       ((list? tree)
        (map (lambda (t) (translate-in-tree t hashmap)) tree)
       )
       ((and
          (or (string? tree) (symbol? tree))
          (hash-has-key? hashmap tree) )
        (hash-ref hashmap tree "ERROR")
       )
       (else tree)
      )
  )
;  (if-verbose (printf "~A~%" result))
  result
)


(define (translate-in-indexed-tree-using-vector  tree  transvec)
;  (if-verbose (printf "(translate ~A  ~A)~%" tree transvec))
  (cond
   ((list? tree)
;    (if-verbose (display (printf "got list: ~A~%" tree)))
    (map (lambda (t) (translate-in-indexed-tree-using-vector t transvec))
         tree)
   )
    ((and
       (number? tree)
       (> (vector-length transvec) tree) ) ; todo: signal error if requested too high index
;     (if-verbose (printf "found index ~A in vector ~A ~%" tree transvec ))
     (vector-ref transvec tree)
    )
   (else tree) ;(translate-if-index tree transvec))
  )
)


{define list-base-functions
	'(
		!
		not ; IDEA should `not`|`!` be a special operator with special treatment? probably not, but maybe it would be better
		and
		or
		nor
		nand
		xor
	)
}

(define (valid-basic-bool-function? s)
  (list? (member s list-base-functions)) )

(define (valid-numerized-value? v)
  (or (valid-basic-bool-function? v)
	  (integer? v) ) )


(define (boolfun-numerized? bt)
;not numerized = (or (and i1 i2) i3)
;yes numerized = (or (and  1  2)  3)
	(cond
	  ((list? bt)
	   (andmap
		 boolfun-numerized?
		 bt) )
	  (else (valid-numerized-value? bt)) ) )


(define (make-hash-from-2-lists keys vals)
  (make-hash
	(map cons keys vals) ) )



(define (make-inputs-map-from-list  list-inputs)
  (define number-range (range (length list-inputs) ))
  (make-hash-from-2-lists
	list-inputs
	number-range ) )



(define (numerize-boolfun boolfun inputs-list) ;TODO - write tests
;   (if-verbose (printf "(numerize-boolfun~%  ~A~%  ~A )~%" boolfun inputs-list))
;  (cond ((not (boolfun-numerized? boolfun))
;		 (error "not numerized function! (numerize-boolfun  >>~A<<  ~A)~%" boolfun inputs-list)
;   ))
  (define l (length inputs-list))
  (define inputs-map (make-inputs-map-from-list inputs-list))
  (define numerized (translate-in-tree boolfun inputs-map))
;  (if-verbose (printf "numerized = ~A~%" numerized))
  numerized
;	(error "todo (numerize-boolfun boolfun inputs-list)")
)


; https://stackoverflow.com/questions/20778926/mysterious-racket-error-define-unbound-identifier-also-no-app-syntax-trans#20783438
(define my-eval
  (let ((ns (make-base-namespace)))
    (parameterize ((current-namespace ns))
      (namespace-require 'racket/bool))
    (lambda (expr) (eval expr ns))))


(define (evaluate-booltree bt)
;assumes that the tree has proper form
; that is: only lists beginning with a proper base function symbol, and everything else is a boolean
; TODO error checking
; problem I have here:
; this function receives input of the form 
;   '(and #t #t (or #t #f))
; now, how do I force the program to evaluate it? I just took (my-eval) from some stack-overflow thing
	(my-eval bt)
)


(define (evaluate-numerized-boolfun boolfun boolvec) ;TODO - write tests
  (define booltree (translate-in-indexed-tree-using-vector boolfun boolvec))
  (define evaluated-value (evaluate-booltree booltree))
  evaluated-value
)



(define (evaluate-numerized-boolfun-with-boolvecvec boolfun boolvecvec)
	(vector-map
	  (lambda (boolvec)
		(evaluate-numerized-boolfun boolfun boolvec) )
	  boolvecvec ) )



; DONE truthtable
(define (build-truthtable boolfun inputs-list)
;; boolfun: the tree which contains definition of boolean function
;; inputs-list: this will help in transforming inputs in boolfun into positions
;	(if-verbose (printf "~%(build-truthtable~%  ~A~%  ~A)~%" boolfun inputs-list))
	(define numerized (numerize-boolfun boolfun inputs-list))
;	(if-verbose (printf "numerized = ~A~%" numerized))
	(define bvv (generate-boolvecvec (length inputs-list)))
;	(if-verbose (printf "bvv = ~A~%" bvv))
	(vector-map
	  (lambda (boolvec) (evaluate-numerized-boolfun numerized boolvec))
	  bvv
	)
)





(define (function->list-variables f)
	(define flat (flatten f))
	(define varlist (filter flat variable?))
	(define cleaned (remove-duplicates varlist))
	(define sorted (sort cleaned))

	sorted
)







(define list-axioms ;TODO finish this list
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
  (printf "Uwaga! input jest predefiniowany w funkcji (gather-input ...)")
  ;; TODO: gather from a file or something, instead of being preprogrammed
  ;; gather from a file? for now, let's just spit out a bunch of ASTs
  '(
	(o1
		(and
			(or i1 i2)
			(or i3 i4)
		 )
	)
	(o2
	  (and
		(or  i4 i2 i8)
		(or  i1 i2 i3)
		(or
		  (and i1  i2  i3  i4)
		  (and i5  i6  i7  i8)
		  ;(and i9 i10 i11 i12)
		  )
	   )
	)
	(o3
	  (and
		i1
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
	;TODO: validate input
	; valid input will have the following properties:
	; 1. only lists and symbols allowed
	; 2. each list in the input must be a valid definition, that means:
	;	1. list has length = 2
	;	2. 1th value in definition shall be a symbol
	;	3. 2th value in definition shall be a valid boolean function, that means:
	;		1. boolean function is a tree: list of lists, each of those lists containing either symbols or lists
	;		2. 1th value in each list in boolean function is a valid basic-function symbol, that is `and` `or` `not` etc.
	in
)




(define (valid-input-string? s)
;; TODO make it less strict, that is allow any symbol that isn't a symbol of boolfun
  (and
	(string? s)
	(equal?
	  (string-ref s 0)
	  #\i )
	(string->number (substring s 1))
  )
)

(define (valid-input-symbol? s)
  (valid-input-string? (symbol->string s)) )

(define
  (sort-list-of-symbols-alphabetically
	list-of-symbols)
  (define (symbol-less? s0 s1)
	(string<?
	  (symbol->string s0)
	  (symbol->string s1) ) )
  (sort
	list-of-symbols
	symbol-less? )
 )

(define (build-list-ins input)
 ;; TODO make it, so this function
 ; 1. recursively traverses the boolfun tree
 ; 2. takes all symbols that aren't on the 0th position in list
 ; errors out upon:
 ; - encountering `(basic-function?)` on non-0th position
 ; - encountering `(not (basic-function? ...))` on 0th position
(printf "(build-list-ins ~A)~%" input)
(newline)
(newline)
  (sort-list-of-symbols-alphabetically
	(filter
	  (lambda (s) (not (valid-basic-bool-function? s)))
	  ;valid-input-symbol? ; 
	  (remove-duplicates (flatten input))) ) )




(struct outfun
  ( out-symbol
    boolfun
    list-inputs
    vector-vectors-bools
    truthtable )
  #:transparent
 )
(define (make-outfun-with-list-ins definition list-ins)
;; definition is a list of length 2:
; 1. symbol? : designator of out
; 2. list? : tree, which is definition of boolean function
;; list-ins is a list of any length, which has the symbols of inputs as values in list
  (define out-symbol (list-ref definition 0))
  (define boolfun (list-ref definition 1))
;  (if-verbose  (printf "list-ins = ~A~%" list-ins))
;  (if-verbose  (printf "(make-outfun ~A): (length list-ins) = ~A~%" definition (length list-ins)))
  (define boolvecvec (generate-boolvecvec (length list-ins)))
  (define truthtable (build-truthtable boolfun list-ins))
  (outfun
	out-symbol ;out-symbol
	boolfun ;boolfun
	list-ins ;list-ins
	boolvecvec
	truthtable  ;truthtable
))

(define (make-outfun definition)
;;definition is a list of length 2:
; 1. symbol? : designator of out
; 2. list? : tree, which is definition of boolean function
  (define out-symbol (list-ref definition 0))
  (define boolfun (list-ref definition 1))
  (define list-ins (build-list-ins boolfun))
  (make-outfun-with-list-ins definition list-ins)
 )




(define (outfun-print-truthtable outfun)
;; TODO print out a tab-separated list of values, to read conveniently in a spreadsheet program
;; TODO decouple, that is make a separate function instead of keeping it all as (outfun-print-truthable ... )
;; TODO format it properly instead of displaying the raw vectors:
; i1 i2 i3 o1
; ...
; #t #f #f #t
; #t #f #t #t
; ...
	(define tt-ins (outfun-vector-vectors-bools outfun))
	(define tt-outs (outfun-truthtable outfun))
	(define tt-header
	  (append
		(outfun-list-inputs outfun)
		(list (outfun-out-symbol outfun)) ) )
	(printf "(outfun-print-truthtable):~%~A~%~A~%~A~%~%" tt-ins tt-outs tt-header)
	(printf " ~A~%" tt-header)
	(for ((i tt-ins)
		  (o tt-outs) )
	  (printf "~A  ~A~%" i o)
	 )
)


;(define (format-outfun outfun)
;  
;)


(define (build-list-outs list-input)
;;;; this shall build a list of outputs, which have the structure defined by
; `(struct outfun ... )`
  (map make-outfun list-input)
)

(define (build-list-outs-globalized list-definitions list-input)
;;;; this shall build a list of outputs, which have the structure defined by
; `(struct outfun ... )`
'()
;  (map make-outfun-globalized list-definitions list-input)
)


(define (optimize-outs outs-list)
  ;TODO - this is the most important function where magic should happen xD
  (printf "TODO: code for (optimize-outs ...)")
)




(module+ main
	(define input-list (gather-input '())) ; TODO actually gather input from something, not have it prebaked.
	(define valid-input (validate-input input-list)) ; (validate-input) should ideally actually validate the input
	(printf "~%  valid-input: ~%")
	(display valid-input)
	(newline)

	(define (take-only-boolfuns deflist) ; TODO
	  (map
		(lambda (def) (list-ref def 1))
		deflist)
	  )
	(define list-ins  (build-list-ins  (map cdr valid-input))) ;; TODO (easy) take only boolfuns when building list of insymbols
	(printf "~%  list-ins: ~A~%" list-ins)
	(newline)

	(define list-outs (build-list-outs valid-input))
	(define (display-outs)
		(for ((o list-outs))
			(display o)
			(newline)
		 )
	 )

;(define outfun-def (list-ref valid-input 0))
;(display outfun-def)
;(newline)
;(define outfun (make-outfun outfun-def))
;(display outfun)
;(display (format-outfun outfun))
	(newline)
	(cond (verbose?
		(for ((o list-outs))
			(outfun-print-truthtable o)
		)
	))
;(outfun-print-truthtable (car list-outs))

;	(define list-outs (build-list-outs valid-input))
;	(printf "~%  list-outs:~%")
;	(display list-outs)
;	(newline)
;	(for
;	  ((out list-outs))
;	  (write out)
;	  (newline) )
;	(newline)

;	(define finished-product (optimize-outs list-outs))
;
;	(printf "~%finished product:~%")
;	(display finished-product)
;	(newline)



	(define list-globalized-outs
	  (build-list-outs-globalized
		valid-input
		list-ins) )
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



