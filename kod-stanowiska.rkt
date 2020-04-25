#lang racket
;(provide (contract-out
;		   [gather-input (list? . -> . list?)]
;))

{define (gather-input input-info)
  (printf "Uwaga! input jest predefiniowany w funkcji (gather-input ...)")
;; TODO: gather from a file or something, instead of being preprogrammed
;; gather from a file? for now, let's just spit out a bunch of ASTs
'{
  [pracuj ;; pracuj tylko, kiedy są wszystkie surowce
	(and
	  start
	  cj-kubek
	  cj-koncentrat
	  cj-woda
	  (or
		cj-mleko
		(not w-mleko)
		)
	  (or
		cj-cukier
		(not w-cukier)
		)
	  )
	]
  [led-brak-woda
	(not cj-woda)
	]
  [led-brak-koncentrat
	(not cj-koncentrat)
	]
  [led-brak-kubek
	(not cj-kubek)
	]
  [led-brak-cukier
	(not cj-cukier)
	]
  [led-brak-mleko
	(not cj-mleko)
	]

  [zamknij-kubek
	(and
	  cj-kubek
	  pracuj
	  )
	]
  [grzej-woda
	(and
	  cj-kubek
	  cj-woda
	  )
	]
  [syp-woda
	(and
	  cj-kubek
	  cg-woda-zagrzana
	  )
	]
  [syp-koncentrat
	(and
	  cj-kubek
	  cg-woda
	  cj-koncentrat
	  )
	]
  [syp-cukier
	(and
	  cg-kubek
	  cg-woda
	  cg-koncentrat
	  w-cukier
	  cj-cukier
	  )
	]
  [syp-mleko
	(and
	  cg-kubek
	  cg-woda
	  cg-koncentrat
	  (and
		w-mleko
		cj-mleko
		(or
		  (not w-cukier)
		  (and
			w-cukier
			cg-cukier
			)
		  )
		)
	  )
	]

  [wszystko-gotowe
	(and
	  cg-kubek
	  cg-koncentrat
	  cg-woda-zagrzana
	  cg-woda-nalana
	  (or
		(not w-cukier)
		(and
		  w-cukier
		  cg-cukier )
		)
	  (or
		(not w-mleko)
		(and
		  w-mleko
		  cg-mleko )
		)
	  )
	]
  }
}
	;;







(provide (all-defined-out))
