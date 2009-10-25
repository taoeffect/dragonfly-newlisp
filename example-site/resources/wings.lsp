;; @author Greg Slepak

(DF:activate-plugin "artfulcode/json")

(context 'Resource.Wings)

(set 'my-data
  '((wings (left right))
	(wings-condition ("good" "excellent"))
	(wings-opacity 0.5))
)

(define (Resource.Wings:Resource.Wings response-format)
	; defaults to calling show
	(show response-format)
)

(define (show response-format)
	(if (= response-format "json")
		(begin
			(Response:content-type Response:json-type)
			(print (Json:lisp->json my-data))
		)
		(begin
			(Response:content-type Response:text-type)
			(print my-data)
		)
	)
)

(context MAIN)
