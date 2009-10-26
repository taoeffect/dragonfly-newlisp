;; @author Greg Slepak

(DF:activate-plugin "artfulcode/json")

(context 'Resource.Wings)

(set 'my-data
  '(
	(wings (left right))
	(wings-condition ("good" "excellent"))
	(wings-opacity 0.5)
   )
)

(define (Resource.Wings:Resource.Wings id response-format)
	; defaults to calling show
	(show id response-format)
)

(define (show id response-format)
	; unfortunately in this situation we can't use NL's default values to do this for us...
	(if-not id (set 'id 0))
	
	; uh-oh! No range checking on 'resource-id' ...
	(if (= response-format "json")
		(begin
			(Response:content-type Response:json-type)
			(print (Json:lisp->json (my-data id)))
		)
		(begin
			(Response:content-type Response:text-type)
			(print (my-data id))
		)
	)
)

(context MAIN)
