;; @author Greg Slepak

(DF:activate-plugin "artfulcode/json")

(new Resource 'Resource.Wings)
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

; findWing is an experiment to check if we really need a database abstraction layer
; my thoughts:
; define the resource functions like findWing and implement the necessary db connection in it
; common types of functions are findAll (SELECT * FROM _ WHERE 1), findOne (SELECT * FROM _ WHERE id = _)
; GET /findWing/1 would do something like SELECT * FROM wings where ID = 1
; GET /findAllWings would do something like SELECT * FROM wings where 1
;
; - Marc
;

(define (findWing id response-format)

	(DF:activate-plugin "sqlite3")

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
			(sql3:open "../databases/main.sqlite")
			;(set 'query (string "SELECT * FROM entries WHERE id='"id"'"))
			(set 'query (string "SELECT * FROM entries WHERE 1"))
			(print query "\r\n")
			(print (sql3:sql query))
			; hmm...doesn't return nil, maybe the path to db is wrong
			; there is a table entries - i know that!
		)
	)

)

(context MAIN)
