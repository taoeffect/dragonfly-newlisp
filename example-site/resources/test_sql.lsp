; findWing is an experiment to check if we really need a database abstraction layer
; my thoughts:
; define the resource functions like findWing and implement the necessary db connection in it
; common types of functions are findAll (SELECT * FROM _ WHERE 1), findOne (SELECT * FROM _ WHERE id = _)
; GET /findWing/1 would do something like SELECT * FROM wings where ID = 1
; GET /findAllWings would do something like SELECT * FROM wings where 1
;
; - Marc
;

(DF:activate-plugin "artfulcode/json" "db/database_sqlite3")

(new Resource 'Resource.TestSql)
(context 'Resource.TestSql)

(set 'my-data
  '(
	(wings (left right))
	(wings-condition ("good" "excellent"))
	(wings-opacity 0.5)
   )
)

(define (Resource.TestSql:Resource.TestSql id response-format)
	(findWing id response-format)
)

(define (findWing id response-format)

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
			(let (db (instantiate Sqlite3 "databases/main.sqlite"))
				(set 'query (string "SELECT * FROM entries"))
				(print query "\r\n")
				(print (db:rows-for-query query))
				(deallocate db)
			)
		)
	)

)

(context MAIN)
