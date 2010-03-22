;; @module database_utils
;; @description Utilities for grabbing data out of a DF.DB
;; @version 1.0
;; @author Greg Slepak
;; <p>This file provides a convenience layer above the 'DF.DB' and 'DF.SQL' basic spec for fetching data.</p>
;; <p>All of the functions in this file are declared as global functions!</p>

(context MAIN)

;; @syntax (cast-if <fn-test> <to> <from>)
;; <p>Equivalent to: '(if (fn-test from) to from)'</p>
;; <p>This is a useful function when you want to ensure that certain "empty" values get
;; mapped to something else, for example if you want to make sure that you adds NULLs instead
;; of empty strings to a column you'd use it like this:</p>
;; <pre> (cast-if null? nil value)</pre>
(define (cast-if test to from)
	(if (test from) to from)
)
(global 'cast-if)

;; @syntax (for-query-with-db <ctx-db> <str-query> [<body...>])
;; <p>This macro is useful for writing templates, say for example to display a table of data, or simply
;; to iterate over a set of results. It takes an SQL query string and then a body.
;; All of the column names from the SQL result are available for use in the
;; body as uppercase labels, which are then substituted with their values.</p>
;; <p><b>example:</b></p>
;; <pre> &lt;table&gt;
;; 	&lt;tr class="header"&gt;&lt;td&gt;ID&lt;/td&gt;&lt;td&gt;Name&lt;/td&gt;&lt;td&gt;Age&lt;/td&gt;&lt;/tr&gt;
;; 	&lt;% (for-query-with-db db "SELECT rowid,name,age FROM people" %&gt;
;; 		&lt;tr&gt;
;; 			&lt;td&gt;&lt;%=ROWID%&gt;&lt;/td&gt;
;; 			&lt;td&gt;&lt;%=NAME%&gt;&lt;/td&gt;
;; 			&lt;td&gt;&lt;%=AGE%&gt;&lt;/td&gt;
;; 		&lt;/tr&gt;
;; 	&lt;% ) %&gt;
;; &lt;/table&gt;</pre>
;; <p>This function requires <b>newLISP version >= 10.1.11</b>.</p>
; define-smacro defined in utils.lsp (part of Dragonfly's core functions)
(define-smacro (for-query-with-db db query)
	(letn (ctx (prefix db) db (eval db) sql (db:prepare-sql (eval query)) keys '() values)
		(dotimes (i (sql:col-count))
			(push (sym (upper-case (sql:col-name i)) ctx) keys -1)
		)
		(push-autorelease-pool) ; in case we have blobs
		(while (list? (setf values (sql:next-row)))
			(eval (expand (cons 'begin $args) (unify keys values)))
		)
		(pop-autorelease-pool)
		(deallocate sql)
	)
)

;; @syntax (fn-query-with-db <ctx-db> <str-query> <func> [<list-params>])
;; <p>This function is similar to 'for-query-with-db' except that instead of taking
;; a body directly, you pass in a function 'func' that takes a single argument&mdash;the
;; results as an association list&mdash;which contains the body that will be executed
;; for each of the rows. Additionally, this function allows you to use queries safely
;; with a 'WHERE' clause by supplying parameters through 'list-params'.</p>
;; <p><b>example:</b></p>
;; <pre> &lt;table&gt;
;; 	&lt;tr class="header"&gt;&lt;td&gt;ID&lt;/td&gt;&lt;td&gt;Name&lt;/td&gt;&lt;td&gt;Age&lt;/td&gt;&lt;/tr&gt;
;; 	&lt;% (fn-query-with-db db "SELECT rowid,name,age FROM people WHERE age < ?" (fn (row) %&gt;
;; 		&lt;tr&gt;
;; 			&lt;td&gt;&lt;%=(&lt;- "rowid" row)%&gt;&lt;/td&gt;
;; 			&lt;td&gt;&lt;%=(&lt;- "name" row)%&gt;&lt;/td&gt;
;; 			&lt;td&gt;&lt;%=(&lt;- "age" row)%&gt;&lt;/td&gt;
;; 		&lt;/tr&gt;
;; 	&lt;% ) '(25)) %&gt;
;; &lt;/table&gt;</pre>
(define (fn-query-with-db db query func params , sql keys values)
	(when (setf sql (db:prepare-sql query))
		(when (or (not params) (sql:bind-params params))
			(setf keys (map sql:col-name (sequence 0 (-- (sql:col-count)))))
			(push-autorelease-pool) ; in case we have blobs
			(while (list? (setf values (sql:next-row)))
				(func (transpose (list keys values))))
			(pop-autorelease-pool)
		)
		(deallocate sql)
	)
)
(global 'fn-query-with-db)

;; @syntax (assoc-row-with-db <ctx-db> <str-query> [<list-params>])
;; @return An association list representing a single row where the keys are the column names and the values are the values for that column
;; @example
;; (assoc-row-with-db db "SELECT name,age FROM people WHERE age < ?" '(25))
;; ;=> (("name" "Sally") ("age" 12))
(define (assoc-row-with-db db query params , sql keys values result)
	(when (setf sql (db:prepare-sql query))
		(when (or (not params) (sql:bind-params params))
			(set 'keys (map sql:col-name (sequence 0 (-- (sql:col-count))))
			     'values (sql:next-row))
			(when (list? values)
				(setf result (transpose (list keys values))))
		)
		(deallocate sql)
		result
	)
)
(global 'assoc-row-with-db)

;; @syntax (assoc-rows-with-db <ctx-db> <str-query> [<list-params>])
;; <p>Like 'assoc-row-with-db' except returns multiple association lists for all the returned rows.</p>
(define (assoc-rows-with-db db query params , sql keys values rows)
	(when (setf sql (db:prepare-sql query))
		(when (and (or (not params) (sql:bind-params params))
		         (setf values (sql:next-row)))
			(set 'keys (map sql:col-name (sequence 0 (-- (sql:col-count))))
			     'rows '())
			(while (list? values)
				(push (transpose (list keys values)) rows -1)
				(setf values (sql:next-row))
			)
		)
		(deallocate sql)
		rows
	)
)
(global 'assoc-rows-with-db)

;; @syntax (query-cell-with-db <ctx-db> <str-query> [<list-params>])
;; @return The exact value at a specific row/column or 'nil' if not found.
(define (query-cell-with-db db query params , sql result)
	(when (setf sql (db:prepare-sql query))
		(when (or (not params) (sql:bind-params params))
			(setf result (sql:next-row))
			(if (list? result)
				(setf result (first result))
				(setf result nil) ; next-row returns true
			)
		)
		(deallocate sql)
		result
	)
)
(global 'query-cell-with-db)
