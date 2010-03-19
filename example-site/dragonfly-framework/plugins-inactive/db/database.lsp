;; @module database
;; @description Generic database access interface for <a href="http://www.rundragonfly.com">Dragonfly</a> using Objective newLISP
;; @version 1.2
;; @author Greg Slepak
;; @location http://www.taoeffect.com/newlisp/database.lsp.txt
;; <p>The purpose of this module is to standardize the interface to
;; access databases. This will allow you, for the most part, to write your code once
;; and easily switch the type of database that you're using.</p>
;; <p>To accomplish this, the interface introduces three Objective newLISP classes:
;; 'DF.DB', 'DF.SQL', and 'DF.BLOB'.</p>
;; <h3>DF.DB</h3>
;; A 'DF.DB' object represents a database connection. Using it you connect to
;; the database and execute SQL queries.
;; <h3>DF.SQL</h3>
;; A 'DF.SQL' object is a wrapper around an SQL statement, is retrieved through one of
;; two functions: 'DF.DB:execute-query' and the lower-level 'DF.DB:preprare-sql'.
;; <p>It is used to retrieve rows from the result set of a query one-by-one.</p>
;; <h3>DF.BLOB</h3>
;; 'DF.BLOB' is used to insert and retrieve (possibly large) binary data into databases.
;; It is needed for two reasons:
;; <ol>
;; <li>newLISP uses strings to buffer and store binary data, and that's already used to store text.</li>
;; <li>BLOBs can be very large, so by storing them in a context we avoid excessive copying.</li>
;; </ol>
;; <p>Unlike the other two classes, 'DF.BLOB' provides a basic working implementation for 'DF.SQL' subclasses
;; to use. You may of course subclass it if your database requires additional functionality. It requires
;; special usage considerations, see its documentation below.</p>
;; <h3>Example</h3>
;; <pre>
;; (push-autorelease-pool) ; we're going to be using DF.BLOB's.
;; (setf db (instantiate Sqlite3 ":memory:"))
;; (if-not db (throw-error "couldn't open db"))
;; (db:execute-update "CREATE TABLE fish (id INTEGER PRIMARY KEY, name TEXT, weight REAL, blah BLOB)")
;; (db:execute-update "INSERT INTO fish (name,weight) VALUES (?,?)" '("flipper" 234.123))
;; (db:execute-update "INSERT INTO fish (name,weight) VALUES (?1,?2)" '(("?1" "catfish") ("?2" 100.3)))
;; (db:execute-update "INSERT INTO fish (blah) VALUES (?)" (list (DF.BLOB (dup "\000" 10))))
;; (db:execute-update "INSERT INTO fish (blah) VALUES (:cat)" (list (list ":cat" (DF.BLOB (dup "\000" 10)))))
;; (setf sql (db:execute-query "SELECT * FROM fish"))
;; (do-while (list? row)
;;     (push-autorelease-pool) ; "in case" we end up fetching a lot of large blobs
;;     (setf row (sql:next-row))
;;     (println "row: " row)
;;     (pop-autorelease-pool)
;; )
;; (deallocate sql)
;; (deallocate db)
;; (pop-autorelease-pool) ; deallocate the blobs we created</pre>
;; <h3>Requirements</h3>
;; <ul>
;; <li>Dragonfly newLISP Web Framework (see note below)</li>
;; <li>newLISP 10.1.9 or higher is <b>strongly recommended</b>, but any version after 10.1 should work.</li>
;; <li>Objective newLISP 1.0</li>
;; <li>Libraries for a supported database</li>
;; </ul><br/>
;; @link http://www.rundragonfly.com Dragonfly is only required for its logging functions. You can easily implement your own
;; versions of 'DF:log-err', 'DF:log-debug', and the other functions found in Dragonfly&apos;s 'log.lsp'.
;; <h3>Version history</h3>
;; <b>1.2</b> &bull; added 'DF.SQL:col-name' and 'DF.SQL:col-count' to specification
;; <b>1.1</b> &bull; 'DF.BLOB' added<br/>
;; <b>1.0</b> &bull; initial release

(constant (global 'NEWLISP64) (not (zero? (& (sys-info -1) 256))))
; cache the function for getting a pointer
(constant (global 'get-ptr) (if NEWLISP64 get-long get-int))
; used to indicate that a method *must* be overwritten
(constant (global 'throw-not-implemented) (fn()(throw-error "not defined by subclass!")))

(new-class 'DF.DB)
(new-class 'DF.SQL)
(new-class 'DF.BLOB)

; NOTE: all functions here are context-qualified because
;       newLISP will complain if there already exists a function
;       of that name in the MAIN context.

;---------------------------------------------------------------
; !DF.DB
;---------------------------------------------------------------

;; @syntax DF.DB
;; <p>Represents a database connection. You create a DF.DB object
;; like so:</p>
;; <pre>(instantiate <DF.DB-Subclass> [<connection> [<args...>]])</pre>
;; <p>What type should be represented by <connection> is undefined, but
;; it&apos;s recommended that subclasses use strings. If <connection> is specified
;; the a connected instance must be returned (or 'nil' upon failure).</p>
;; <p>The possibly optional <args...> are specific to which subclass you're using. See its
;; corresponding documentation.</p>
;; <p>Instances must have code in their 'dealloc' method so that they can properly cleanup their resources
;; (e.g. shutdown connection) if deallocated with 'deallocate'.</p>
(context DF.DB)

(define (DF.DB:DF.DB connection)
	(throw-not-implemented)
)

;; @syntax (DF.DB:open <connection>)
;; <p>Returns 'true' if the connection was opened successfully, 'nil'
;; if there was an error opening the connection, or throws an exception
;; if already connected.</p>
(define (DF.DB:open connection)
	(throw-not-implemented)
)

;; @syntax (DF.DB:close)
;; <p>Returns 'true' if the connection was closed successfully or was
;; already closed, or 'nil' if there was an error closing the connection.</p>
(define (DF.DB:close)
	(throw-not-implemented)
)

;; @syntax (DF.DB:prepare-sql <str-sql>)
;; @param <str-sql> A single SQL statement. Does not need to end in ';'
;; <p>Returns a 'DF.SQL' object upon success, 'nil' on failure, or throws
;; an exception if not connected.</p>
;; <p><b>important:</b> If your SQL statement contains placeholders (to be
;; bound later using 'DF.SQL:bind-params') you may not mix and match placeholder styles!
;; Pick one placeholder style and stick with it for the entire statement.</p>
(define (DF.DB:prepare-sql sql)
	(throw-not-implemented)
)

;; @syntax (DF.DB:execute-update <str-sql> [<list-params>])
;; @param <str-sql> A single SQL statement. Does not need to end in ';'
;; @param <list-params> A list of parameters to bind to a parameterized query
;; <p>Same as 'DF.DB:execute-query' but returns 'true' instead of a 'DF.SQL' object upon success.
;; Useful for SQL such as "UPDATE" and "INSERT".</p>
(define (DF.DB:execute-update sql params)
	(throw-not-implemented)
)

;; @syntax (DF.DB:execute-query <str-sql> [<list-params>])
;; @param <str-sql> A single SQL statement. Does not need to end in ';'
;; @param <list-params> A list of parameters to bind to a parameterized query
;; <p>A short-hand for 'DF.DB:prepare-sql' and 'DF.SQL:bind-params'. Returns
;; a 'DF.SQL' object upon success, 'nil' on failure, or throws an exception if
;; not connected.</p>
;; <p><b>see:</b> documentation for 'DF.SQL:bind-params' for more info on <list-params>.
(define (DF.DB:execute-query sql params)
	(throw-not-implemented)
)

;; @syntax (DF.DB:rows-for-query <str-sql> [<list-params>])
;; <p>Same as 'DF.DB:execute-query' but retrieves all of the rows and returns them as a list of results.</p>
;; <p><b>important:</b> If any columns contain BLOB types, you <b><i>must</i></b> have
;; an autorelease pool allocated prior to calling this function!</p>
(define (DF.DB:rows-for-query sql params)
	(throw-not-implemented)
)

;; @syntax (DF.DB:rowid)
;; <p>Returns the row id for the last row that was inserted or throws an
;; exception if not connected.</p>
(define (DF.DB:rowid)
	(throw-not-implemented)
)

;; @syntax (DF.DB:changes)
;; <p>Returns how many rows were affected by the last INSERT/UPDATE, or throws
;; an exception of not connected</p>
(define (DF.DB:changes)
	(throw-not-implemented)
)

;; @syntax (DF.DB:version)
;; <p>Returns the version number of the database library being used as an integer.</p>
(define (DF.DB:version)
	(throw-not-implemented)
)

;; @syntax (DF.DB:table-exists? <table-name>)
;; <p>Returns nil or non-nil depending on whether the table named 'table-name' exists,
;; or throws an exception if not connected.</p>
(define (DF.DB:table-exists? table-name)
	(throw-not-implemented)
)

;; @syntax (DF.DB:connected?)
;; @return nil or non-nil depending on whether this 'DF.DB' object has an active connection.
(define (DF.DB:connected?)
	(throw-not-implemented)
)

;; @syntax (DF.DB:last-error)
;; @return a list of two elements: the most recent error code and a description string.
(define (DF.DB:last-error)
	(throw-not-implemented)
)

;---------------------------------------------------------------
; !DF.SQL
;---------------------------------------------------------------

;; @syntax DF.SQL
;; <p>Represents a prepared statement. It is used to bind values to a statement's parameters
;; and retrieve the results of a query.</p>
;; <p>You do not create a DF.SQL instance yourself but obtain one through 'DF.DB:prepare-sql'.
;; However, if you've obtained an instance then you are responsible for freeing its memory
;; and closing its connection when you're finished with it.</p>
;; <p>Subclasses should make sure that their 'dealloc' method calls 'DF.SQL:close' so that
;; statements can be freed and closed in one function call using 'deallocate'.</p>
(context DF.SQL)

;; @syntax (DF.SQL:bind-params <list-params>)
;; <p>Binds the placeholders of this SQL statement to the values in <list-params>.</p>
;; <p><list-params> may be simply a list of values, or an association list of key/value
;; pairs, depending on the placeholder pattern used in the SQL. Placeholder styles
;; <b>may not</b> be mixed per SQL statement.</p>
;; <p>Values may be any of newLISP's primitives. To specify a BLOB wrap your string
;; in a 'DF.BLOB':</p>
;; <b>example:</b>
;; <pre>
;; ; "weight" is a REAL and "blah" is a BLOB
;; (push-autorelease-pool) ; create our autorelease pool since we're using DF.BLOB
;; (setf sql (db:prepare-sql "INSERT INTO fish (weight,blah) VALUES (?,?)"))
;; (sql:bind-params (list 1043.3 (DF.BLOB (dup "\000" 5))))
;; (println (sql:next-row))
;; (deallocate sql)
;; (pop-autorelease-pool)
;; </pre>
(define (DF.SQL:bind-params params)
	(throw-not-implemented)
)

;; @syntax (DF.SQL:next-row)
;; <p>Returns a row as a list from the result set of a statement, 'true' if
;; there are no more rows to return, or 'nil' if there was an error.</p>
;; <b>example:</b>
;; <pre> (push-autorelease-pool) ; only necessary if there's a BLOB column
;; (while (list? (setf row (sql:next-row)))
;; 	(println "row: " row)
;; )
;; (pop-autorelease-pool) ; free any blobs</pre>
;; <p><b>important:</b> If any columns contain BLOB types, you <b><i>must</i></b> have
;; an autorelease pool allocated prior to calling this function!</p>
(define (DF.SQL:next-row)
	(throw-not-implemented)
)

;; @syntax (DF.SQL:reset)
;; <p>Resets the statement so that it can be re-used without preparing another
;; statement (which is more efficient). After reseting a statement you typically
;; use 'DF.SQL:bind-params' to bind new values and then 'DF.SQL:next-row'.</p>
(define (DF.SQL:reset)
	(throw-not-implemented)
)

(define (DF.SQL:col-name col-num)
	(throw-not-implemented)
)

(define (DF.SQL:col-count)
	(throw-not-implemented)
)

;; @syntax (DF.SQL:close)
;; <p>Releases the resources used by the SQL statement represented by this object.
;; You cannot use it anymore after calling this method, the only thing left to do
;; is to 'deallocate' it, and since sublasses of 'DF.SQL' must call 'close' in
;; their 'dealloc' methods, it&apos;s often simpler to call 'deallocate' on the object instead.</p>
;; <p>Returns 'true' upon success, 'nil' on failure or if already closed.</p>
(define (DF.SQL:close)
	(throw-not-implemented)
)

;---------------------------------------------------------------
; !DF.BLOB
;---------------------------------------------------------------

(context DF.BLOB)

;; @syntax (DF.BLOB:DF.BLOB <str-blob>)
;; <p>An object wrapper around a blob of data represented by <str-blob>, used
;; for efficient passing of possibly large binary data.</p>
;; <p>Unlike most other 'ObjNL' classes, you typically create an instance
;; by simply calling its constructor instead of calling 'instantiate' yourself.
;; It will then 'instantiate' and 'autorelease' a 'DF.BLOB' instance containing the data
;; in 'DF.BLOB:blob'.</p>
;; <p><b>important:</b> An autorelease pool <b>must</b> be in place when using the
;; constructor to instantiate a blob!</p>
;; <p><b>see:</b> the introduction to this document for example usage.</p>
(define (DF.BLOB:DF.BLOB _blob)
	(if (= @self @class)
		(autorelease (instantiate @class _blob))
		(begin (setf blob _blob) true)
	)
)

(context MAIN)
