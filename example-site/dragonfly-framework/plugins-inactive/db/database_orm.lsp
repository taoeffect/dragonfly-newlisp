;; @module database_orm
;; @description DB.OBJ - Simple ORM class for DF.DB
;; @version 1.0
;; @author Greg Slepak
;; <p>'DB.OBJ' provides very basic Object-relational mapping (ORM) for 'DF.DB'. Specifically,
;; each 'DB.OBJ' object has accessors to manipulate or retrieve the values for some or all of the
;; columns of a specific row.</p>
;; <p>All of the functions documented here are declared global!</p>
;; <h3>Obtaining a 'DF.OBJ' object</h3>
;; There are currently three functions for obtaining an object: 'create-dbobj', 'find-dbobj' and 'find-or-create-dbobj'.
;; <p>To obtain an object, one of the arguments that must be specified is called the <finder>.</p>
;; <p>The <finder> can either be multiple things:</p>
;; <ol><li>An <b>integer</b> - in which case it is assumed to refer to the exact ROWID of the row you want.</li>
;; <li>A <b>string</b> - in which case it is greated as the argument to the WHERE clause of an SQL statement. Be careful of SQL injection attacks when choosing this method, it may be preferable to instead use...</li>
;; <li>An <b>association list</b> - For example, to ask for the row(s) where the 'name' is "Greg" and 'age' is 12, we&apos;d pass: '&apos;(("name" "Greg") ("age" 12))'</li>
;; </ol>
;; <p>In order to specify what attributes the object will have, a list is passed in containing (as strings) the desired columns.
;; Because this list simply contains portions of the SQL, '&apos;("*")' may be specified to indicate all columns.</p>
;; <h3>Manipulating values</h3>
;; <p>Once you've obtained an object you fetch its values by simply calling the appropriate method for that value.
;; The name of this method will be based upon the column it corresponds to (it can be altered using SQL aliases). The columns are alternatively called the object&apos;s "attributes" or "keys."</p>
;; <pre> (println "Person who is named: " (*obj*:name))
;; ;=> "John Doe"</pre>
;; <p>To modify a value simply pass in a value as the second argument:</p>
;; <pre> (*obj*:name "Greg Slepak")
;; (println "Person is now named: " (*obj*:name))
;; ;=> "Greg Slepak"</pre>
;; <p>It&apos;s important to note though that the values <b>aren&apos;t saved to the database until you call 'dbobj-save' on the object!</b></p>
;; <pre> (dbobj-save *obj*)</pre>
;; <p>Querying a value from an object returns the latest value, even if it's not saved. To ask for the value prior to modification
;; pass 'true' in as the third argument:</p>
;; <pre> (*obj*:name "John Doe") ; change it back
;; (println "Original value: " (*obj*:name nil true))
;; ;=> "Greg Slepak"</pre>
;; <h3>Objective newLISP</h3>
;; <p>The objects returned by these functions are ObjNL objects and thus must be properly memory managed using the conventions
;; in ObjNL to avoid memory leaks. See the 'release', 'retain' and 'autorelease' functions in the ObjNL documentation for more detail.</p>
;; <h3>Version history</h3>
;; <b>1.0</b> &bull; initial release

(DF:activate-plugin "db/database_utils")

(new-class 'DB.OBJ)

; TODO: transform all of these into an object-based system of transformations (i.e. Sqlite3Adapter, etc.)
(set (global 'DBOBJ_SELECT_SQL)   (or DBOBJ_SELECT_SQL   "SELECT %s FROM %s WHERE %s LIMIT %d")
     (global 'DBOBJ_SELECT_SQL2)  (or DBOBJ_SELECT_SQL2  "SELECT * FROM %s") ; b/c INSERT doesn't tell us. we don't actually retrieve the rows.
     (global 'DBOBJ_UPDATE_SQL)   (or DBOBJ_UPDATE_SQL   "UPDATE %s SET %s=? WHERE %s")
     (global 'DBOBJ_INSERT_SQL)   (or DBOBJ_INSERT_SQL   "INSERT INTO %s (%s) VALUES (%s)")
     (global 'DBOBJ_INSERT_SQL2)  (or DBOBJ_INSERT_SQL2  "INSERT INTO %s VALUES (%s)")
	 (global 'DBOBJ_DELETE_SQL)   (or DBOBJ_DELETE_SQL   "DELETE FROM %s WHERE %s")
	 (global 'DBOBJ_ROWID)        (or DBOBJ_ROWID        "ROWID")
	 (global 'DBOBJ_ROWID_ATTR)   (or DBOBJ_ROWID_ATTR   "id")
     (global 'DBOBJ_ROWID_COL)    (or DBOBJ_ROWID_COL    (string DBOBJ_ROWID " as " DBOBJ_ROWID_ATTR))
	 (global 'DBOBJ_ROWID_FINDER) (or DBOBJ_ROWID_FINDER (string DBOBJ_ROWID "="))
	 (global 'DBOBJ_WHERE_COMB)   (or DBOBJ_WHERE_COMB   " AND ")
)

;---------------------------------------------------------------
; !Getting DF.OBJs
;---------------------------------------------------------------

;; @syntax (create-dbobj <ctx-db> <str-table> <list-data>)
;; @param <ctx-db> A 'DF.DB' instance
;; @param <str-table> The table in which this "object" will be created in
;; @param <list-data> Either an association list of column/values or a list of values for all the columns
;; <p>IMPORTANT: The returned object is NOT autoreleased! YOU are responsible for releasing it when you're done with it!</p>
;; <p><b>example:</b></p>
;; <pre> (setf db (instantiate Sqlite3 ":memory:"))
;; (db:execute-update "CREATE TABLE people (name TEXT, age INTEGER)")
;; (setf *obj* (create-dbobj db "people" '("Sue" 57)))
;; (println "Create a person named: " (*obj*:name))
;; &nbsp;
;; ; now we release it and create another object, this time using the alternate form, without an age:
;; (release *obj*)
;; (setf *obj* (create-dbobj db "people" '(("name" "Billy Jones"))))</pre>
(define (create-dbobj db table data , qs sql cols result rowid idx)
	(setf qs (join (dup "?" (length data) true) ","))
	(if (list? (first data))
		(when (db:execute-update (format DBOBJ_INSERT_SQL table (join (map first data) ",") qs) (map last data))
			(setf rowid (db:rowid))
			(if (find (list DBOBJ_ROWID_ATTR '?) data match)
				(setf (<- DBOBJ_ROWID_ATTR data) rowid)
				(push (list DBOBJ_ROWID_ATTR rowid) data)
			)
			(instantiate DB.OBJ db table data (string DBOBJ_ROWID_FINDER rowid))
		)
		(when (setf sql (db:prepare-sql (format DBOBJ_SELECT_SQL2 table)))
			(setf cols (map sql:col-name (sequence 0 (-- (sql:col-count)))))
			(when (db:execute-update (format DBOBJ_INSERT_SQL2 table qs) data)
				(setf rowid (db:rowid))
				(if (setf idx (find DBOBJ_ROWID_ATTR cols))
					(setf (data idx) rowid)
					(begin (push DBOBJ_ROWID_ATTR cols) (push rowid data))
				)
				(setf result (instantiate DB.OBJ db table (transpose (list cols data)) (string DBOBJ_ROWID_FINDER rowid)))
			)
			(deallocate sql)
			result
		)
	)
)

;; @syntax (find-dbobj <ctx-db> <str-table> <list-cols> <finder> [<limit>])
;; @param <ctx-db> A 'DF.DB' instance
;; @param <str-table> The table in which this "object" will be created in
;; @param <list-cols> A list of column names. You can use SQL aliases (e.g. "col AS alias") and special identifiers (like "*")
;; @param <finder> As described at the start of this document. See example below too.
;; @param <int-limit> Default is 1. If greater than 1 then a list of objects is returned.
; <p>IMPORTANT: The returned object is NOT autoreleased! YOU are responsible for releasing it when you're done with it!</p>
;; <p><b>example:</b></p>
;; <pre> (push-autorelease-pool) ; even examples should show proper memory management
;; (setf db (instantiate Sqlite3 "path/to/people.db"))
;; ; get the object at row 10
;; (setf *obj* (autorelease (find-dbobj db "people" '("*") 10)))
;; ; get up to 50 teenagers
;; (setf teens (find-dbobj db "people" '("*") "age > 12 AND age < 20" 50))
;; (map autorelease teens)
;; ; find a person of a random age between 1 and 20
;; (setf X (int (random 1 20)))
;; (setf *obj* (autorelease (find-dbobj db "people" '("*") (list (list "age" X)))))
;; (pop-autorelease-pool)</pre>
(define (find-dbobj db table cols finder (limit 1) , data)
	(when (integer? finder) (setf finder (string DBOBJ_ROWID_FINDER finder)))
	(push DBOBJ_ROWID_COL cols)
	(when (setf data (dbobj-assoc-rows db table cols finder limit))
		(setf data (unique data)) ; get rid of possibly duplicate columns caused by the push above
		(if (> limit 1)
			(map (fn (x) (instantiate DB.OBJ db table x (string DBOBJ_ROWID_FINDER (<- DBOBJ_ROWID_ATTR x)))) data)
			(instantiate DB.OBJ db table (first data) (string DBOBJ_ROWID_FINDER (<- DBOBJ_ROWID_ATTR (first data))))
		)
	)
)

;; @syntax (find-or-create-dbobj <ctx-db> <str-table> <list-data> <finder>)
;; <p>This function simply calls 'create-dbobj' if 'find-dbobj' is unable to locate the object(s).
;; The values in <list-data> are ignored if an object is found, and the found object's values are used instead.</p>
;; <p>Note: unlike the more flexible 'create-dbobj', the <list-data> param must be an association list of columns/values.</p>
(define (find-or-create-dbobj db table data finder)
	(unless (find-dbobj db table (map first data) finder)
		(create-dbobj db table data)
	)
)

;---------------------------------------------------------------
; !Manipulating DF.OBJs
;---------------------------------------------------------------

;; @syntax (dbobj-keys <obj>)
;; <p>Returns the keys as strings in a list. Different words, same thing.</p>
(define (dbobj-keys obj)
	(map first obj:change-set)
)

;; @syntax (dbobj-values <obj> [<bool-from-revert-set>])
;; @param <bool-from-revert-set> If true, returns the uncommitted value.
;; <p>Returns a list of the values for all the keys.</p>
(define (dbobj-values obj from-revert-set)
	(if from-revert-set
		(map last obj:revert-set)
		(map last obj:change-set)
	)
)

;; @syntax (dbobj-set-finder <obj> <finder>)
;; <p>Updates how this object finds itself in the table (for updates).</p>
(define (dbobj-set-finder obj finder)
	(if (integer? finder)
		(setf obj:finder (string DBOBJ_ROWID_FINDER finder))
		(or (list? finder) (string? finder))
		(setf obj:finder finder)
		(throw-error (string "bad type for finder: " finder))
	)
)

;; @syntax (dbobj-refetch <obj>)
;; <p>Refetches the object's values from the table. Discards any changes. Returns an association list of the fetched keys/values.</p>
(define (dbobj-refetch obj)
	(set 'obj:dirty      nil
	     'obj:revert-set (first (dbobj-assoc-rows obj:db obj:table (map first obj:revert-set) obj:finder 1))
	     'obj:change-set obj:revert-set
	)
)

;; @syntax (dbobj-save <obj>)
;; <p>Saves any changes to the database.</p>
;; @return a list of saved differences on successful update, 0 if no update was needed, or nil if update failed
(define (dbobj-save obj , diff)
	(if (null? (setf diff (difference obj:change-set obj:revert-set)))
		0
		(when (dbobj-do-update obj:db obj:table diff obj:finder)
			(set 'obj:revert-set obj:change-set 'obj:dirty nil)
			diff
		)
	)
)

;; @syntax (dbobj-delete <obj>)
;; <p>Removes the row representing this <obj> from its table. Returns 'true' on success.</p>
(define (dbobj-delete obj)
	(when (dbobj-do-delete obj:db obj:table obj:finder)
		(set 'obj:revert-set '() 'obj:change-set '())
		true
	)
)

(global
	'create-dbobj 'find-dbobj 'find-or-create-dbobj
	'dbobj-keys 'dbobj-values 'dbobj-refetch 'dbobj-refind 'dbobj-save 'dbobj-save 'dbobj-delete
)

;---------------------------------------------------------------
; !Finder-Binder, for SQL-injection proof binding in the finder
;---------------------------------------------------------------

(define (dbobj-finder-binder finder)
	(join (map (fn(x)(string (first x) "=?")) finder) DBOBJ_WHERE_COMB)
)

(define (dbobj-assoc-rows db table cols finder limit)
	(setf cols (join cols ","))
	(if (list? finder)
		(assoc-rows-with-db db (format DBOBJ_SELECT_SQL cols table (dbobj-finder-binder finder) limit) (map last finder))
		(assoc-rows-with-db db (format DBOBJ_SELECT_SQL cols table finder limit))
	)
)

(define (dbobj-do-update db table kv finder , cols)
	(setf cols (join (map first kv) "=?,"))
	(if (list? finder)
		(db:execute-update (format DBOBJ_UPDATE_SQL table cols (dbobj-finder-binder finder)) (extend (map last kv) (map last finder)))
		(db:execute-update (format DBOBJ_UPDATE_SQL table cols finder) (map last kv))
	)
)

(define (dbobj-do-delete db table finder)
	(if (list? finder)
		(db:execute-update (format DBOBJ_DELETE_SQL table (dbobj-finder-binder finder)) (map last finder))
		(db:execute-update (format DBOBJ_DELETE_SQL table finder))
	)
)

;---------------------------------------------------------------
; !The DB.OBJ constructor
;---------------------------------------------------------------

(context DB.OBJ)

(define (DB.OBJ:DB.OBJ _db _table data _finder)
	(set 'db _db 'table _table 'finder _finder)
	(when (setf change-set (setf revert-set data))
		(dolist (col (map first revert-set))
			(letex (attr-sym (sym col) attr-str col)
				(define (attr-sym)
					(case (length $args)
						(0	(<- attr-str change-set))
						(1	(setf dirty true)
							(setf (<- attr-str change-set) (first $args)))
						(2	(if (args 1)
								(if (first $args)
									(setf (<- attr-str revert-set) (first $args)) ; only do this if you're *SURE*
									(<- attr-str revert-set))
								(attr-sym (first $args)))))))))) ; this is an odd scenario, but I suppose possible

(context MAIN)