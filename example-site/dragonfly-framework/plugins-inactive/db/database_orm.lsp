;; @module database_orm
;; @description DB.OBJ - Simple ORM class for DF.DB
;; @version 1.0
;; @author Greg Slepak
;; <p></p>
;; <p>To accomplish this, the interface introduces three Objective newLISP classes:
;; 'DF.DB', 'DF.SQL', and 'DF.BLOB'.</p>
;; <h3>DF.SQL</h3>
;; A 'DF.SQL' object is a wrapper around an SQL statement, is retrieved through one of
;; two functions: 'DF.DB:execute-query' and the lower-level 'DF.DB:preprare-sql'.
;; <p>It is used to retrieve rows from the result set of a query one-by-one.</p>
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
;; <h3>Version history</h3>
;; <b>1.0</b> &bull; initial release

(DF:activate-plugin "db/database_utils")

(new-class 'DB.OBJ)

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

; The returned object is NOT autoreleased! YOU are responsible for releasing it when you're done with it!
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

; The returned object is NOT autoreleased! YOU are responsible for releasing it when you're done with it!
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


(define (find-or-create-dbobj db table data finder)
	(unless (find-dbobj db table (map first data) finder)
		(create-dbobj db table data)
	)
)

;---------------------------------------------------------------
; !Manipulating DF.OBJs
;---------------------------------------------------------------

(define (dbobj-keys obj)
	(map first obj:change-set)
)

(define (dbobj-values obj from-revert-set)
	(if from-revert-set
		(map last obj:revert-set)
		(map last obj:change-set)
	)
)

(define (dbobj-set-finder obj finder)
	(if (integer? finder)
		(setf obj:finder (string DBOBJ_ROWID_FINDER finder))
		(or (list? finder) (string? finder))
		(setf obj:finder finder)
		(throw-error (string "bad type for finder: " finder))
	)
)

(define (dbobj-refetch obj)
	(set 'obj:dirty      nil
	     'obj:revert-set (first (dbobj-assoc-rows obj:db obj:table (map first obj:revert-set) obj:finder 1))
	     'obj:change-set obj:revert-set
	)
)

; returns list of saved differences on successful update, 0 if no update was needed, or nil if update failed
(define (dbobj-save obj , diff)
	(if (null? (setf diff (difference obj:change-set obj:revert-set)))
		0
		(when (dbobj-do-update obj:db obj:table diff obj:finder)
			(set 'obj:revert-set obj:change-set 'obj:dirty nil)
			diff
		)
	)
)

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
				(define (attr-sym value from-revert-set)
					(if value
						(begin
							(setf (<- attr-str change-set) value)
							(setf dirty true)
							(when from-revert-set (setf (<- attr-str revert-set) value))) ; only do this if you're *SURE*
						(if from-revert-set
							(<- attr-str revert-set)
							(<- attr-str change-set))))))))

(context MAIN)