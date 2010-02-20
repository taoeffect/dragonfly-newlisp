;; @module database_orm
;; @description DB.OBJ - Simple ORM Singleton class for DF.DB
;; @version 1.0
;; @author Greg Slepak

(DF:activate-plugin "db/database_utils")

(new-class 'DB.OBJ)

(set (global 'DBOBJ_SELECT_SQL)   "SELECT %s FROM %s WHERE %s LIMIT 1"
     (global 'DBOBJ_SELECT_SQL2)  "SELECT * FROM %s" ; quasi-hack to obtain the col names b/c INSERT doesn't tell us. we don't actually retrieve the rows.
     (global 'DBOBJ_UPDATE_SQL)   "UPDATE %s SET %s=? WHERE %s" ; LIMIT isn't supported for UPDATE unless sqlite3 was compiled with the option
     (global 'DBOBJ_INSERT_SQL)   "INSERT INTO %s (%s) VALUES (%s)"
     (global 'DBOBJ_INSERT_SQL2)  "INSERT INTO %s VALUES (%s)"
	 (global 'DBOBJ_DELETE_SQL)   "DELETE FROM %s WHERE %s"
     (global 'DBOBJ_ROWID_COL)    "ROWID=")

;---------------------------------------------------------------
; !Getting DF.OBJs
;---------------------------------------------------------------

; The returned object is NOT autoreleased! YOU are responsible for releasing it when you're done with it!
(define (create-dbobj db table data , qs sql cols result)
	(setf qs (join (dup "?" (length data) true) ","))
	(if (list? (first data))
		(when (db:execute-update (format DBOBJ_INSERT_SQL table (join (map first data) ",") qs) (map last data))
			(instantiate DB.OBJ db table data (string DBOBJ_ROWID_COL (db:rowid))))
		(when (setf sql (db:prepare-sql (format DBOBJ_SELECT_SQL2 table)))
			(setf cols (map sql:col-name (sequence 0 (-- (sql:col-count)))))
			(when (db:execute-update (format DBOBJ_INSERT_SQL2 table qs) data)
				(setf result (instantiate DB.OBJ db table (transpose (list cols data)) (string DBOBJ_ROWID_COL (db:rowid)))))
			(deallocate sql)
			result
		)
	)
)

; The returned object is NOT autoreleased! YOU are responsible for releasing it when you're done with it!
(define (find-dbobj db table cols finder , data)
	(when (integer? finder) (setf finder (string DBOBJ_ROWID_COL finder)))
	(when (setf data (assoc-row-with-db db (format DBOBJ_SELECT_SQL (join cols ",") table finder)))
		(instantiate DB.OBJ db table data finder)
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

(define (dbobj-refetch obj)
	(set 'obj:dirty      nil
	     'obj:revert-set (assoc-row-with-db obj:db (format DBOBJ_SELECT_SQL (join (map first obj:revert-set) ",") obj:table obj:finder))
	     'obj:change-set obj:revert-set
	)
)

(define (dbobj-refind obj finder)
	(if (integer? finder)
		(setf obj:finder (string DBOBJ_ROWID_COL finder))
		(setf obj:finder finder)
	)
	(dbobj-refetch obj)
)

; returns list of saved differences on successful update, 0 if no update was needed, or nil if update failed
(define (dbobj-save obj , diff db)
	(setf db obj:db)
	(if (null? (setf diff (difference obj:change-set obj:revert-set)))
		0
		(when (db:execute-update (format DBOBJ_UPDATE_SQL obj:table (join (map first diff) "=?,") obj:finder) (map last diff))
			(set 'obj:revert-set obj:change-set 'obj:dirty nil)
			diff
		)
	)
)

(define (dbobj-delete obj , db)
	(setf db obj:db)
	(when (db:execute-update (format DBOBJ_DELETE_SQL obj:table obj:finder))
		(set 'obj:revert-set '() 'obj:change-set '())
		true
	)
)

(global
	'create-dbobj 'find-dbobj 'find-or-create-dbobj
	'dbobj-keys 'dbobj-values 'dbobj-refetch 'dbobj-refind 'dbobj-save 'dbobj-save 'dbobj-delete
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
							(setf dirty true))
						(if from-revert-set
							(<- attr-str revert-set)
							(<- attr-str change-set))))))))

(context MAIN)