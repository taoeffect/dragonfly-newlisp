;; @module database_orm
;; @description DB.OBJ - Simple Object Relational Mapping (ORM) for DF.DB
;; @version 1.0
;; @author Greg Slepak

(DF:activate-plugin "db/database_utils")
(new-class 'DB.OBJ)
(context DB.OBJ)

(constant 'SELECT_SQL	"SELECT %s FROM %s WHERE %s LIMIT 1"
          'UPDATE_SQL	"UPDATE %s SET %s=? WHERE %s LIMIT 1")

(define (DB.OBJ:DB.OBJ _db _table _cols _finder)
	(if (= @self @class)
		(autorelease (instantiate @class _db _table _cols _finder))
		(begin
			(set 'db _db 'table _table 'cols _cols 'finder _finder)
			(setf revert-set (assoc-row-with-db db (format SELECT_SQL (join cols ",") table finder)))
			(when (setf change-set revert-set)
				(dolist (col cols)
					(letex (attr-sym (sym col) attr-str col)
						(define (attr-sym value from-revert-set)
							(if value
								(begin
									(setf (<- attr-str change-set) value)
									(setf dirty true))
								(if from-revert-set
									(<- attr-str revert-set)
									(<- attr-str change-set))))))))))

(define (DB.OBJ:refetch)
	(set 'dirty nil
	     'revert-set (assoc-row-with-db db (format SELECT_SQL (join (map first revert-set) ",") table finder))
	     'change-set revert-set
	)
)

(define (DB.OBJ:save , diff)
	(unless (null? (setf diff (difference change-set revert-set)))
		(when (db:execute-update (format UPDATE_SQL table (join (map first diff) "=?,") finder) (map last diff))
			(set 'revert-set change-set 'dirty nil)
		)
	)
)

(context MAIN)