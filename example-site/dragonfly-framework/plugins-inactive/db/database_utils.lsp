;; @module database_utils
;; @description Utilities for using DF.DB
;; @version 1.0
;; @author Greg Slepak

; this cannot be used because newlisp's 'caller' function is broken.
; define-smacro defined in utils.lsp (part of Dragonfly's core functions)
; (define-smacro (for-query-with-db db query )
; 	(letn (db (eval db) sql (db:prepare-sql (eval query)) keys '())
; 		(dotimes (i (sql:col-count))
; 			;(push (sym (upper-case (sql:col-name i)) (prefix (caller))) keys -1)
; 			(push (sym (upper-case (sql:col-name i)) Dragonfly) keys -1)
; 		)
; 		(push-autorelease-pool) ; in case we have blobs
; 		(while (list? (setf values (sql:next-row)))
; 			(eval (expand (cons 'begin $args) (unify keys values)))
; 		)
; 		(pop-autorelease-pool)
;		(deallocate sql)
; 	)
; )

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
