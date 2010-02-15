;; @module database_utils
;; @description Utilities for using DF.DB
;; @version 1.0
;; @author Greg Slepak

; this cannot be used because newlisp's 'caller' function is broken.
; define-smacro defined in utils.lsp (part of Dragonfly's core functions)
; (define-smacro (for-query-with-db db query )
; 	(letn (db (eval db) sql (db:prepare-sql (eval query)) keys '() ctx (prefix (caller)))
; 		;(println ctx " vs " (prefix (caller)))
; 		(dotimes (i (sql:col-count))
; 			;(push (sym (upper-case (sql:col-name i)) (prefix (caller))) keys -1)
; 			;(push (sym (upper-case (sql:col-name i)) ctx) keys -1)
; 			(push (sym (upper-case (sql:col-name i)) Dragonfly) keys -1)
; 		)
; 		(push-autorelease-pool) ; in case we have blobs
; 		(while (list? (setf values (sql:next-row)))
; 			(eval (expand (cons 'begin $args) (unify keys values)))
; 		)
; 		(pop-autorelease-pool)
; 	)
; )

(define (fn-query-with-db db query func)
	(let (sql (db:prepare-sql query) keys '())
		(setf keys (map sql:col-name (sequence 0 (-- (sql:col-count)))))
		(push-autorelease-pool) ; in case we have blobs
		(while (list? (setf values (sql:next-row)))
			(func (transpose (list keys values)))
		)
		(pop-autorelease-pool)
	)
)
(global 'fn-query-with-db)
