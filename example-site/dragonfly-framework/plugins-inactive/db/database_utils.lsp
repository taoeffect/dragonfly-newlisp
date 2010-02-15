;; @module database_utils
;; @description Utilities for using DF.DB
;; @version 1.0
;; @author Greg Slepak

; define-smacro defined in utils.lsp (part of Dragonfly's core functions)
(define-smacro (for-query-with-db db query)
	(letn (db (eval db) sql (db:prepare-sql (eval query)) keys '())
		(dotimes (i (sql:col-count))
			(push (sym (upper-case (sql:col-name i)) (prefix (caller))) keys -1)
		)
		(push-autorelease-pool) ; in case we have blobs
		(while (list? (setf values (sql:next-row)))
			(eval (expand (cons 'begin $args) (unify keys values)))
		)
		(pop-autorelease-pool)
	)
)
; backwards compatibility with newLISP < 10.1.11
(unless (global? 'prefix)
	(setf for-query-with-db:prefix (fn-macro () 'DF))
)