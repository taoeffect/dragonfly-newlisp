; NOTE: it's OK to load this file multiple times
;; @module utils.lsp
;; @author Greg Slepak <greg at taoeffect.com>

; protect against situation where one of the load functions is used to
; load this file, thereby redefining the function itself while it's running
; and causing newlisp to crash.
; This may also speed up the loading of this file the second time around.
(unless load-once
	
	(define (load-once)
		; check if the last argument is a context (to behave like 'load' does)
		(let (ctx (let (_ctx (last $args)) (if (context? _ctx) _ctx MAIN)))
			(doargs (file)
				(unless (or (context? file) (find file load-once.loaded))
					(push file load-once.loaded)
					(sys-load file ctx)
				)
			)
		)
	)
	
	(define (Dragonfly:println)
		(apply Dragonfly:print (push "\n" (args) -1))
	)
	
	(define (Dragonfly:print)
		(write-buffer Dragonfly:STDOUT (apply string (args)))
		(last (args)) ; to behave the same way as print
	)
	
	; If someday newLISP supports switching contexts like this we'll use it
	(define-macro (define-subclass)
		(new (args 0 1) (args 0 0))
		(context (args 0 0))
		(dolist (method (rest $args))
			(setf (method 0 0) (sym $it (args 0 0)))
			(eval (push 'define method))
		)
		(context MAIN)
	)
	
;; @syntax (regex-captcha <str-regex> <str> [<int-options>] [<int-captcha>])
;; 
	(define (regex-captcha regex-str str (options 0) (captcha 1))
		(if (regex regex-str str options)
			($ captcha)
		)
	)
	
;; @syntax (load-files-in-dir <str-dir> <regex-match>)
;;
	(define (load-files-in-dir dir regex-match)
		(dolist (x (directory dir regex-match))
			(load-once (string dir "/" x))
		)
	)
	
	; these two functions should be global (define-subclass should not)
	(global 'load-files-in-dir 'regex-captcha 'load-once)
	
	; swap these functions for ours and save the originals
	(constant (global 'sys-load) load)
	(constant 'load load-once)
	(constant (global 'sys-print) print)
	(constant 'print Dragonfly:print)
	(constant (global 'sys-println) println)
	(constant 'println Dragonfly:println)
)

