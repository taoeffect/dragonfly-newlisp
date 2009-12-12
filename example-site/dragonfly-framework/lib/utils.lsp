; NOTE: it's OK to load this file multiple times
;; @module utils.lsp
;; @author Greg Slepak <greg at taoeffect.com>
;; <p>This file not only provides the functions documented below, but
;; it also plays a role in globally overriding certain functions in the MAIN context.</p>
;; <p>The overridden functions include: 'load', 'print', and 'println'.</p>
;; <p>'load' is overwritten to load a file only once, while 'print' and 'println'
;; are overwritten to send their output the the 'Dragonfly:STDOUT' buffer, allowing
;; Dragonfly to ensure pages are displayed properly.</p>

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
		;(context (args 0 0))
		(dolist (method (rest $args))
			(setf (method 0 0) (sym $it (args 0 0)))
			(eval (push 'define method))
		)
		;(context MAIN)
	)
	
;; @syntax (regex-captcha <str-regex> <str> [<int-options>] [<int-captcha>])
;; @param <int-options> options to regex, defaults to 0.
;; @param <int-captch> which of the regex group captures to return, defaults to 1.
;; <p>Returns the captured text, or nil if it couldn't be captured.<br/>This is a global function.</p>
;; <b>example:</b>
;; <pre> (regex-captcha {^(foo|bar).*} "foobaz") => "foo"
;; (regex-captcha {^(foo|bar).*} "bazfoo") => nil</pre>
;; 
	(define (regex-captcha regex-str str (options 0) (captcha 1))
		(if (regex regex-str str options)
			($ captcha)
		)
	)
	
;; @syntax (load-files-in-dir <str-dir> <regex-match>)
;; <p>Loads all the files in <str-dir> matching <regex-match>. Does not
;; traverse subdirectories.<br/>This is a global function.</p>
;;
	(define (load-files-in-dir dir regex-match)
		(dolist (x (directory dir regex-match))
			(load-once (string dir "/" x))
		)
	)

;; @syntax (into-ctx-assoc <ctx> <list-assoc>)
;; <p>Places the key/value pairs in <list-assoc> into the context <ctx>
;; to be used as a lookup table.<br/>This is a global function.</p>
;; <b>example:</b>
;; <pre> (new Tree 'MyCtx)
;; (into-ctx-assoc MyCtx '(
;;     ("key" "value")
;;     ("apple" "mmmm... good")
;;     ("organic?" true)
;; ))</pre>
;; 
	(define (into-ctx-assoc ctx assoc-list)
		(dolist (x assoc-list) (ctx (x 0) (x 1))) ; here dolist is slightly faster than map
	)
	
	; these functions should be global (define-subclass should not)
	(global 'load-files-in-dir 'regex-captcha 'load-once 'into-ctx-assoc)
	
	; swap these functions for ours and save the originals
	(constant (global 'sys-load) load)
	(constant 'load load-once)
	(constant (global 'sys-print) print)
	(constant 'print Dragonfly:print)
	(constant (global 'sys-println) println)
	(constant 'println Dragonfly:println)
	
	; some other useful globals (primarily used by database.lsp)
	(constant (global 'NEWLISP64) (not (zero? (& (sys-info -1) 256))))
	; cache the function for getting a pointer
	(constant (global 'get-ptr) (if NEWLISP64 get-long get-int))
	; used to indicate that a method *must* be overwritten
	(constant (global 'throw-not-implemented) (fn()(throw-error "not defined by subclass!")))
)

