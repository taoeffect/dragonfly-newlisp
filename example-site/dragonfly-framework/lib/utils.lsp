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
	
	(setf load-once.lp '()) ; empty load path initially
	(new Tree 'load-once.loaded)
	
;; @syntax (add-to-load-path <str-path-1> <str-path-2> ...)
;; <p>Dragonfly overrides the built-in function 'load' so that files
;; are loaded only once. In addition it supports the concept of "load paths"
;; which can be added using this function. This means that you no longer need
;; to modify third-party code that contains 'load' calls to files located in
;; different locations. Simply add a new load path instead.</p>
;; <b>example:</b>
;; <pre> ; the old way
;; (load "MyClass.lsp") ;=> ERROR! MyClass.lsp doesn't exist here!
;; ; we must rewrite the file to point to the new location of MyClass.lsp:
;; (load "../../myfolder/MyClass.lsp")
;; ; -------------------------------
;; ; New way, using add-to-load-path
;; ; -------------------------------
;; (add-to-load-path "../../myfolder")
;; ; no need to update any source files, it just works.</pre>
;; <b>warning:</b> Use this function sparingly as name-conflicts could
;; result in the wrong file being loaded!
	(define (add-to-load-path)
		(doargs (path)
			(setf load-once.lp (unique (push (real-path path) load-once.lp)))
		)
	)
	
	(define (load-once)
		; check if the last argument is a context (to behave like 'load' does)
		(let (ctx (let (_ctx (last $args)) (if (context? _ctx) _ctx MAIN)) filename nil)
			(doargs (file (context? file))
				(setf filename file)
				(dolist (lp load-once.lp (file? file))
					(setf file (string lp "/" filename))
				)
				(unless (setf file (real-path file))
					(throw-error (string "cannot load file: " filename))
				)
				(when (not (load-once.loaded file))
					(load-once.loaded file true)
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
	
;; @syntax (wrap-func <func> <lambda>)
;; <p>Replaces <func> with <lambda>. Inside of <lambda> use 'wrapped-func' to refer
;; to the original function. This can be very handy for "aspect oriented programming".</p>
;; <b>example:</b>
;; <pre> (wrap-func db:execute-update
;;     (fn () (unless (apply wrapped-func $args)
;;         (throw-error (string "execute-update failed: " $args)))))</pre>
	(define-macro (wrap-func func-sym wrapper , wrapped-func)
		(setf wrapped-func (sym (string func-sym "|wrapped#" (inc wrap-func.counter))))
		(set wrapped-func (eval func-sym))
		(set func-sym (eval (expand wrapper 'wrapped-func)))
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
		; dolist is slightly faster than map
		(dolist (x assoc-list) (ctx (first x) (last x)))
	)
	
;; @syntax (SET_DFLY_SELF <str-filepath>)
;; @param <str-filepath> The path to the file being served, or the primary file responsible for handling this route.
;; <p>Routes should call this global function with the path to the file that's being displayed
;; or the file that's principly in charge of handling this route. This function will then
;; set the global variables 'DFLY_SELF' and 'DFLY_SELF_DIR' to point to that file and its
;; parent directory, respectively.</p>
;; <p>The default routes 'Route.Static' and 'Route.Resource' call this function first thing
;; in their 'Route:run' methods, prior to loading the files. This is the recommended way of
;; calling this function.</p>
	(define (SET_DFLY_SELF file)
		(when (setf file (real-path file))
			(set (global 'DFLY_SELF)     file
			     (global 'DFLY_SELF_DIR) (regex-captcha {^(.+/)} file)
			)
		)
	)
	
	; these functions should be global (define-subclass should not)
	(global 'load-files-in-dir 'regex-captcha 'load-once
		'wrap-func 'into-ctx-assoc 'add-to-load-path 'SET_DFLY_SELF
	)
	
	; swap these functions for ours and save the originals
	(constant (global 'sys-load) load)
	(constant 'load load-once)
	(constant (global 'sys-print) print)
	(constant 'print Dragonfly:print)
	(constant (global 'sys-println) println)
	(constant 'println Dragonfly:println)
	
;; @syntax NEWLISP64
;; <p>A constant that is 'true' if we're running the 64-bit version of newLISP.</p>
	(constant (global 'NEWLISP64) (not (zero? (& (sys-info -1) 256))))
;; @syntax (get-ptr <symbol>)
;; <p>Alias for 'get-long' on 64-bit systems, and 'get-int' on 32-bit systems.</p>
	(constant (global 'get-ptr) (if NEWLISP64 get-long get-int))
;; @syntax (throw-not-implemented)
;; <p>Used to indicate that an ObjNL method *must* be overwritten.</p>
	(constant (global 'throw-not-implemented) (fn()(throw-error "not defined by subclass!")))
)

