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
		(extend Dragonfly:STDOUT (apply string $args))
		(last $args) ; to behave the same way as print
	)
	
	(set 'file-ext-regex (regex-comp {^.*\.(.+)$})
	     'del-ext-regex  (regex-comp {^(.*)\..+$})
	     'basename-regex (regex-comp {^(.*/)?(.+)$})
	     'dirname-regex  (regex-comp {^(.*)/([^/]+/?)$})
	)

;; @syntax (file-ext <str-path>)
;; <p>Returns the file extension of the file in <str-path></p>
;; <pre> (file-ext "")
;; ;=> ""
;; (file-ext "asdf")
;; ;=> ""
;; (file-ext "asdf.")
;; ;=> ""
;; (file-ext "asdf.jpg")
;; ;=> "jpg"</pre>
	(define (file-ext path)
		(if (regex file-ext-regex path 0x10000)
			$1
			""
		)
	)

;; @syntax (del-ext <str-path>)
;; <p>Returns the <str-path> without the file extension removed.</p>
;; <pre> (del-ext "")
;; ;=> ""
;; (del-ext "asdf")
;; ;=> "asdf"
;; (del-ext "asdf.")
;; ;=> "asdf."
;; (del-ext "asdf.jpg")
;; ;=> "asdf"</pre>
	(define (del-ext path)
		(if (regex del-ext-regex path 0x10000)
			$1
			path
		)
	)

;; @syntax (basename <str-path>)
;; <p>Returns the filename component of <str-path>.</p>
;; <pre> (basename "")
;; ;=> ""
;; (basename "/")
;; ;=> ""
;; (basename "asdf")
;; ;=> "asdf"
;; (basename "/foo/bar")
;; ;=> "bar"
;; (basename "/foo/bar/")
;; ;=> "bar"</pre>
	(define (basename path)
		(if (regex basename-regex path 0x10000)
			(trim $2 "/")
			""
		)
	)

;; @syntax (dirname <str-path>)
;; <p>Returns the directory path component of <str-path>.</p>
;; <pre> (dirname "")
;; ;=> ""
;; (dirname "/")
;; ;=> "/"
;; (dirname "asdf")
;; ;=> "."
;; (dirname "/asdf/")
;; ;=> "/"
;; (dirname "asdf/foo")
;; ;=> "asdf"</pre>
	(define (dirname path)
		(if (empty? path)
			""
			(if (regex dirname-regex path 0x10000)
				(if (empty? $1) "/" $1)
				(if (starts-with path "/") "/" ".")
			)
		)
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
		(setf wrapped-func (sym (string func-sym "|wrapped#" (++ wrap-func.counter))))
		(set wrapped-func (eval func-sym))
		(set func-sym (eval (expand wrapper 'wrapped-func)))
	)

;; @syntax (SET_DF_SELF <str-filepath>)
;; @param <str-filepath> The path to the file being served, or the primary file responsible for handling this route.
;; <p>Routes should call this global function with the path to the file that's being displayed
;; or the file that's principly in charge of handling this route. This function will then
;; set the global variables 'DF_SELF' and 'DF_SELF_DIR' to point to that file and its
;; parent directory, respectively.</p>
;; <p>The default routes 'Route.Static' and 'Route.Resource' call this function first thing
;; in their 'Route:run' methods, prior to loading the files. This is the recommended way of
;; calling this function.</p>
	(define (SET_DF_SELF file)
		(when (setf file (real-path file))
			(constant (global 'DF_SELF) file)
			(constant (global 'DF_SELF_DIR) (regex-captcha {^(.+)/} file))
		)
	)
	
;; @syntax (define-smacro)
;; <p>Defines a "safe macro", free of variable capture issues. Note though
;; that it does this by placing the macro in its own context, so be careful
;; when choosing its name!</p>
	(define-macro (define-smacro)
	    (let (_temp1 (append (fn-macro) (cons (1 (args 0)) (rest $args))) _temp2 (args 0 0))
	        (def-new '_temp1 (sym _temp2 _temp2))
		)
	)
	
	(define (extract lst sym-assoc-list , idx)
		(when (setf idx (find lst (eval sym-assoc-list) match))
			(nth idx (eval sym-assoc-list))
		)
	)
	
	; these functions should be global (define-subclass should not)
	(global 'load-files-in-dir 'regex-captcha 'load-once
		'wrap-func 'add-to-load-path 'SET_DF_SELF
		'file-ext 'del-ext 'basename 'dirname 'define-smacro
		'extract
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
;; @syntax (<- <key> <assoc-list>)
;; <p>An alias for 'lookup'</p>
	(constant (global '<-) lookup)
)

