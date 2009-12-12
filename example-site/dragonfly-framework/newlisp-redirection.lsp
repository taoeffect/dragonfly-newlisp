;; This file is used when testing using the newLISP server
;; i.e. via the ./newlispServer command
;; 
;; This allows you to use URLs like this:
;; 	http://localhost:8080/dragonfly_welcome
;; Instead of:
;; 	http://localhost:8080/?dragonfly_welcome

(set 'NEWLISP_REDIRECTION_EXTRACT_PATH (regex-comp {\w+\s+/([^\s]+)}))
(set 'NEWLISP_REDIRECTION_REWRITE (regex-comp {^(\w+)\s+/([^\?]+)(\?)?}))

; on windows newlisp will complain if this directory doesn't exist
(when (and (= ostype "Win32") (not (directory? "/tmp")))
	(make-dir "/tmp")
)

(command-event (fn (s , request)
	(regex NEWLISP_REDIRECTION_EXTRACT_PATH s 0x10000)
	(set 'request $1)
	; TODO: we account for most scenarios here but we don't account
	; for the situation where say an .html template is directly requested
	; by its actual path. The demo site doesn't use this but some other
	; sites might. The fix is to check STATIC_TRIGGER_EXTENSIONS.
	; The workaround is to use views (i.e. STATIC_TRANSFORMATIONS) so
	; that the actual file isn't specified (the way the example-site does it)
	(if (and request (or (not (file? request)) (directory? request)))
		(replace NEWLISP_REDIRECTION_REWRITE s
			(string $1 " /?" $2 (if $3 "&" "")) 0x10000
		)
	)
	s
))
