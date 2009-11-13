;; This file is used when testing using the newLISP server
;; i.e. via the ./newlispServer command
;; 
;; This allows you to use URLs like this:
;; 	http://localhost:8080/dragonfly_welcome
;; Instead of:
;; 	http://localhost:8080/?dragonfly_welcome

(set 'NEWLISP_REDIRECTION_EXTRACT_PATH (regex-comp {\w+\s+/([^\s]+)}))
(set 'NEWLISP_REDIRECTION_REWRITE (regex-comp {^(\w+)\s+/([^\?]+)(\?)?}))

(command-event (fn (s , request)
	(regex NEWLISP_REDIRECTION_EXTRACT_PATH s 0x10000)
	(set 'request $1)
	(if (and request (or (not (file? request)) (directory? request)))
		(replace NEWLISP_REDIRECTION_REWRITE s
			(string $1 " /?" $2 (if $3 "&" "")) 0x10000
		)
	)
	s
))
