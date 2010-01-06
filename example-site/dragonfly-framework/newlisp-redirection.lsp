;; This file is used when testing using the newLISP server
;; i.e. via the ./newlispServer command
;; 
;; This allows you to use URLs like this:
;; 	http://localhost:8080/dragonfly_welcome
;; Instead of:
;; 	http://localhost:8080/?dragonfly_welcome
;;
;; NEWLISP_REDIRECTION_EXTENSIONS is a list of file extensions that,
;; when specified explicitely in the URL, will cause the URL to be
;; process through Dragonfly's routes and not sent directly to newLISP.
;; 
;; This allows you to explicitely specify a file, like so:
;;  http://localhost:8080/foo.html
;; 
;; That will process 'foo.html' through Dragonfly's routes, typically
;; this allows newLISP code in an HTML template to be evaluated (as with
;; Route.Static).
;; 
;; Feel free to customize this list to your liking.

(set 'NEWLISP_REDIRECTION_EXTRACT_PATH (regex-comp {\w+\s+/([^\s]+)})
     'NEWLISP_REDIRECTION_REWRITE (regex-comp {^(\w+)\s+/([^\?]+)(\?)?})
     'NEWLISP_REDIRECTION_EXTENSIONS '(".html" ".nhtml" ".nl")
)

; on windows newlisp will complain if this directory doesn't exist
(when (and (= ostype "Win32") (not (directory? "/tmp")))
	(make-dir "/tmp")
)

(command-event (fn (s , request)
	(regex NEWLISP_REDIRECTION_EXTRACT_PATH s 0x10000)
	(setf request $1)
	(if (and request
			(or (not (file? request))
				(exists (curry ends-with request) NEWLISP_REDIRECTION_EXTENSIONS)
				(directory? request)
			)
		)
		(replace NEWLISP_REDIRECTION_REWRITE s
			(string $1 " /?" $2 (if $3 "&" "")) 0x10000
		)
	)
	s
))
