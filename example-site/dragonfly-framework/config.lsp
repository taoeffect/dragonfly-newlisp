; Do not modify the two lines below!
(env "_" "") (delete '_) ; make sure this isn't defined!
(dolist (x (env)) (constant (global (sym (upper-case (first x)))) (last x)))

; NOTE: the paths, including folders, shouldn't have slashes on the ends!

;===============================================================================
; Global Constants
;===============================================================================

; docroot (also site root, usually doesn't need modification)
(constant (global 'DOCUMENT_ROOT) (env "DOCUMENT_ROOT"))
; dragonfly root
(constant (global 'DRAGONFLY_ROOT) (string DOCUMENT_ROOT "/dragonfly-framework"))

; ------------------------------------------------------
; sync any customization of DOCUMENT_ROOT with the 'env'
; don't modify these two lines below!
(constant (global 'ORIGINAL_ROOT) (env "DOCUMENT_ROOT"))
(env "DOCUMENT_ROOT" DOCUMENT_ROOT)
; ------------------------------------------------------

(context 'Dragonfly)

;===============================================================================
; Default locations
;===============================================================================

; location of views (for use with 'display-view' function)
(constant 'VIEWS_PATH (string DOCUMENT_ROOT "/views"))
; location of partials (for use with 'display-partial' function)
(constant 'PARTIALS_PATH (string DOCUMENT_ROOT "/views/partials"))
; setting a default view (sans file extension)
(constant 'DEFAULT_VIEW "welcome")
; used by 'display-view' to save you keystrokes and by the static routing.
(constant 'VIEW_EXTENSION ".html")

;===============================================================================
; Static Route Configuration
;===============================================================================

; If you set to nil then make sure to comment out the line in .htaccess
(constant 'ENABLE_STATIC_TEMPLATES true)
; If the requested URL has one of these extensions, the handler will check
; if it exists. If it does it will pass the file through the template evaluator,
; otherwise it will not match and defer to the other handler(s).
; If you don't want the file to be passed through the template evaluator, then
; you should either remove its extension from this list (and the .htaccess),
; or modify the .htaccess file to include an exception for that file.
; Remember: make *sure* to update .htaccess to match this! (see comment there.)
(constant 'STATIC_TRIGGER_EXTENSIONS '(".html"))
; If the path does not have one of the STATIC_TRIGGER_EXTENSIONS, then the
; handler will attempt to modify the URL (not including the GET params) using
; the STATIC_TRANSFORMATIONS, which is simply a list of possible modifications
; to the path, which is bound to the '_' symbol). If one of them matches a file,
; the entire route matches and the file is passed through the template evaluator,
; otherwise it will not match and defer to the other handler(s).
(constant 'STATIC_TRANSFORMATIONS '(
	(string DOCUMENT_ROOT "/" _ "/index.html")
	(string VIEWS_PATH "/" _)
	(string VIEWS_PATH "/" _ VIEW_EXTENSION)
))

;===============================================================================
; RESTful Route Configuration
;===============================================================================

; set to nil to disable the RESTful handler
(constant 'ENABLE_RESTFUL_HANDLER true)
; location of RESTful resources
(constant 'RESOURCES_PATH (string DOCUMENT_ROOT "/resources"))

;===============================================================================
; Logging
;===============================================================================

; One of 'LOG_DEBUG, 'LOG_INFO, 'LOG_WARN, and 'LOG_ERROR
(constant 'LOG_LEVEL 'LOG_INFO)
; the location of the logfile
(constant 'LOG_FILE_PATH (string DRAGONFLY_ROOT "/dragonfly.log"))

;===============================================================================
; Templating
;===============================================================================

; Add an '=' sign after the OPEN_TAG to inline the output
; ex: <%= (+ 1 1) %> will print '2' to the output webpage
(constant 'OPEN_TAG "<%")
(constant 'CLOSE_TAG "%>")

(context MAIN)
