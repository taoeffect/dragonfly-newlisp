; do not modify the line below
(dolist (pair (env)) (constant (global (sym (first pair))) (last pair)))

; NOTE: the paths, including folders, shouldn't have slashes on the ends!

;===============================================================================
; Global Constants
;===============================================================================

; docroot (also site root, usually doesn't need modification)
(constant (global 'DOCUMENT_ROOT) (env "DOCUMENT_ROOT"))
; dragonfly root
(constant (global 'DRAGONFLY_ROOT) (string DOCUMENT_ROOT "/dragonfly-framework"))

; sync any customization of DOCUMENT_ROOT with the 'env'
; don't modify these two lines below!
(constant (global 'ORIGINAL_ROOT) (env "DOCUMENT_ROOT"))
(env "DOCUMENT_ROOT" DOCUMENT_ROOT)

(context 'Dragonfly)

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

;===============================================================================
; Filtering of static files (for .php-like behavior)
;===============================================================================

; if you set to nil then make sure to comment out the line in .htaccess
(constant 'ENABLE_STATIC_TEMPLATES true)
; file extensions that triggers the handler (update .htaccess to match this!)
(constant 'STATIC_EXTENSIONS '(".html"))
; alternatively the static handler can be used to load index files given
; a URL pointing to a directory. This variable customizes the extension
; of the index file that the handler will check to see if it handles the request
; ex: mysite.com/myproduct/support => mysite.com/myproduct/support/index.html
(constant 'STATIC_INDEX_EXTENSION ".html")

;===============================================================================
; Views
;===============================================================================

; set to nil to disable views handling
(constant 'ENABLE_VIEW_HANDLER true)
; location of views
(constant 'VIEWS_PATH (string DOCUMENT_ROOT "/views"))
; location of partials
(constant 'PARTIALS_PATH (string DOCUMENT_ROOT "/views/partials"))
; setting a default view
(constant 'DEFAULTVIEW "welcome")
; setting a default action
(constant 'DEFAULTACTION "index") ; display all
; setting a default rss view
(constant 'DEFAULTRSS "dragonfly_rssfeed")
; if non-nil, then all of your views (and partials) must have this extension
; ex: ".html"
(constant 'VIEW_EXTENSION nil)

;===============================================================================
; RESTful Resources
;===============================================================================

; TODO: implement this

; set to nil to disable REST handling
(constant 'ENABLE_RESTFUL_HANDLER true)
; location of RESTful resources (relative to DOCUMENT_ROOT)
(constant 'RESTFUL_DIR "resources")

; define any RESTful resources (just the root, i.e. the first slash)
; note that to allow runtime modification this is not a constant
(define restful-resources '(
	"create"
	"show"
	"edit"
	"update"
	"remove"
))

(context MAIN)
