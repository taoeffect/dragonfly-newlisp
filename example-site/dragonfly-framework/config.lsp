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
; File extensions that trigger the handler immediately.
; This list can be large with no real performance penalty.
; Make *sure* to update .htaccess to match this! (see comment there.)
(constant 'STATIC_TRIGGER_EXTENSIONS '(".html"))
; If STATIC_PATH_PREFIX is non-nil, then the static handler will check to
; see if the given URL exists when prepended with the prefix, relative
; to the DOCUMENT_ROOT. It can be used in conjunction with the
; STATIC_TEST_EXTENSIONS to create pretty URLs.
(constant 'STATIC_PATH_PREFIX "pages/")
; You can use the static handler to create pretty URLs using the
; STATIC_TEST_EXTENSIONS constant. It is used to check for index files in
; directories and in conjunction with STATIC_PATH_PREFIX.
; Here are some examples:
; (=> means "loads file")
; example-site.com/foo => example-site.com/foo/index.html
; example-site.com/about => example-site.com/pages/about.html
; Do not make this a very long list, otherwise the page will load slowly.
(constant 'STATIC_TEST_EXTENSIONS '(".html"))

;===============================================================================
; Views
;===============================================================================

; set to nil to disable views handling
(constant 'ENABLE_VIEW_HANDLER true)
; location of views
(constant 'PAGES_PATH (string DOCUMENT_ROOT "/views"))
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

; set to nil to disable REST handling
(constant 'ENABLE_RESTFUL_HANDLER true)
; location of RESTful resources
(constant 'RESOURCES_PATH (string DOCUMENT_ROOT "/resources"))

(context MAIN)
