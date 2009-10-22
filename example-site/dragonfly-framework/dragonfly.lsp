;;  Copyright (C) <2009> <Marc Hildmann, Greg Slepak>
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; @module Dragonfly
;; @author Marc Hildmann <marc.hildmann at gmail.com>, Greg Slepak <greg at taoeffect.com>
;; @version 0.50
;; 
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; just like newLISP itself.</p>

; $SERVER is a synonym for env
; this line at top because it must be executed in MAIN
; for $GET, $POST, and $FILES see lib/request.lsp
(constant (global '$SERVER) env)

; DF is a shorthand to the Dragonfly context
; so that things like log-err can be written DF:log-err
(constant (global 'DF) Dragonfly)

(context 'Dragonfly)

;===============================================================================
; !Public Constants and Variables
;===============================================================================

; These are some of the few constants that we'll have outside of env
(constant 'DRAGONFLY_MAJOR 0)
(constant 'DRAGONFLY_MINOR 50)
(constant 'DRAGONFLY_VERSION (format "Version %d.%d" DRAGONFLY_MAJOR DRAGONFLY_MINOR))

; This is buffer that contains that content that will get written
; to STDOUT if no errors are thrown. If you define your own Route
; then you must write the results to it using 'write-buffer' instead
; of calling 'println' or 'print' directly!
(define STDOUT "")

; you can customize this variable with your own routes, note
; that you might need to clear the default routes out of it (added below)
(define dragonfly-routes '())

; make sure these two are defined
(if-not DOCUMENT_ROOT (throw-error "Environment variable DOCUMENT_ROOT missing!"))
(if-not QUERY_STRING (throw-error "Environment variable QUERY_STRING missing!"))

;===============================================================================
; !Load Libraries and Plugins
;===============================================================================

; load utils.lsp before loading anything else
(load (string DRAGONFLY_ROOT "/lib/utils.lsp"))

; next load lib, and plugins, in that order
(load-files-in-dir (string DRAGONFLY_ROOT "/lib") "\.lsp$")
(load-files-in-dir (string DRAGONFLY_ROOT "/plugins-active") "\.lsp$")

;===============================================================================
; !Setup Default Routes
;===============================================================================

; we want 3 basic routes:

; 1) Basic files
; 	- URL has an extension, assume it's a file and try to load it
; 2) Resources
; 	- URL begins with one of the reserved resource keywords
; 		(i.e. create/show/update/remove)
; 3) Views
; 	- Anything else. Will attempt to show one of the views in /views
; 	
; To specify a route you'll need just two things:
; 	- A filter function that returns true/nil
; 	- A function that gets called if the filter returned true
; 		it must also return true/nil which will indicate it was
; 		to send a response or not. Note however that the function
;		does not need to return actually, it can call (exit) if
;		everything went fine.

; switch to main prior to using define-subclass
(context 'MAIN)

; the static route is used to serve possibly templated files
; for example, so that you can include newLISP code in .html files
; will also handle .xml and .rss extensions
(define-subclass (Route.Static Route)
	((matches?)
		; ex: .html or .html?a=4
		(set 'file (if (empty? (set 'chunks (parse QUERY_STRING "?"))) QUERY_STRING (first chunks)))
		(if (ends-with QUERY_STRING Dragonfly:TEMPLATE_EXTENSION)
			(set 'content-type Response:html-type)
			(or (ends-with QUERY_STRING ".xml") (ends-with QUERY_STRING ".rss"))
			(set 'content-type Response:xml-type)
		)
	)
	((run)
		; pass through template TODO: make sure this is secure! no ../ bullshit!
		(DF:log-debug "Route.Static: " file)
		(Response:content-type content-type)
		(Web:eval-template (read-file file))
	)
)

(define-subclass (Route.Resource Route)
	((matches?)
		nil
	)
	((run)
		; pass along to Resource
	)
)

(define-subclass (Route.View Route)
	((matches?)
		(if (empty? QUERY_STRING)
			(set 'viewpath (string Dragonfly:VIEWS_PATH "/" Dragonfly:DEFAULTVIEW))
			(set 'viewpath (string Dragonfly:VIEWS_PATH "/" (first (parse QUERY_STRING "/"))))
		)
		(file? viewpath)
	)
	((run)
		; pass through template
		(DF:log-debug "Route.View: " viewpath)
		(set 'Dragonfly:viewname (last (parse viewpath "/")))
		(Web:eval-template (read-file viewpath))
	)
)

(define-subclass (Route.ALL Route)
	((matches?) true)
	((run)
		; 404 or redirect to home page?
		; TODO: if DEFAULT404 not found then still send something
		(DF:log-debug "Route.ALL")
		(Web:eval-template (read-file (string Dragonfly:VIEWS_PATH "/" Dragonfly:DEFAULT404)))
	)
)

(context 'Dragonfly)

(push (Route.ALL) dragonfly-routes)
(if ENABLE_VIEW_HANDLER (push (Route.View) dragonfly-routes))
(if ENABLE_RESTFUL_HANDLER (push (Route.Resource) dragonfly-routes))
(if ENABLE_STATIC_TEMPLATES (push (Route.Static) dragonfly-routes))

; TODO: these either need to be deleted or moved elsewhere
; set the paths to views and partials


;===============================================================================
; !Core Functions
;===============================================================================

; setup our error handler
(define (error-handler)
	(Response:status 500)
	(Response:content-type Response:text-type)
	(Response:send-headers)
	(MAIN:println (last (last-error))) ; TODO: make a nice template for this
	;(log-err "Got error (" (last (last-error)) ") with STDOUT contents:\n{" STDOUT "}")
	(log-err (last (last-error)))
	(exit)
)

(error-event error-handler)

;; @syntax (Dragonfly:partial <partial>)
;; @param <partial> name of partial
;; <p>Evaluates the partial and returns it.</p>
;; 
(define (partial partialname)
  	(Web:eval-template (read-file (string PARTIALS_PATH "/" partialname)))
)

(define (listener)
	(dolist (route dragonfly-routes)
		(when (:matches? route)
			(:run route)
			(Response:send-headers)
			(MAIN:println STDOUT)
			(exit)
		)
	)
)
	
(context MAIN)
