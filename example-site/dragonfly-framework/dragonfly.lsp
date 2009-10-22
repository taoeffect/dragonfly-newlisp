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
; load all our essential stuff
(load-files-in-dir (string DRAGONFLY_ROOT "/lib") "\.lsp$")
; plugins are loaded when listener is called so that they
; can modify the variables in this file is they want.

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
		(set 'ext (exists (curry ends-with file) DF:STATIC_EXTENSIONS))
	)
	((run)
		; pass through template TODO: make sure this is secure! no ../ bullshit!
		(DF:log-debug "Route.Static: " file)
		(Response:content-type (Response:extension->type ext))
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
			(set 'DF:viewname DF:DEFAULTVIEW)
			(set 'DF:viewname (first (parse QUERY_STRING "/")))
		)
		(file? (DF:view-path DF:viewname))
	)
	((run)
		; pass through template
		(DF:log-debug "Route.View: " DF:viewname)
		(DF:display-view DF:viewname)
	)
)

(context 'Dragonfly)

(if ENABLE_STATIC_TEMPLATES (push (Route.Static) dragonfly-routes -1))
(if ENABLE_RESTFUL_HANDLER (push (Route.Resource) dragonfly-routes -1))
(if ENABLE_VIEW_HANDLER (push (Route.View) dragonfly-routes -1))

;===============================================================================
; !Public Functions
;===============================================================================

(define (view-path viewname)
	(string VIEWS_PATH "/" viewname (if TEMPLATE_EXTENSION TEMPLATE_EXTENSION ""))
)

(define (partial-path partialname)
	(string PARTIALS_PATH "/" partialname (if TEMPLATE_EXTENSION TEMPLATE_EXTENSION ""))
)

;; @syntax (Dragonfly:display-partial <partial>)
;; @param <partial> name of partial
;; <p>Evaluates the partial and returns it (or nil).</p>
;;
(define (display-partial partialname)
  	(Web:eval-template (read-file (partial-path partialname)))
)

(define (display-view viewname)
	(Web:eval-template (read-file (view-path viewname)))
)

(define (display-error error-code (clear-stdout true))
	(Response:status error-code)
	(if clear-stdout (set 'STDOUT ""))
	
	(unless (display-view (string error-code))
		(log-info "display-error using ERROR_TEMPLATE for error-code " error-code)
		(Web:eval-template ERROR_TEMPLATE)
	)
)

; our main entry-point. this calls exit.
(define (listener)
	; we load these here so that they can modify any of the variables in this file
	(load-files-in-dir (string DRAGONFLY_ROOT "/plugins-active") "\.lsp$")
	
	; go through 
	(dolist (route dragonfly-routes)
		(when (:matches? route)
			(:run route)
			(send-and-exit)
		)
	)
	
	(log-info "no route matched for QUERY_STRING: " QUERY_STRING)
	(display-error 404)
	(send-and-exit)
)

;===============================================================================
; !Private Functions (i.e. you shouldn't ever call these)
;===============================================================================

(define (send-and-exit)
	(Response:send-headers)
	(MAIN:println STDOUT)
	(exit)
)

; setup our error handler
(define (error-handler)
	(Response:status 500)
	(Response:content-type Response:text-type)
	
	;(log-err "Got error (" (last (last-error)) ") with STDOUT contents:\n{" STDOUT "}")
	(log-err (last (last-error)))
	
	(set 'STDOUT "") ; clear STDOUT
	(println (last (last-error)))
	(send-and-exit)
)

(error-event error-handler)

;===============================================================================
; !Private Variables
;===============================================================================

(set 'ERROR_TEMPLATE
[text]
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title><%= (join (map string (Response:status)) " ") %></title>
</head><body>
<h1><%= (last (Response:status)) %></h1>
<p>The requested URL /<%= QUERY_STRING %> resulted in error <%= (join (map string (Response:status)) " ") %>.</p>
<p>Additionally, a 404 Not Found
error was encountered while trying to use an ErrorDocument to handle the request.</p>
</body></html>
[/text]
)

(context MAIN)
