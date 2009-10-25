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

;===============================================================================
; !Basic Setup, Global Vars, and Sanity Checks
;===============================================================================

; $SERVER is a synonym for env, for $GET, $POST, and $FILES see lib/request.lsp
(constant (global '$SERVER) env)

; DF is a convenient shorthand to the Dragonfly context
(constant (global 'DF) Dragonfly)

; make sure these two are defined
(if-not DOCUMENT_ROOT (throw-error "Environment variable DOCUMENT_ROOT missing!"))
(unless QUERY_STRING
	(constant (global 'QUERY_STRING) "")
	(env "QUERY_STRING" QUERY_STRING)
)

; seed the random number generator immediately.
(seed (time-of-day))

(context 'Dragonfly)

;===============================================================================
; !Public Constants and Variables
;===============================================================================

; These are some of the few constants that we'll have outside of env
(constant 'DRAGONFLY_MAJOR 0)
(constant 'DRAGONFLY_MINOR 50)
(constant 'DRAGONFLY_VERSION (format "Version %d.%d" DRAGONFLY_MAJOR DRAGONFLY_MINOR))

; This is the buffer that contains the content that will get written
; to STDOUT if no errors are thrown. In the Dragonfly context 'print'
; and 'println' are overridden to write to this buffer.
(define STDOUT "")

; you can customize this variable with your own routes, note
; that you might need to clear the default routes out of it (added below)
(define dragonfly-routes '())

;===============================================================================
; !Load Libraries and Plugins
;===============================================================================

; load utils.lsp before loading anything else
(load (string DRAGONFLY_ROOT "/lib/utils.lsp"))
; load all our essential stuff
(load-files-in-dir (string DRAGONFLY_ROOT "/lib") "\.lsp$")
; plugins are loaded when listener is called so that they
; can modify the variables in this file if they want.
; you can also load the inactive plugins on a need-to-load basis
; by using the 'activate-plugin' function.

;===============================================================================
; !Public Functions
;===============================================================================

;; @syntax (Dragonfly:activate-plugin <plugin-name-1> [<plugin-name-2> ...])
;; @param <plugin-name-1> The name of the plugin to load, without the ".lsp" extension.
;; <p>Loads (once only) an inactive plugin(s). Quite often you'll only want some plugins
;; loaded when 'listener' is called, and only sometimes you'll need to load a
;; specific plugin. This can speed things up, especially if the plugin is large.</p>
(define (activate-plugin)
	(doargs (plugin-name)
		(load-once (string DRAGONFLY_ROOT "/plugins-inactive/" plugin-name ".lsp"))
	)
)

;; @syntax (Dragonfly:web-root <path>)
;; <p>web-root is used to make things work nicely if the site isn't
;; located at DOCUMENT_ROOT but in a subdirectory of it. Instead
;; of including a link to "/welcome", you'd use (web-root "welcome")</p>
(define (web-root path)
	; WEB_ROOT should have a "/" on the end
	(if (starts-with path "/") (pop path))
	(string WEB_ROOT path)
)

(define (view-path view-name)
	(string VIEWS_PATH "/" view-name (if VIEW_EXTENSION VIEW_EXTENSION ""))
)

(define (partial-path partial-name)
	(string PARTIALS_PATH "/" partial-name (if VIEW_EXTENSION VIEW_EXTENSION ""))
)

(define (resource-path resource-name)
	(string RESOURCES_PATH "/" resource-name ".lsp")
)

;; @syntax (Dragonfly:include)
;; <p>String-concats its arguments to form a path and displays the file inline
;; without evaluating it as a template.</p>
;; @see (Dragonfly:display-file)
(define (include)
	(print (read-file (apply string $args)))
)

;; @syntax (Dragonfly:display-file)
;; <p>String-concats its arguments and displays the file
;; at that path after passing it through 'eval-template'.</p>
(define (display-file)
	(eval-template (read-file (apply string $args)))
)

(define (display-partial partialname)
  	(display-file (partial-path partialname))
)

(define (display-view viewname)
	(display-file (view-path viewname))
)

(define (display-error error-code (clear-stdout true))
	(Response:status error-code)
	(Response:content-type Response:html-type)
	(if clear-stdout (set 'STDOUT ""))
	
	(unless (display-view (string error-code))
		(log-info "display-error using ERROR_TEMPLATE for error-code " error-code)
		(eval-template ERROR_TEMPLATE)
	)
)

(define (eval-template str (ctx Dragonfly) , start end block (buf ""))
	(while (and (setf start (find OPEN_TAG str)) (setf end (find CLOSE_TAG str)))
		(write-buffer buf (string "(print [text]" (slice str 0 start) "[/text])"))
		(setf block (slice str (+ start 2) (- end start 2)))
		(if (starts-with block "=")
			(write-buffer buf (string "(print " (rest block) ")"))
			(write-buffer buf block)
		)
		(setf str (slice str (+ end 2)))
	)
	(when str
		(write-buffer buf (string "(print [text]" str "[/text])"))
		(eval-string buf ctx)
	)
)

(define (die)
	(let (msg (apply string $args))
		(log-err msg)
		(throw-error msg)
	)
)

; our main entry-point. this calls exit.
(define (listener)
	; we load these here so that they can modify any of the variables in this file
	(load-files-in-dir (string DRAGONFLY_ROOT "/plugins-active") "\.lsp$")
	
	; go through all the routes, if one matches, run it and we're done!
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
; !Setup Default Routes
;===============================================================================

; switch to main prior to using define-subclass
(context 'MAIN)

; Define the base Route from which all Routes inherit.
(new Class 'Route)
(define (Route:matches?) nil)
(define (Route:run) nil)

; Route.Static handles "normal" URLs, i.e. the URL represents the actual
; location of the file. Two scenarios are handled:
; 1) URL refers to real file ending in one of the STATIC_EXTENSIONS
; 2) URL refers to real directory and has an index file ending in STATIC_INDEX_EXTENSION
(define-subclass (Route.Static Route)
	((matches?)
		(set 'chunks (parse QUERY_STRING "?"))
		(when (not (empty? chunks))
			; ex: .html or .html?a=4
			(set 'file (first chunks))
			; check if 'file' has one of the static extensions. If not, it could
			; be a directory with an index file inside of it, so check that.
			(unless (set 'ext (exists (curry ends-with file) DF:STATIC_EXTENSIONS))
				(set 'ext DF:STATIC_INDEX_EXTENSION)
				(set 'file (string DOCUMENT_ROOT "/" file "/index" ext))
			)
			; finally, we match only if the file actually exists
			; this allows us to support without conflict twitter-like RESTful URLs with format specifiers
			(file? file)
		)
	)
	((run)
		; pass through template TODO: make sure this is secure! no ../ bullshit!
		(DF:log-debug (context) ": " file)
		(Response:content-type (Response:extension->type ext))
		(unless (DF:eval-template (read-file file))
			(DF:display-error 404)
		)
	)
)

; Route.Resource handles URLs that refer to RESTful resources, represented
; as newLISP contexts. These resources reside in the RESOURCES_PATH as .lsp files.
; The URL works in a similar manner to twitter's RESTful API:
; http://mysite.com/<resource_name>[/resource_action][.restponse_format][?get_paramters]
; <resource_name> maps to a context name in a special way: first "Resource." is prepended
; to the name, then the underscores are removed and the name is mapped to title case.
; ex: resource_name => Resource.ResourceName
; The name also maps to a real file located in RESOURCES_PATH by appending ".lsp" to the name:
; ex: resource_name => load file: RESOURCES_PATH/resource_name.lsp
; If <resource_name> implements <resource_action>, then that function is called
; optionally passing in the <response_format> in as a paramter (if it was given).
; If no <resource_action> is specified, then the resource's default function is called instead.
; <resource_name>, <resource_action> and <response_format> may only contain alphanumeric
; characters and the underscore.
(define-subclass (Route.Resource Route)
	((matches?)
		(when (regex {^(\w+)(/(\w+))?(\.(\w+))?} QUERY_STRING)
			(set 'resource_name $1 'resource_action $3 'response_format $5)
			(file? (set 'path (DF:resource-path resource_name)))
		)
	)
	((run)
		(load path)
		(set 'ctx-str (string "Resource." (join (map title-case (parse resource_name "_")))))
		(set 'ctx-sym (sym ctx-str))
		
		; If no action is specified, use the default function
		(if (null? resource_action) (set 'resource_action ctx-str))
		(set 'action (eval (sym resource_action ctx-sym)))
		
		(if-not (lambda? action) (DF:die ctx-str ":" resource_action " not defined!"))
		
		; call the action on the resource with the optional response_format
		(action (if-not (null? response_format) response_format))
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
		(DF:log-debug (context) ": " DF:viewname)
		(DF:display-view DF:viewname)
	)
)

(context 'Dragonfly)

(if ENABLE_STATIC_TEMPLATES (push (Route.Static) dragonfly-routes -1))
(if ENABLE_RESTFUL_HANDLER (push (Route.Resource) dragonfly-routes -1))
(if ENABLE_VIEW_HANDLER (push (Route.View) dragonfly-routes -1))

;===============================================================================
; !Private Functions (i.e. you shouldn't ever call these)
;===============================================================================

(define (send-and-exit)
	(Response:send-headers)
	(sys-print STDOUT)
	(exit)
)

; setup our error handler
(define (error-handler)
	;(log-err "Got error (" (last (last-error)) ") with STDOUT contents:\n{" STDOUT "}")
	(log-err (last (last-error)))
	(display-error 500)
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

(set 'WEB_ROOT (slice DOCUMENT_ROOT (length ORIGINAL_ROOT)))
(push "/" WEB_ROOT -1)

(context MAIN)
