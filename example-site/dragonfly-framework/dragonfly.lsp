;;  Copyright (C) <2009> <Marc Hildmann>
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
;; @author Marc Hildmann <marc.hildmann at gmail.com>
;; @version 0.20
;; 
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; just like newLISP itself.</p>

;===============================================================================
; !Loading modules and defining new context
;===============================================================================

(context 'Dragonfly)

; setting some constants for Dragonfly
(constant 'dragonfly_version "Version 0.20")
(constant 'host (env "HTTP_HOST"))
(constant 'documentroot (env "DOCUMENT_ROOT"))
(constant 'dragonfly-root (append (env "DOCUMENT_ROOT")"/dragonfly-framework"))
(constant 'useragent (env "HTTP_USER_AGENT"))
(constant 'server (env "SERVER_SOFTWARE"))
(constant 'programfiles (env "PROGRAMFILES"))
(constant 'proxy (env "HTTP_PROXY"))

; init HTTP Status Codes for Dragonfly listener

(constant 'http-200 "Status: 200 OK\r\n")
(constant 'http-301 "Status: 301 Moved Permanently\r\n")
(constant 'http-400 "Status: 400 Bad Request\r\n")
(constant 'http-401 "Status: 401 Unauthorized\r\n")
(constant 'http-403 "Status: 403 Forbidden\r\n")
(constant 'http-404 "Status: 404 Not Found\r\n")
(constant 'http-410 "Status: 410 Gone\r\n")
(constant 'http-500 "Status: 500 Internal Server Error\r\n")

(constant 'http-html-header "Content-Type: text/html; charset=utf-8\r\nConnection: keep-alive\r\n")
(constant 'http-xml-header "Content-Type: text/xml; charset=utf-8\r\nConnection: keep-alive\r\n")
(constant 'http-atom-header "Content-Type: application/atom+xml; charset=utf-8\r\nConnection: keep-alive\r\n")


; loading configuration files
(set 'files (directory (append dragonfly-root"/config") "^[^.]")) ; show all files which do not start with a dot
(dolist (filename files)
	(load (append dragonfly-root"/config/"filename))
)

; loading additional (3rd party) modules in active directory
(set 'files (directory (append dragonfly-root"/modules-active") "^[^.]")) ; show all files which do not start with a dot
(dolist (filename files)
	(load (append dragonfly-root"/modules-active/"filename))
)

; loading additional Dragonfly helper modules in active directory
(set 'files (directory (append dragonfly-root"/helpers-active") "^[^.]")) ; show all files which do not start with a dot
(dolist (filename files)
	(load (append dragonfly-root"/helpers-active/"filename))
)

; set the paths to views and partials
(constant 'views-path (append documentroot"/views/"))
(constant 'partials-path (append documentroot"/views/partials/"))
(constant 'databases-path (append documentroot"/databases/"))

; init symbols for Dragonfly listener
(set 'viewname "")
(set 'action "")
(set 'params "")
(set 'selector "")

;===============================================================================
; !Core Functions
;===============================================================================

;; @syntax (Dragonfly:listener)
;; <p>The Dragonfly listener parses the QUERY STRING
;; for the specified view, action and params.</p>
;;
(define (listener)

	(set 'query-list (clean empty? (parse (env "QUERY_STRING") "/|=" 0)))
	
	(cond		
			
			((ends-with query-list "xml")
				(print http-xml-header)
				;(print http-atom-header)
				(print http-200)
				(println)
				(Dragonfly:view defaultrss))

			((ends-with query-list "rss")
				(print http-xml-header)
				;(print http-atom-header)
				(print http-200)
				(println)
				(Dragonfly:view defaultrss))
			
   			((empty? query-list)
				(print http-html-header)
				(print http-200)
				(println)
        		; no info at all, so just pass to default view
       			(Dragonfly:view defaultview))

   			((= (length query-list) 1)		

				(print http-html-header)
				(print http-200)
				(println)
        		; one argument means a selector.
        		; pass the default view after setting selector
       			(set 'selector (first query-list))				
       			(Dragonfly:view defaultview))

   			((> (length query-list) 1 )

				(print http-html-header)
				(print http-200)
				(println)
				
      			; = 2 a view, followed by an action
				; = 3 a view, followed by an action, followed by params
				(map set '(viewname action params) query-list)
       			; (println  { template: } viewname { action: } action { params: } params)
       			; (params will be nil if length is 2)
				
       			(Dragonfly:view viewname))

	)
)


(define (listener2)

	; at first get the query
	(set 'query-list (clean empty? (parse (env "QUERY_STRING") "/|=" 0)))
	; then map the query to view, action and params
	(map set '(viewname action params) query-list)
	
	; check for all existing views ...
	(set 'files (directory views-path "^[^.]")) ; show all files which do not start with a dot
	; if the current view doesn't exist, then throw a 404

	(print http-html-header)
	(print http-200)
	(println)
	
	(println files)
	(exit)
	
	(if (nil? (find viewname files))
		(begin
			(print "Status: 404\r\n")
			(println)
			(print http-html-header)
			(Dragonfly:view "404")
		)
		; else check some conditions ...
		(cond

				((ends-with query-list "xml")
					(print http-xml-header)
					;(print http-atom-header)
					(print http-200)
					(println)

					(Dragonfly:view defaultrss))

				((ends-with query-list "rss")
					(print http-xml-header)
					;(print http-atom-header)
					(print http-200)
					(println)

					(Dragonfly:view defaultrss))

		   		((empty? query-list)

					(print http-html-header)
					(print http-200)
					(println)
		        	; no info at all, so just pass to default view
		       		(Dragonfly:view defaultview))

		   		((= (length query-list) 1)		

					(print http-html-header)
					(print http-200)
					(println)
		        	; one argument means a selector.
		        	; pass the default view after setting selector
		       		(set 'selector (first query-list))				
		       		(Dragonfly:view defaultview))

		   		((> (length query-list) 1 )

					(print http-html-header)
					(print http-200)
					(println)
					(Dragonfly:view viewname))


		) ; end of conditions
		
	)
	
)

;; @syntax (Dragonfly:benchmark-start)
;; <p>Sets the start point for benchmarking.</p>
;; 
(define (benchmark-start)
	(set 'microtime-start (time-of-day))
)

;; @syntax (Dragonfly:benchmark-end)
;; <p>Sets the end point for benchmarking and calculates the result in milliseconds plus
;; some information about memory usage.</p>
;; 
(define (benchmark-result)

  	(set 'mem_cells_bytes (* (sys-info 0) 16))
  	(set 'mem_cells_kilobytes (/ mem_cells_bytes 1024))

  	(set 'mem_cells-constant_bytes (* (sys-info 1) 16))
  	(set 'mem_cells-constant_kilobytes (/ mem_cells-constant_bytes 1024))
  	(set 'mem_cells-constant_megabytes (/ mem_cells-constant_kilobytes 1024))

  	(set 'mem_symbols_bytes (* (sys-info 2) 32))
  	(set 'mem_symbols_kilobytes (/ mem_symbols_bytes 1024))

    (set 'mem_total_usage (+ mem_cells_kilobytes mem_symbols_kilobytes))
  
	(set 'microtime-end (time-of-day))
	(set 'execution-time-milliseconds (- microtime-end microtime-start))
	(set 'execution-time-seconds (div execution-time-milliseconds 1000))
	(println "<div id='dragonfly_benchmark'>Rendered in "execution-time-milliseconds" milliseconds. Used "mem_total_usage" KB of memory.<br/><div id='dragonfly_logo'><a href='http://code.google.com/p/dragonfly-newlisp/'>&mdash;()o Dragonfly <span class='dragonfly_uppercase'>web framework "dragonfly_version"</span></a></div></div>")
	
)

;; @syntax (Dragonfly:debugging)
;; <p>Writes some debug information to the screen. Requires Web.lsp module for POST and GET information.</p>
;; 
(define (debugging)

  (set 'mem_cells_bytes (* (sys-info 0) 16))
  (set 'mem_cells_kilobytes (/ mem_cells_bytes 1024))

  (set 'mem_cells-constant_bytes (* (sys-info 1) 16))
  (set 'mem_cells-constant_kilobytes (/ mem_cells-constant_bytes 1024))
  (set 'mem_cells-constant_megabytes (/ mem_cells-constant_kilobytes 1024))

  (set 'mem_symbols_bytes (* (sys-info 2) 32))
  (set 'mem_symbols_kilobytes (/ mem_symbols_bytes 1024))

  (set 'mem_total_usage (+ mem_cells_kilobytes mem_symbols_kilobytes))

  (println "
	<div id='dragonfly_debug' style='width:474px; margin-top:20px;' >
	<h1>Dragonfly DEBUG information</h1><br/>
	<h2>HOST</h2>"host"
	<h2>DOCUMENT ROOT</h2>"documentroot"
	<h3>DRAGONFLY ROOT</h3>"dragonfly-root"
	<h3>Windows Programfiles</h3>"programfiles"
	<h3>QUERY</h3>"(env "QUERY_STRING")"
	<h3>DEFAULT VIEW</h3>"defaultview"
	<h3>DEFAULT ACTION</h3>"defaultaction"
	<h3>CURRENT VIEW</h3>"viewname"
	<h3>CURRENT ACTION</h3>"action"
	<h3>CURRENT SELECTOR</h3>"selector"
	<h3>CURRENT PARAMS</h3>"params"
	<h3>USER-AGENT</h3>"useragent"
	<h3>Proxy</h3>"proxy"
	<h3>SERVER</h3>"server"
	<h3>POST</h3>"Web:POST"
	<h3>GET</h3>"Web:GET"
	<h3>System information</h3>
	<ul>
		<li>Total memory usage: "mem_total_usage" KB</li>	
		<li>Number of Lisp Cells (16 bytes per cell): "(sys-info 0)"</li>
		<li>Memory used by Lisp Cells: "mem_cells_kilobytes" KB</li>
		<li>Maximum number of Lisp cells constant: "(sys-info 1)"</li>
		<li>Maximum memory used by Lisp cells constant: "mem_cells-constant_megabytes" MB</li>
		<li>Number of symbols (32 bytes per symbol): "(sys-info 2)"</li>
		<li>Memory used by symbols: "mem_symbols_kilobytes" KB</li>
		<li>Evaluation/recursion level: "(sys-info 3)"</li>
		<li>Environment stack level: "(sys-info 4)"</li>
		<li>Maximum call stack constant: "(sys-info 5)"</li>
		<li>Pid of running newLISP process: "(sys-info 6)"</li>
		<li>Version number as an integer constant: "(sys-info 7)"</li>
		<li>Operating system constant: "(sys-info 8)"</li>
		<li>Used symbols in Dragonfly: <pre>"(symbols 'Dragonfly)"</pre></li>
		<li>Last system error: "(sys-error)"</li>
	</ul>
	
	</div>")
)


;; @syntax (Dragonfly:view <view>)
;; @param <viewname> name of view
;; <p>Evaluates the view and returns it.</p>
;; 
(define (view viewname)
	(set 'path-to-views views-path)
    (push viewname path-to-views -1)
	(if (nil? (read-file path-to-views))
		(begin
			(set 'path-to-error-views views-path)
			(push default404 path-to-error-views -1)
			(Web:eval-template (read-file path-to-error-views))	
		)
			(Web:eval-template (read-file path-to-views))				
	)
)

;; @syntax (Dragonfly:partial <partial>)
;; @param <partial> name of partial
;; <p>Evaluates the partial and returns it.</p>
;; 
(define (partial partialname)
  	(Web:eval-template (read-file (append partials-path partialname)))
)

;; @syntax (Dragonfly:title <websitename>)
;; @param <websitename> a string containing creen or print
;; <p>Writes a title including the current navigation entry.</p>
;; 
(define (title websitename)
  (print (append (title-case (replace "_" viewname " ")) " " websitename  ))
)

;; @syntax (Dragonfly:css <css-media> <css-location>)
;; @param <css-media> a string containing screen or print
;; @param <css-location> the location of your stylesheet
;; <p>Writes a standard stylesheet HTML tag.</p>
;; 
(define (css css-media css-location)
  (print "<link rel='stylesheet' type='text/css' media='"css-media"' href='"css-location"' />")
)

;; @syntax (Dragonfly:meta <meta-name> <meta-content>)
;; @param <meta-name> a string containing the meta-name
;; @param <meta-content> a string containing the meta content
;; <p>Writes a standard meta HTML tag.</p>
;; 
(define (meta meta-name meta-content)
  (print "<meta name='"meta-name"' content='"meta-content"' />")
)

;; @syntax (Dragonfly:rss <rss-title> <rss-location>)
;; @param <rss-title> a string containing the rss title
;; @param <rss-location> a string containing the rss location
;; <p>Writes a standard RSS HTML tag.</p>
;; 
(define (rss rss-title rss-location)
  (print "<link rel='alternate' type='application/rss+xml' title='"rss-title"' href='"rss-location"' />")
)

;; @syntax (Dragonfly:script <script-location>)
;; @param <script-location> a string containing the script location
;; <p>Writes a standard script HTML tag.</p>
;; 
(define (script script-location)
  (print "<script type='text/javascript' src='"script-location"'></script>")
)

;; @syntax (Dragonfly:autoload-css css-media css-screen css-iphone)
;; @param <css-media> a string containing the css media type
;; @param <css-screen> a string containing the path to screen stylesheet
;; @param <css-iphone> a string containing the path to iPhone stylesheet
;; <p>Detects the iPhone and loads the apropiate CSS.</p>
;; 
(define (autoload-css css-media css-screen css-iphone)
  (if (find "iPhone" useragent)
	(print "<!-- found iPhone --><meta name='viewport' content='width=320, user-scalable=yes' /><link rel='stylesheet' type='text/css' media='"css-media"' href='"css-iphone"' />")
	(print "<!-- no iPhone detected ... loading screen stylesheets --><link rel='stylesheet' type='text/css' media='"css-media"' href='"css-screen"' />")
  )
)


;; @syntax (Dragonfly:compare-lists <list1> <list2>)
;; @param <list1> list number 1
;; @param <list2> list number 2
;; <p>Compares two lists and return the score of same and same position. This is a function by cormullion.</p>
;;
(define (compare-lists list1 list2)
	(print "Comparing " list1 " and " list2 " ...")
	(print " "(first (count '(true) (map = list1 list2))) " elements are the same and in the same position.")
)

;; @syntax (Dragonfly:google-analytics <analytics-id>)
;; @param <analytics-id> enter the specified id provided by Google Analytics, e.g. UA-123456-7
;; <p>Writes the Google Analytics tracking code.</p>
;;
(define (google-analytics analytics-id)
	(print "
	<script type=\"text/javascript\">
		var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
		document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
		</script>
		<script type=\"text/javascript\">
		try {
			var pageTracker = _gat._getTracker(\""analytics-id"\");
			pageTracker._trackPageview();
		} catch(err) {}
	</script>				
	")
)



;===============================================================================
; !Date Functions
;===============================================================================

;; @syntax (Dragonfly:todays-date-german including actual time)
;; <p>Writes todays date including time in german localization</p>
;; 
(define (todays-date-german)
	(set-locale "de_DE")
	(print (date (date-value) 0 "%A, den %d. %B %Y um %H:%M:%S Uhr"))
)

;; @syntax (Dragonfly:time-now)
;; <p>Writes the actual time</p>
;; 
(define (time-now)
	(print (date (date-value) 0 "%H:%M:%S"))
)


;===============================================================================
; !Image Functions
;===============================================================================

;; @syntax (Dragonfly:image <image_name> <image_url>, <options>)
;; @param <image_name> a string containing the image alternative title
;; @param <image_url> a string containing the url
;; @param <options> optional settings like class, rel, width, height ...
;; <p>Writes a standard HTML image.</p>
;; 
(define (image image-name image-url, image-options)
  (print "<img src='"image-url"' alt='"image-name"' title='"image-name"' border='0' "image-options" />")
)


;===============================================================================
; !Link Functions
;===============================================================================

;; @syntax (Dragonfly:link_to <link_name> <view>)
;; @param <link_name> a string containing the link's name
;; @param <view> a string containing the view
;; <p>Writes a internal link</p>
;; 
(define (link_to link-name view action)
	
  	; if Dragonfly runs on newLISP webserver, we cannot
  	; use .htaccess, so we've to write the "?" into the url
    ; else we miss it
	(if (true? (find "newLISP" server))
		(set 'link-url (append "?" view "/" action))
		(set 'link-url (append "/" view "/" action))
	)
	
  	(print "<a href='"link-url"'>"link-name"</a>")
)

;; @syntax (Dragonfly:link_to <link_name> <url>)
;; @param <link_name> a string containing the link's name
;; @param <url> a string containing the target URL
;; <p>Writes a standard HTML link</p>
;; 
(define (link_to_external link-name url)
		
  	(print "<a href='"url"'>"link-name"</a>")

)

;; @syntax (Dragonfly:link_mailto <link_name> <options>)
;; @param <name> a string containing the link's name
;; @param <options> a string containing the url
;; <p>Writes a standard HTML mailto link</p>
;; 
(define (link_mailto link-name link-url)
  (print "<a href='mailto:"link-url"'>"link-name"</a>")
)


;===============================================================================
; !AJAX Functions
;===============================================================================

;; @syntax (Dragonfly:ajax-updater <html-elementid> <request-url> <params-url> <timeout>)
;; @param <html-elementid> a string containing the elementID
;; @param <request-url> a string containing the url which is called frequently
;; @param <params-url> a string containing params which are POSTED against request-url
;; @param <timeout> an integer containing the number of microseconds after recalling the request-url
;; <p>Writes a simple AJAX-updater, e.g. for displaying the time on a website.</p>
;;
(define (ajax-updater html-elementid request-url params-url timeout)
	(print "<div id='"html-elementid"'>&nbsp;</div>")
	(print "<script language='javascript'>")
	(print "function responseFunction(responseText, responseStatus) {")
	(print "var response = responseText;")
	(print "document.getElementById('"html-elementid"').innerHTML = response;")	
	(print "setTimeout(\"ajax"html-elementid".post('"params-url"');\","timeout");")
	(print "}")
	
	; check for newLISP as webserver, then we've to use a ? before request-url, because there's no working .htaccess
	(if (true? (find "newLISP" server))
		(print "var ajax"html-elementid" = new AjaxRequest(\"?"request-url"\", responseFunction);")
		(print "var ajax"html-elementid" = new AjaxRequest(\""request-url"\", responseFunction);")
	)
	
	(print "ajax"html-elementid".post(\""params-url"\");")
	(print "</script>")
)
		

(context MAIN)