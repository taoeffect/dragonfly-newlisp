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
;; @author Marc Hildmann <marc.hildmann at gmail.com>, Greg Slepak <greg at taoeffect.com>
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
	(println "<div id='dragonfly_benchmark'>Rendered in "execution-time-milliseconds" milliseconds. Used "mem_total_usage" KB of memory.<br/><div id='dragonfly_logo'><a href='http://code.google.com/p/dragonfly-newlisp/'>&mdash;()o Dragonfly <span class='dragonfly_uppercase'>web framework "DRAGONFLY_VERSION"</span></a></div></div>")
	
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
	<div id='dragonfly_debug'>
	<h1>Dragonfly DEBUG information</h1><br/>
	<h2>HOST</h2>" HTTP_HOST "
	<h2>DOCUMENT ROOT</h2>"DOCUMENT_ROOT"
	<h2>DRAGONFLY ROOT</h2>"DRAGONFLY_ROOT"
	<h2>Windows Programfiles</h2>"PROGRAMFILES"
	<h2>QUERY</h2>"QUERY_STRING"
	<h2>REQUEST METHOD</h2>"REQUEST_METHOD"
	<h2>DEFAULT VIEW</h2>"DEFAULT_VIEW"
	<h2>CURRENT VIEW</h2>"viewname"
	<h2>USER-AGENT</h2>"HTTP_USER_AGENT"
	<h2>Proxy</h2>"HTTP_PROXY"
	<h2>SERVER</h2>"SERVER_SOFTWARE"
	<h2>$GET</h2>"($GET)"
	<h2>$POST</h2>"($POST)"
	<h2>$SERVER</h2>"(string {<table>} (join (map (fn (x) (string {<tr><td>} (x 0) {</td><td>} (x 1) "</td></tr>")) ($SERVER))) "</table>")"
	<h2>System information</h2>
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
		<li>Used symbols in Dragonfly: <div style=\"width: 474px;\">"(symbols 'Dragonfly)"</div></li>
		<li>Last system error: "(sys-error)"</li>
	</ul>
	
	</div>")
)


;; @syntax (Dragonfly:title <websitename>)
;; @param <websitename> a string containing creen or print
;; <p>Writes a title including the current navigation entry.</p>
;; 
(define (title websitename)
  (print (string (title-case (replace "_" (copy viewname) " ")) " " websitename  ))
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
  (if (find "iPhone" HTTP_USER_AGENT)
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
	(print [text]
	<script type="text/javascript">
		var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
		document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
		</script>
		<script type="text/javascript">
		try {
			var pageTracker = _gat._getTracker("[/text] analytics-id [text]");
			pageTracker._trackPageview();
		} catch(err) {}
	</script>				
	[/text])
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
(define (link_to link-name view , link-url)
  	; if Dragonfly runs on newLISP SERVER_SOFTWARE, we cannot
  	; use .htaccess, so we've to write the "?" into the url
    ; else we miss it
	(if (find "newLISP" SERVER_SOFTWARE)
		(set 'link-url (string "?" view))
		(set 'link-url (string "/" view))
	)
  	(print "<a href=\"" (web-root link-url) "\">" link-name "</a>")
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

;; @syntax (ajax-updater <html-elementid> <request-url> <str-params> <timeout>)
;; @param <html-elementid> a string containing the elementID
;; @param <request-url> a string containing the url which is called frequently
;; @param <str-params> a string containing params which are POSTED against request-url
;; @param <int-timeout> an integer containing the number of microseconds after recalling the request-url
;; <p>Writes a simple AJAX-updater, e.g. for displaying the time on a website.</p>
(define (ajax-updater html-elementid request-url str-params timeout)
	; check for newLISP as SERVER_SOFTWARE, then we prefix a ? before request-url because there's no working .htaccess
	(when (find "newLISP" SERVER_SOFTWARE)
		(if (starts-with request-url "/") (pop request-url))
		(push "/?" request-url)
	)
	(set 'request-url (web-root request-url))
	; 'fetcher' is prevented from entering the global scope because
	; of the surrounding parenthesis. Meaning, this code won't conflict if
	; it's listed multiple times on the page. The parenthesis play two roles:
	; to protect it from entering the global scope, and to call the function.
	(print (format [text]
		<div id="%s">&nbsp;</div>
		<script type="text/javascript">
			(function fetcher() {
				var div = $('#' + '%s');
				var load_text = 'loading...';
				setTimeout(function() {
					if (load_text)
						div.html(load_text);
				}, 1000);
				$.ajax({
					url: "%s",
					data: "%s",
					type: "POST",
					timeout: 10000,
					success: function (data, status) {
						load_text = null;
						div.html(data);
						setTimeout(fetcher, %d);
					},
					error: function (xmlReq, status, error) {
						load_text = null;
						div.html('error code: ' + xmlReq.status + '<br/>' +
					 	           'error message: ' + error + '<br/>' +
						           'attempting to load: ' + this.url);
						setTimeout(fetcher, %d);
					}
				});
			})();
		</script>
		[/text] html-elementid html-elementid request-url str-params timeout timeout)
	)
)

;===============================================================================
; !RSS Functions
;===============================================================================

;; @syntax (read-atom-feed <feed-url> <max-items> <raw-xml>)
;; @param <feed-url> a string containing the URL to the ATOM feed
;; @param <max-items> INTEGER, maximum of items you want to show
;; @param <raw-xml> BOOLEAN, if true raw XML is send right back and there's no parsing
;; <p>Reads an atom feed from a given URL and displays it. There are three span classes to style Your feed:
;; atomFeedTitle, atomFeedUpdated and atomFeedAuthor.</p>

(define (read-atom-feed feed-url max-items raw-xml)

	; get feed-url
	(set 'xml (get-url (string feed-url) ))
	
	(if (true? raw-xml)
		(print xml)
		(begin
			(xml-type-tags nil nil nil nil) ; no extra tags
			(set 'sxml (xml-parse xml 31)) ; turn on SXML options
			(set 'entry-index (ref-all '(entry *) sxml match))

			(when (empty? entry-index)
				(println "The feed is empty.")
			)
			
			(dolist (idx entry-index (= max-items $idx)) 
				
				(set 'entry (sxml idx))
				(set 'dateseconds (parse-date (lookup 'updated entry) "%Y-%m-%dT%H:%M:%SZ")) ; convert string date to seconds

				(set 'contenthtml (lookup 'content entry))
				(replace "&lt;br/&gt;" contenthtml "<br/>") ; we need to replace some html entities
				(replace "&#160;" contenthtml "")

				(println
					"<span class='atomFeedTitle'><a href='" (lookup '(link @ href) entry) "' rel='nofollow'>"(lookup 'title entry) "</a></span><br/>"
					"<span class='atomFeedUpdated'>" (date dateseconds 0 "%a %d %b %Y %H:%M:%S") "</span> , <span class='atomFeedAuthor'>" (lookup '(author name) entry) "</span><br/><br/>"
					contenthtml "<br/><br/>"
				)
			)
		)
	)
	
)


;; @syntax (read-rss-feed <feed-url> <max-items> <raw-xml>)
;; @param <feed-url> a string containing the URL to the RSS feed
;; @param <max-items> INTEGER, maximum of items you want to show
;; @param <raw-xml> BOOLEAN, if true raw XML is send right back and there's no parsing
;; <p>Reads an RSS feed from a given URL and displays it. There are three span classes to style Your feed:
;; rssFeedTitle, rssFeedUpdated and rssFeedAuthor.</p>

(define (read-rss-feed feed-url max-items raw-xml)

	; get feed-url
	(set 'xml (get-url (string feed-url) ))
	
	(if (true? raw-xml)
		(print xml)
		(begin
			(xml-type-tags nil nil nil nil) ; no extra tags
			(set 'sxml (xml-parse xml 31)) ; turn on SXML options
			(set 'item-index (ref-all '(item *) sxml match))

			(when (empty? item-index)
				(println "The feed is empty.")
			)
			
			
			(dolist (idx item-index (= max-items $idx))
				(set 'item (sxml idx))
				(println
					"<span class='rssFeedTitle'><a href='" (lookup 'link item) "' rel='nofollow'>"(lookup 'title item) "</a></span><br/>"
					"<span class='rssFeedUpdated'>" (lookup 'pubDate item) "</span>&nbsp;<span class='rssFeedAuthor'>" (lookup (sym "dc:creator") item) "</span><br/><br/>"
					(lookup 'description item) "<br/>"
					
					; TODO: Problems in parsing media:content - ask the forum
					; (lookup (sym "media:content") item) "<br/>"
					;"<span class='rssFeedCategory'>Categories: " (lookup '(category) item) "</span><br/>"
					
					"<br/><br/>"
				)
			)
		)
	)
	

	
)


(context Dragonfly)
