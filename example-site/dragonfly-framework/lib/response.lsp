;; @module response.lsp
;; @author Greg Slepak

(context 'Response)

;===============================================================================
; !Public API
;===============================================================================

;; @syntax (Response:status)
;; @syntax (Response:status <int-code> [<str-description>])
;; <p>In the first syntax, returns a list containing the current status code and corresponding description.</p>
;; <p>In the second syntax, sets the response status to <int-code>, using the description from
;; a built-in list. If the description for <int-code> is not found in the built-in
;; list then an error will be thrown if <str-description> is not provided.</p>
(define (status code description)
	(if code
		(begin
			(unless (assoc code status-codes)
				(if-not description
					(throw-error (string "Unknown status code " code ". Please provide a description.")))
				(push (list code description) status-codes))
			(setf status-code code)
		)
		(assoc status-code status-codes)
	)
)

;; @syntax (Response:header <str-key>)
;; @param <str-key> the header's name<br>
;; @syntax (Response:header <str-key> <str-value>)
;; @param <str-key> the header's name
;; @param <str-value> the header's value
;; <br>In the first syntax, returns the header matching <str-key> or,
;; if <str-key> is nil, all of the headers in an association list.
;; <br>In the second syntax, sets or updates the header matching <str-key> or,
;; if <str-value> is nil, deletes the header for <str-key>.
(define (header key)
	(if (nil? key) headers
		(empty? $args) (lookup key headers)
		(let (value (first $args))
			(if value
				(if (assoc key headers)
					(setf (lookup key headers) value)
					(push (list key value) headers -1)
				)
				(pop headers (find key headers comp-func))
			)
		)
	)
)

;; @syntax (Response:cookie <str-key>)
;; @param <str-key> the cookie's name<br>
;; @syntax (Response:cookie <str-key> <str-value> [<int-expires> [<str-path> [<str-domain> [<bool-http-only>]]]])
;; @param <str-key> the cookie's name
;; @param <str-value> the cookie's value
;; @param <int-expires> (optional) the expiration date of the cookie as a unix timestamp; default is a session cookie
;; @param <str-path> (optional) the cookie's path; default is the current path
;; @param <str-domain> (optional) the cookie's domain; default is the current host
;; @param <bool-http-only> (optional) whether the cookie may be read by client-side scripts
;; <p>In the first syntax, 'cookie' returns the value of the cookie named <str-key> or 'nil'. If
;; <str-key> is not provided, an association list of all cookie key/value pairs is returned.</p>
;; <p>In the second syntax, 'cookie' sets a new cookie. If <str-value> is nil then any existing
;; cookie is deleted, otherwise it is updated with the value and the rest of the parameters.</p>
(define (cookie key)
	(local (value expires path domain http-only)
		(map (fn(k v) (if v (set k v))) '(value expires path domain http-only) $args)
		(if (nil? key) cookies
			(empty? $args) (lookup key cookies)
			(nil? value) (cookie key "" (date-value))
			(let (cookie (list key value expires path domain http-only))
				(if (assoc key cookies)
					(setf (assoc key cookies) cookie)
					(push cookie cookies -1)
				)
			)
		)
	)
)

;; @syntax (Response:send-headers)
;; <p>Actually sends the headers (without buffering them to 'Dragonfly:STDOUT').
;; Normally you should never call this yourself!</p>
(define (send-headers)
	(sys-print "Status: " status-code " " (lookup status-code status-codes) "\r\n")
	(dolist (header headers) (sys-print (first header) ": " (last header) "\r\n"))
	(dolist (cookie cookies) (sys-print "Set-Cookie: " (apply format-cookie cookie) "\r\n"))
	(sys-print "\r\n")
)

;===============================================================================
; !Public Convenience Functions and Variables
;===============================================================================

;; @syntax (Response:redirect <str-url>)
;; @param <str-url> The URL you'd like to send them to
;; <p>Does an immediate 302 Found redirect and calls 'exit'.</p>
(define (redirect url)
	(header "Location" url)
	(status 302)
	(send-headers)
	(exit)
)

;; @syntax (Response:send-headers-with-status <int-code> <str-description>)
;; <p>Convenience. Combines a call to 'Response:status' and 'Response:send-headers'.
;; As this calls 'send-headers', you typically do not want to call this yourself!</p>
(define (send-headers-with-status code description)
	(status code description)
	(send-headers)
)

;; @syntax (Response:content-type <str-value>)
;; <p>Convenience for calling '(Response:header "Content-Type" str-value)'.</p>
;; <p>If <str-value> is nil, returns the current content-type value</p>
(define (content-type value)
	(if value
		(header "Content-Type" value)
		(header "Content-Type")
	)
)

;; @syntax (Response:extension->type <str-file-extension>)
;; <p>Given a file extension (with or without a preceding dot), returns the
;; MIME-type for that extension. Currently only a small number of file extensions
;; are supported by default, see the source in lib/response.lsp for a complete list.</p>
(define (extension->type file-extension)
	(if-not (starts-with file-extension ".") (push "." file-extension))
	(eval (lookup file-extension extension-to-type-map))
)

(constant
	'text-type "text/plain; charset=utf-8"  'xml-type "text/xml"
	'html-type "text/html; charset=utf-8"   'js-type "application/javascript"
	'atom-type "application/atom+xml"       'css-type "text/css"
	'json-type "application/json"
)

(set 'extension-to-type-map
  '((".html" html-type)  (".txt" text-type)
	(".xml" xml-type)    (".js" js-type)   
	(".rss" xml-type)    (".css" css-type))
)

;===============================================================================
; !Private Functions
;===============================================================================

; we do *NOT* want to use url-encode on the value
; that's something the user can do if they want to.
; these parameters must match the order in the 'cookie' function.
(define (format-cookie key value expires path domain http-only)
	(let (cookie (string key "=" value))
		(if expires (extend cookie (string "; expires=" (date expires 0 "%a, %d %b %Y %H:%M:%S %Z"))))
		(if path (extend cookie (string "; path=" path)))
		(if domain (extend cookie (string "; domain=" domain)))
		cookie
	)
)

; this is used by the cookie and header functions
(define (comp-func x y)
	(= x (y 0))
)

;===============================================================================
; !Private Variables
;===============================================================================

; common status codes, you can easily add your own using Response:status
(set 'status-codes
  '((200 "OK")
	(301 "Moved Permanently")
	(302 "Found")
	(400 "Bad Request")
	(401 "Unauthorized")
	(403 "Forbidden")
	(404 "Not Found")
	(410 "Gone")
	(500 "Internal Error"))
)

(set 'headers '())
(set 'cookies '())
(set 'status-code 200)

(content-type html-type)
(header "Connection" "keep-alive")

(context MAIN)
