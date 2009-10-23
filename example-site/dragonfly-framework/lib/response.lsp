;; @author Greg Slepak

(context 'Response)

;===============================================================================
; !Public API
;===============================================================================

(define (status code description)
	(if code
		(begin
			(unless (assoc code status-codes)
				(push (list code description) status-codes))
			(setf status-code code)
		)
		(assoc status-code status-codes)
	)
)

;; @syntax (Request:header <str-key>)
;; @param <str-key> the header's name
;; 
;; @syntax (Request:header <str-key> <str-value>)
;; @param <str-key> the header's name
;; @param <str-value> the header's value
;; <p>In the first syntax, returns the header matching <str-key> or,
;; if <str-key> is nil, all of the headers in a list</p>
;; <p>In the second syntax, sets or updates the header matching <str-key> or,
;; if <str-value> is nil, deletes the header for <str-key>.</p>
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

;; @syntax (Request:cookie <str-key>)
;; @param <str-key> the cookie's name
;; 
;; @syntax (Request:cookie <str-key> <str-value> [<int-expires> [<str-path> [<str-domain> [<bool-http-only>]]]])
;; @param <str-key> the cookie's name
;; @param <str-value> the cookie's value
;; @param <int-expires> (optional) the expiration date of the cookie as a unix timestamp; default is a session cookie
;; @param <str-path> (optional) the cookie's path; default is the current path
;; @param <str-domain> (optional) the cookie's domain; default is the current host
;; @param <bool-http-only> (optional) whether the cookie may be read by client-side scripts
;; <p>In the first syntax, 'cookie' returns the value of the cookie named <str-key> or 'nil'. If
;; <str-key> is not provided, an association list of all cookie values is returned.</p>
;; <p>In the second syntax, 'cookie' sets a new cookie. If <str-value> is nil then any existing
;; cookie is deleted, otherwise it is updated with the value and the rest of the parameters.</p>
(define (cookie key)
	(local (value expires path domain http-only)
		(map set '(value expires path domain http-only) $args)
		(if (nil? key) cookies
			(empty? $args) (lookup key cookies)
			(nil? value) (pop cookies (find key cookies comp-func))
			(let (cookie (list key value expires path domain http-only))
				(if (assoc key cookies)
					(setf (assoc key cookies) cookie)
					(push cookie cookies -1)
				)
			)
		)
	)
)

(define (send-headers)
	(print "Status: " status-code " " (lookup status-code status-codes) "\r\n")
	(dolist (header headers) (print (first header) ": " (last header) "\r\n"))
	(dolist (cookie cookies) (print "Set-Cookie: " (apply format-cookie cookie) "\r\n"))
	(print "\r\n")
)

;===============================================================================
; !Public Convenience Functions and Variables
;===============================================================================

(define (redirect path)
	(header "Location" path)
	(status 302)
	(send-headers)
	(exit)
)

(define (send-headers-with-status code description)
	(status code description)
	(send-headers)
)

(define (content-type value)
	(if value
		(header "Content-Type" value)
		(header "Content-Type")
	)
)

(define (extension->type file-extension)
	(if-not (starts-with file-extension ".") (push "." file-extension))
	(eval (lookup file-extension extension-to-type-map))
)

(constant
	'text-type "text/plain; charset=utf-8"  'xml-type "text/xml"
	'html-type "text/html; charset=utf-8"   'js-type "application/javascript"
	'atom-type "application/atom+xml"       'css-type "text/css"
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
		(if expires (write-buffer cookie (string "; expires=" (date expires 0 "%a, %d %b %Y %H:%M:%S %Z"))))
		(if path (write-buffer cookie (string "; path=" path)))
		(if domain (write-buffer cookie (string "; domain=" domain)))
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
