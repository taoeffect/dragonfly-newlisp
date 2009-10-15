(context 'Response)

;; mark Public API

(define (Response:Response str)
	(_response 200 str)
)

(define (redirect path)
	(header "Location" path)
	(_response 302)
)

(define (not-found str)
	(_response 404 str)
)

(define (error str)
	(_response 500 str)
)

;; mark Headers

;; add the header with key and associated value to the list of headers
;; replaces the old value if key is already in there
(define (header key val)
	(set 'key (join (map title-case (parse key "-")) "-"))
	(if (member key _headers)
    	(setf (assoc key _headers) (list key val))
    	(push (list key val) _headers)
	)
)

(define (header? key)
	(lookup key _headers)
)

(define (headers)
	_headers
)


;; mark Cookies

(define (set-cookie key value domain path expires)
	(if (cookie-set? key '? domain path)
		(delete-cookie key domain path)
	)
	(push (list key value domain path expires) _cookies -1)
)


;; needs to check for set cookies in _cookies and remove
(define (delete-cookie key domain path)
	(if (cookie-set? key '? domain path)
		(pop _cookies (find (list key '? domain path '?) _cookies match))
		(set-cookie key nil domain path (date-value))
	)
)

; NOTE: bug fixed: definition was (cookie-set? key domain path)
(define (cookie-set? key value domain path)
	(true? (find (list key value domain path '?) _cookies match))
)

;; mark Private API

; returns a string version ready for sending to browser of the cookie
(define (_format-cookie key value domain path expires)
	;; expires must be timestamp (use date-value)
	(set 'value (if value (string value) ""))
	(let (cookie "")
		(write-buffer cookie (format "%s=%s" key value))
		(if expires (write-buffer cookie (format "; expires=%s" (date (int expires) 0 "%a, %d %b %Y %H:%M:%S %Z"))))
		(if path (write-buffer cookie (format "; path=%s" path)))
		(if domain (write-buffer cookie (format "; domain=%s" domain)))
		cookie
	)
)

; hack to get it work on both newlisp and apache because of bug in newlisp
(define (print-header code , header)
	; (if (find "newLISP" (env "SERVER_SOFTWARE"))
	; 	(println "HTTP/1.0 " code " " (lookup code _response-codes) "\r\n")
	; 	(println "Status: " code " " (lookup code _response-codes) "\r\n")
	; )
	
	(print "Status: " code " " (lookup code _response-codes) "\r\n")
	
	; (set 'header (string code " " (lookup code _response-codes)))
	; (set '$status-header (append "HTTP/1.0 " header "\r\n")) ; for newlisp
	; (println "Status: " header)
)

;; http://en.kioskea.net/contents/internet/http.php3
;; http://hoohoo.ncsa.uiuc.edu/cgi/out.html

;; NOTE: completely changed
(define (_response code content)
	; (print-header code)
	(print "Status: " code " " (lookup code _response-codes) "\r\n")
	(dolist (hdrs _headers) (print (hdrs 0) ": " (hdrs 1) "\r\n"))
	(dolist (cookie _cookies) (print "Set-Cookie: " (apply _format-cookie cookie) "\r\n"))
	(print "Content-type: " _content-type "\r\n\r\n")
	(print (string content))
	(exit)
)

;; mark Private variables

(set '_response-codes
  '((200 "OK")
	(302 "Found")
    (404 "Not Found")
    (500 "Internal Error"))
)

(set '_content-type "text/html; charset=utf-8")
(set '_headers '())
(set '_cookies '())

(context MAIN)
