;;; Note: POST data can only be read once, after which it becomes unavailable
;;; to future parts of the program.  Should the default cgi.lsp module precede
;;; this module, POST data will be unavailable through the Request class.  If
;;; cgi.lsp is loaded after this module, POST data is unavailable to it.

;;; This module does not include output functions, including setting cookies.
;;; That will be part of the Response class.  The Response class may also
;;; include a session framework; any session data will be available through this
;;; class (once designed).

(context MAIN)

; globals must be delcared in MAIN context
(unless (number? *max-post-length*)
	(set '*max-post-length* 1002537))

(global '*max-post-length*)

(context 'Request)

;; mark Public API

(define (method)		_method)
(define (segments)		_segments)
(define (raw-query)		_rawQuery)
(define (post-length)   _postLength)
(define (binary?)       _binaryData)
(define (get?)			(= _method 'GET))
(define (post?)			(= _method 'POST))
(define (cookie? key)	(lookup key _cookies))

(define (get key)
	(if key
		(lookup key _get)
		_get
	)
)

(define (post key)
	(if key
		(lookup key _post)
		_post
	)
)

(define (cookies key)
	(if key
		(lookup key _cookies)
		_cookies
	)
)

(define (segment num)
	(if-not (>= _current-segment (- (length _segments) 1))
		(nth (if num num (inc _current-segment)) _segments)
		(begin (set '_current-segment -1) nil) ; reset current_segment and return nil
	)
)

;; mark Private API

;; (url-translate "What+time+is+it%3f")  => "What time is it?"
(define (url-translate str)
   (replace "+" str " ")
   (replace "%([0-9A-F][0-9A-F])" str (format "%c" (int (append "0x" $1))) 1)
)

(define (parse-query query-string , (params '()) pair)
	(dolist (element (parse query-string "&"))
		(set 'pair (parse element "="))
		(if (= 1 (length pair))
			(push nil pair -1)
			(setf (pair 1) (url-translate (last pair)))
		)
		(push pair params -1)
	)
	params
)

(define (regex-captcha regex-str str (options 0) (captcha 1))
	(if (regex regex-str str options)
		($ captcha)
	)
)

(define (parse-multipart-chunk chunk boundary-len, idx disp var val data (params '()))
	(set 'idx (find "Content-Disposition" chunk))
	
	(when idx
		(set 'chunk (idx (length chunk) chunk))
		(set 'disp (0 (find "\r\n" chunk) chunk))
		
		(when disp
			(set 'var (regex-captcha {name="(.*)"} disp 512))
		
			(when var
				(set 'data ((+ 4 (find "\r\n\r\n" chunk)) (length chunk) chunk))
				(set 'idx (find "\r\n--" data))
			
				(when idx
					(set 'data (0 idx data))
			
					(if (set 'val (regex-captcha (string var {="(.*)"}) disp 512))
						(begin
							(push (list var val) params -1)
							(push (list (append var "_data") data) params -1)
							(push (list (append var "_length") (length data)) params -1)
						)
						(push (list var data) params -1)
					)
				)
			)
		)
	)
	params
)

(define (parse-multipart-query , buff bytes-read boundary-len (params '()))
	(set 'boundary (regex-captcha {boundary=(.*)} contentType))
	(set 'boundary-len (length boundary))
	(set '_postLength 0)
	
	(while (set 'bytes-read (read-buffer (device) post-data *max-post-length* boundary))
		(inc _postLength bytes-read)
		(write-buffer _rawQuery post-data)
		(dolist (param (parse-multipart-chunk post-data boundary-len))
			(push param params -1)
		)
	)
	params
)

;; mark Go!

; (set '_cgi-keys '("REDIRECT_STATUS" "HTTP_HOST" "HTTP_USER_AGENT" "HTTP_ACCEPT"
; 	"HTTP_ACCEPT_LANGUAGE" "HTTP_ACCEPT_ENCODING" "HTTP_ACCEPT_CHARSET"
; 	"HTTP_KEEP_ALIVE" "HTTP_CONNECTION" "HTTP_COOKIE" "HTTP_CACHE_CONTROL" "PATH"
; 	"SERVER_SIGNATURE" "SERVER_SOFTWARE" "SERVER_NAME" "SERVER_ADDR" "SERVER_PORT"
; 	"REMOTE_ADDR" "DOCUMENT_ROOT" "SERVER_ADMIN" "SCRIPT_FILENAME" "REMOTE_PORT"
; 	"REDIRECT_URL" "GATEWAY_INTERFACE" "SERVER_PROTOCOL" "REQUEST_METHOD"
; 	"QUERY_STRING" "REQUEST_URI" "SCRIPT_NAME" "PATH_INFO" "PATH_TRANSLATED")
; )
;; this shit isn't necessary, just use (env) ====== NO! it could return nil! which is not a string!
;; set cleaned CGI environment parameters

; TODO: unset this, but make it simply create the variables instead (e.g. (set 'REQUEST_URI (env "REQUEST_URI")))
; (set '_cgi-env (map (fn (key) (list key (trim (string (env key))))) _cgi-keys))

(set '_cookies '() '_post '() '_rawQuery "")
(set '_method 'GET) ; set default method
(set '_segments (parse (trim (env "REQUEST_URI") "/") "/"))
(set '_current-segment -1)
(set 'path (env "REQUEST_URI"))
(set 'domain (env "HTTP_HOST"))
(set 'contentType (string (env "CONTENT_TYPE")))
(set '_binaryData (and contentType (not (or (starts-with contentType "text" 1) (find "form" contentType 1)))))

;; deal with GET params from QUERY_STRING
(set '_rawQuery (env "QUERY_STRING"))
(set '_get (parse-query _rawQuery))


;; deal with POST params from stdin data

(if (starts-with contentType "multipart/form" nil)
	(begin
		(set '_method 'POST)
		(set '_post (parse-multipart-query))
	)
	(begin
		(set '_postLength (read-buffer (device) post-data *max-post-length*))
		(when post-data
			(set '_method 'POST)
			(if-not _binaryData
				(set '_post (parse-query post-data))
			)
			(set '_rawQuery post-data)
		)
	)
)

;; deal with HTTP_COOKIE data
(dolist (element (parse (string (env "HTTP_COOKIE")) ";"))
	(push (parse element "=") _cookies -1)
)

(context MAIN)
