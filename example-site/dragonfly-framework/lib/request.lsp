; This file should only be loaded once!

;; @module request.lsp
;; @author Greg Slepak <greg at taoeffect.com>
;; <h3>$GET/$POST</h3>
;; These are very similar to the PHP $_GET and $_POST associative arrays.
;; Use them to retrieve the GET/POST values.
;; If a variable was specified, but no value assigned (i.e. '/?foo'), then
;; the value will be an empty string (you can use 'empty?' to check for this).
;; If the variable does not exist, 'nil' will be returned.
;; 
;; The values are url decoded:
;; <pre> "What+time+is+it%3f"  => "What time is it?"</pre>
;; <h3>$FILES</h3>
;; If a 'multipart/form-data' content type was submitted, then
;; STDIN is read for the values. Any files are placed in '$FILES', while
;; other key/value pairs are placed in '$POST'.
;; 
;; The values in '$FILES' contain an associative array with more information
;; about the files.
;; <h3>$BINARY</h3>
;; If the environment variable 'HTTP_CONTENT_TRANSFER_ENCODING' is set to
;; "binary" then STDIN is read (up to 'MAX_POST_LENGTH') and the data is
;; stored in '$BINARY'. Note that unlike the other symbols in this file,
;; '$BINARY' is a string, not a function.
;; <h3>$COOKIES</h3>
;; The environment variable 'HTTP_COOKIE' is parsed and the cookies are
;; in '$COOKIES' as key/value pairs just like '$GET' and '$POST'.

;; @syntax ($GET <str-key>)
;; @return the value of the GET parameter for <str-key>, nil if no such GET variable was passed in.
;; @syntax ($POST <str-key>)
;; @return the value of the POST parameter for <str-key>, nil if no such POST variable was passed in.
;; @syntax ($FILES <str-key>)
;; @syntax ($COOKIES <str-key>)
;; @syntax $BINARY

;===============================================================================
; !Global Variables
;===============================================================================

; For PHP's 'isset' use 'empty?' like this: (empty? ($POST "foo"))
(new Tree '$POST)
(new Tree '$GET)
(new Tree '$COOKIES)

; use like this: (lookup 'data ($FILES "filename"))
; Valid keys: 'data, 'name, 'length
(new Tree '$FILES)

; used to store binary STDIN data
(global '$BINARY)

; define MAX_POST_LENGTH if you want a custom value
(unless (and (number? MAX_POST_LENGTH) (global? MAX_POST_LENGTH))
	(constant (global 'MAX_POST_LENGTH) 1048576)
)

(context 'Request)

;===============================================================================
; !Private Functions
;===============================================================================

; (url-decode "What+time+is+it%3f")  => "What time is it?"
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))

(define (url-decode str)
   (replace "+" str " ")
   (replace REGEX_HEX_ENCODED_CHAR str (char (int (string "0x" $1))) 0x10000)
)

(constant 'REGEX_QUERY (regex-comp {&([^&=]+)=?([^&=]*?)(?=&|$)} 1))

(define (parse-query query)
	(when (starts-with query "?") (pop query))
	(push "&" query)
	(find-all REGEX_QUERY query (list $1 (url-decode $2)) 0x10000)
)

(define (regex-captcha regex-str str (options 0) (captcha 1))
	(if (regex regex-str str options)
		($ captcha)
	)
)

(define (handle-binary-data)
	(read-buffer (device) $BINARY MAX_POST_LENGTH)
)

(define (parse-multipart-chunk chunk , idx disp var val data)
	(when (set 'idx (find "Content-Disposition" chunk))
		(set 'chunk (idx (length chunk) chunk))
		(set 'disp (0 (find "\r\n" chunk) chunk))
		
		(when (and disp (set 'var (regex-captcha {name="(.+?)"} disp)))
			(set 'data ((+ 4 (find "\r\n\r\n" chunk)) (length chunk) chunk))
			(when (set 'idx (find "\r\n--" data))
				(set 'data (0 idx data))
				(if (set 'val (regex-captcha (string var {="(.+?)"}) disp))
					($FILES var (list (list 'name val) (list 'data data) (list 'length (length data))))
					($POST var data)
				)
			)
		)
	)
)

(define (handle-multipart-data , buff boundary)
	(set 'boundary (regex-captcha {boundary=(.+)} CONTENT_TYPE))	
	(while (read-buffer (device) buff MAX_POST_LENGTH boundary)
		(parse-multipart-chunk buff)
	)
)

;===============================================================================
; !$GET
;===============================================================================

(when QUERY_STRING
	(dolist (pair (parse-query QUERY_STRING))
		($GET (first pair) (last pair))
	)
)

;===============================================================================
; !$POST/$FILES/$BINARY
;===============================================================================

; windows doesn't have 'peek'
;(unless (zero? (peek (device)))
	(if (and (setf temp HTTP_CONTENT_TRANSFER_ENCODING) (= temp "binary"))
		(handle-binary-data)
		(and (setf temp CONTENT_TYPE) (starts-with temp "multipart/form-data"))
		(handle-multipart-data)
		(and (read-buffer (device) temp MAX_POST_LENGTH) temp)
		(dolist (pair (parse-query temp))
			($POST (first pair) (last pair))
		)
	)
;)

;===============================================================================
; !$COOKIES
;===============================================================================

; we do *NOT* want to use url-decode on the value
; that's something the user can do if they want to
(when HTTP_COOKIE
	(dolist (cookie (parse HTTP_COOKIE "; *" 0))
		(map set '(key value) (parse cookie "="))
		($COOKIES key value)
	)
)

(context 'MAIN)
