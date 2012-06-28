; This file should only be loaded once!

;; @module request.lsp
;; @author Greg Slepak <greg at taoeffect.com>
;; <h3>$GET/$POST</h3>
;; These are very similar to the PHP $_GET and $_POST associative arrays.
;; Use them to retrieve the GET/POST values.
;; If a variable was specified, but no value assigned (i.e. '/?foo'), then
;; the value will be an empty string (you can use 'empty?' to check for this).
;; If the variable does not exist, 'nil' will be returned.
;; <br><br>The values are url decoded:
;; <pre> "What+time+is+it%3f"  => "What time is it?"</pre>
;; <h3>$FILES</h3>
;; If a 'multipart/form-data' content type was submitted, then
;; STDIN is read for the values. Any files are placed in '$FILES', while
;; other key/value pairs are placed in '$POST'.
;; <br><br>The values in '$FILES' contain an associative array with more information
;; about the files.
;; <h3>$BINARY</h3>
;; If the environment variable 'HTTP_CONTENT_TRANSFER_ENCODING' is set to
;; "binary" then STDIN is read (up to 'MAX_POST_LENGTH') and the data is
;; stored in '$BINARY'. Note that unlike the other symbols in this file,
;; '$BINARY' is a string, not a function.
;; <h3>$COOKIES</h3>
;; The environment variable 'HTTP_COOKIE' is parsed and the cookies are placed in
;; in '$COOKIES' as key/value pairs just like '$GET' and '$POST'.
;; @syntax ($GET <str-key>)
;; @return the value of the GET parameter for <str-key>, nil if no such GET variable was passed in.
;; @syntax ($POST <str-key>)
;; @return the value of the POST parameter for <str-key>, nil if no such POST variable was passed in.
;; @syntax ($FILES <str-key>)
;; <p>Returns an association list with keys ''name', ''data', and ''length' for the result
;; of a 'multipart/form-data' posted form.</p>
;; @syntax ($COOKIES <str-key>)
;; @return the value of the cookie with the key <str-key>, or nil if no such cookie exists.
;; @syntax $BINARY
;; <p>This is a global buffer that contains what was sent to STDIN (up to 'MAX_POST_LENGTH')
;; when the value of environment variable 'HTTP_CONTENT_TRANSFER_ENCODING' is set to "binary".</p>

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
(set (global '$BINARY) "")

;; @syntax MAX_POST_LENGTH
;; <p>A constant global integer whose value by default is '1048576'. This represents
;; the maximum amount of binary data (in bytes) that can be sent in the body of the
;; request (which gets stored in '$BINARY').</p>
;; <p>Minimum value is '8192'. Set this value prior to the loading of this file
;; (for example in 'config.lsp') to customize it.</p>
(global 'MAX_POST_LENGTH)
(unless (> MAX_POST_LENGTH 8192)
	(constant (global 'MAX_POST_LENGTH) 1048576)
)

(context 'Request)

;===============================================================================
; !UTF8 Compatible URL encoding/decoding
;===============================================================================

(constant 'REGEX_HTTP_SPECIAL_STR (regex-comp {([^.0-9a-z]+)} 1))
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))

(define (hex-encode-str str , cnvrt)
	(setf cnvrt	(dup "%%%X" (length str)))
	(format cnvrt (unpack (dup "b" (length str)) str))
)

;; @syntax (utf8-urlencode <str> [<bool-everything>])
;; @param str the string to encode
;; @param bool-everything whether to escape the entire string or just most of the "non-ascii friendly" parts.
;; <p>Use this function to safely encode data that might have foreign characters in it, or simply
;; characters that should be placed into URLs:</p>
;; <b>example:</b>
;; <pre> (utf8-urlencode "What time is it?")  => "What%20time%20is%20it%3F"</pre>
(define (utf8-urlencode str everything)
	(if everything
		(hex-encode-str str)
		(replace REGEX_HTTP_SPECIAL_STR str (hex-encode-str $1) 0x10000)
	)
)

;; @syntax (utf8-urldecode <str>)
;; <p>Decodes a utf8-urlencoded string. Converts '+'&apos;s to spaces.</p>
(define (utf8-urldecode str)
	(replace "+" str " ")
	(replace REGEX_HEX_ENCODED_CHAR str (pack "b" (int $1 nil 16)) 0x10000)
)

;===============================================================================
; !Private Functions
;===============================================================================

(constant 'REGEX_QUERY (regex-comp {&([^&=]+)=?([^&=]*?)(?=&|$)} 1))

(define (parse-query query)
	(when (starts-with query "?") (pop query))
	(push "&" query)
	(find-all REGEX_QUERY query (list (utf8-urldecode $1) (utf8-urldecode $2)) 0x10000)
)

(define (regex-captcha regex-str str (options 0) (captcha 1))
	(if (regex regex-str str options)
		($ captcha)
	)
)

; we can't simply do: (read (device) $BINARY MAX_POST_LENGTH)
; because versions of newlisp (upto and including 10.1.9) have a fairly
; broken 'read-buffer' function that can't handle large amounts of data.
; this may be fixed in a future version of newLISP, but for now we're doing
; it the C-way.
(define (handle-binary-data , (chunk "") (chunk-size 8192) (max-bytes MAX_POST_LENGTH) bytes)
	(while (and (setf bytes (read (device) chunk chunk-size)) chunk (not (zero? max-bytes)))
		(extend $BINARY chunk)
		(-- max-bytes bytes)
		(when (< max-bytes chunk-size) (setf chunk-size max-bytes))
	)
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
	(while (read (device) buff MAX_POST_LENGTH boundary)
		(parse-multipart-chunk buff)
	)
)

(define (add-keyvalue-to-ctx key value ctx)
	; support PHP-like multi-params
	(if (ends-with key "[]")
		(if (list? (ctx key))
			(push value (ctx key) -1)
			(ctx key (list value))
		)
		(ctx key value)
	)
)

;===============================================================================
; !$GET
;===============================================================================

(unless (empty? QUERY_STRING)
	(dolist (pair (parse-query QUERY_STRING))
		(add-keyvalue-to-ctx (first pair) (last pair) $GET)
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
		(and (read (device) temp MAX_POST_LENGTH) temp)
		(dolist (pair (parse-query temp))
			(add-keyvalue-to-ctx (first pair) (last pair) $POST)
		)
	)
;)

;===============================================================================
; !$COOKIES
;===============================================================================

; we do *NOT* want to use utf8-urldecode on the value
; that's something the user can do if they want to
(when HTTP_COOKIE
	(dolist (cookie (parse HTTP_COOKIE "; *" 0))
		(map set '(key value) (parse cookie "="))
		($COOKIES key value)
	)
)

(context 'MAIN)
