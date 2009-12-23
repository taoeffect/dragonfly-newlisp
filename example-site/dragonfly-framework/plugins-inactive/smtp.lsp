(context 'SMTP) 

(define (send-mail mail-from mail-to mail-subject mail-body (SMTP-server "localhost") user-name password (port 25)) 
    (and 
        (set 'from-hostname (nth 1 (parse mail-from "@"))) 
        (set 'socket (net-connect SMTP-server port)) 
        (confirm-request "2") 
        (net-send-get-result (string "HELO " from-hostname) "2") 
        (if (or (null? user-name) (null? password))  ; NOTE: changed unless -> if-not
           true (mail-authorize user-name password)) 
        (net-send-get-result (string "MAIL FROM: <" mail-from ">") "2") 
        (net-send-get-result (string "RCPT TO: <" mail-to ">") "2") 
        (net-send-get-result "DATA" "3") 
        (mail-send-header)
        (mail-send-body) 
        (confirm-request "2") 
        (net-send-get-result "QUIT" "2") 
        (or (net-close socket) true))) 

(define (confirm-request conf)
    (net-receive socket recvbuff 256 "\r\n")
    ; Empty out pipe. According to SMTP spec, last line has valid code.
    ; added for 1.8 for newLISP 9.2.0
    (while (< 0 (net-peek socket))
        (net-receive socket recvbuff 256 "\r\n")
    )
    (starts-with recvbuff conf))


(define (net-send-get-result str conf) 
   (set 'send-str (string str "\r\n")) 
   (net-send socket send-str) 
   (if conf (confirm-request conf) true))

; DANGER! We *must* use 'append' here instead of 'string' as the two treat "\000" differently!
(define (mail-authorize user-name password) 
   (net-send-get-result 
       (string "AUTH PLAIN " 
               (base64-enc (append "\000" user-name "\000" password))) "235"))

;; old functions, we have our own.
; (define (mail-send-header) 
;     (net-send-get-result (string "TO: " mail-to)) 
;     (net-send-get-result (string "FROM: " mail-from)) 
;     (net-send-get-result (string "SUBJECT: " mail-subject)) 
; 	;(net-send-get-result headers)
;     (net-send-get-result (string "X-Mailer: newLISP v." (nth -2 (sys-info))))) 
; 
; (define (mail-send-body ) 
;     (net-send-get-result "") 
;     (dolist (lne (parse mail-body "\r\n")) 
;         (if (starts-with lne ".") 
;             (net-sent-get-result (string "." lne)) 
;             (net-send-get-result lne))) 
;     (net-send-get-result ".")) 

(define (get-error-text) 
    recvbuff) 

; ---------------------------------------------------------------
; !Attachments - Public API
; ---------------------------------------------------------------

(define (clear-attachments)
	(setf attachments '())
)

;; @param disposition one of "attachment" or "inline". default is "attachment".
;; @param mime-type default is "application/octet-stream".
;; @param encoding default is "base64". If 'encoding' is "base64" it will be automatically transformed using 'encode64-widthsafe'
(define (attach-document content filename (disposition "attachment") (mime-type "application/octet-stream") (encoding "base64"))
	(push (list content filename disposition mime-type encoding) attachments -1)
)

; ---------------------------------------------------------------
; !UTF-8 encoding support for non-ASCII characters
; ---------------------------------------------------------------

;; useful for attaching binary data such as images.
;; if you use this make sure to set the attachment's encoding to "base64"
(define (encode64-widthsafe data)
	(join (explode (base64-enc data) 76) "\n")
)

(define (encode64-line str)
	(string "=?UTF-8?B?" (base64-enc str) "?=")
)

; ---------------------------------------------------------------
; !Attachments - Private API
; ---------------------------------------------------------------

(setf boundary (string "newLISP-" (nth -2 (sys-info)) "--65z64F4n654"))
(setf headers (string "MIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=" boundary))

(setf mail-body-wrapper (string
{--} boundary {
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: base64

%s
	
--} boundary {%s}))

;; filename madness. We actually do not need the *=utf-8 weirdness
;; if we're using the encode64-line func instead of utf8-urlencode

(setf attachment-wrapper (string
;{Content-Disposition: %s; filename*=utf-8''%s
{Content-Disposition: %s; filename="%s"
Content-Type: %s; name="%s"
Content-Transfer-Encoding: %s

%s

--} boundary {%s}))

(setf attachments '()) ; the list of attachments is placed here

(define (prepared-body)
	(format mail-body-wrapper (encode64-widthsafe mail-body)
		; indicate this is the last boundary if no attachments
		(if (zero? (length attachments)) "--" "")
	)
)

(define (mail-send-header) 
    (net-send-get-result (string "TO: " mail-to)) 
    (net-send-get-result (string "FROM: " mail-from)) 
    (net-send-get-result (string "SUBJECT: " (encode64-line mail-subject)))
	(net-send-get-result headers)
    (net-send-get-result (string "X-Mailer: newLISP v." (nth -2 (sys-info)) "\r\n")))

(define (mail-send-body)
	(net-send-get-result "")
	(net-send-get-result (prepared-body))
	(send-attachments)
	(net-send-get-result ".")
)

(define (send-attachments , encoding filename)
	(dolist (attachment attachments)
		(set 'encoding (attachment 4) 'filename (attachment 1))
		(net-send-get-result (format attachment-wrapper
			(attachment 2)
			(encode64-line filename)
			(attachment 3)
			(encode64-line filename)
			encoding
			(if (= encoding "base64")
				(encode64-widthsafe (attachment 0))
				(attachment 0)
			)
			; indicate this is the last boundary if no more
			(if (= (+ 1 $idx) (length attachments)) "--" "")
		))
	)
)

(context MAIN) 

