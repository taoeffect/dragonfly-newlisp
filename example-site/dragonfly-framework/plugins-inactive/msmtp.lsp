;; @module msmtp.lsp 
;; @description SMTP-compatible API for using msmtp to send email
;; @version 1.0 - Everything in "smtpx.lsp" plus inheriting msmtp's powers (especially: SMTPS!)
;; @author Greg Slepak 2023, Lutz Mueller 2001-2009, Cormullion 2008

(context 'MSMTP) 

;; @syntax (MSMTP:send-mail <str-from> <str-to> <str-subject> <str-message> [<str-server> [<str-usr> <str-pass> [<int-port>]]])
;; @param <str-from> The email address of the sender.
;; @param <str-to> The email address of the recipient.
;; @param <str-subject> The subject line of the email.
;; @param <str-message> The message part of the email.
;; @param <str-server> The address of the SMTP server (default: "localhost")
;; @param <str-user> Optional user name for authentication.
;; @param <str-pass> Optional password for authentication.
;; @param <int-port> Optional port to communicate on (default: 25)
;; @return On success 'true', on failure 'nil'.
;; In case the function fails returning 'nil', the function
;; 'MSMTP:get-error-text' can be used to receive the error text.
;;
;; @example 
;; (MSMTP:send-mail "jdoe@asite.com" "somebody@isp.com" "Greetings" 
;;   "How are you today? - john doe -" "smtp.asite.com" "jdoe" "secret") 
;; 
;; This logs in to the server, tries to authenticate using the username 'jdoe' and password 'secret' (if supplied), 
;; and sends an email with the format: 
;;
;;  From:    jdoe@asite.com 
;;  To:      somebody@isp.com 
;;  Subject: Greetings 
;;  Message: How are you today? - John Doe - 
(define (send-mail mail-from mail-to mail-subject mail-body (SMTP-server "localhost") user-name password (port 465)) 
    (set 'send-buffer "" 'fail-reason "")
    (and 
        (mail-send-header)
        (mail-send-body)
        (run-msmtp)))

(define (build-message str) (extend send-buffer (string str "\r\n")))

; NOTE: this is the only possible way to handle process input, output, and error messages in newlisp
;       for whatever reason, the 'process' function cannot be used because neither 'peek' nor 'wait-pid'
;       work to wait for the process to finish and collect the output. So this method using tmp files is
;       used instead, and actually ends up being shorter/simpler than the process method.
(define (run-msmtp , filename err-info r)
  (setf filename (string "/tmp/email-" (uuid)))
  (setf err-info (string filename ".err"))
  (when (write-file filename send-buffer)
    (setf r (! (format {cat %s | msmtp --auth=plain --tls=on --tls-starttls=off --host %s --port %d --user %s --read-envelope-from -t --passwordeval "echo \"%s\"" 2>%s} filename SMTP-server port user-name password err-info)))
    (unless (= 0 r)
      (setf fail-reason (read-file err-info)))
    (delete-file filename)
    (delete-file err-info)
    (= 0 r)))

; ---------------------------------------------------------------
; !Attachments - Public API
; ---------------------------------------------------------------

;; @syntax (MSMTP:get-error-text)
;; <p>Call this to get the reason 'send-mail' returned 'nil'.</p>
(define (get-error-text) 
    fail-reason) 

;; @syntax (MSMTP:clear-attachments)
(define (clear-attachments)
  (setf attachments '())
)

;; @syntax (MSMTP:attach-document <str-content> <str-filename> [<str-disposition> [<str-mime-type> [<str-encoding>]]])
;; @param <str-content> The attachment data.
;; @param <str-filename> How you'd like your attachment to appear named in the email.
;; @param <str-disposition> "attachment" or "inline". default is "attachment".
;; @param <str-mime-type> default is "application/octet-stream".
;; @param <str-encoding> default is "base64". If 'encoding' is "base64" it will be automatically transformed using 'encode64-widthsafe'
(define (attach-document content filename (disposition "attachment") (mime-type "application/octet-stream") (encoding "base64"))
  (push (list content filename disposition mime-type encoding) attachments -1)
)

; ---------------------------------------------------------------
; !UTF-8 encoding support for non-ASCII characters
; ---------------------------------------------------------------

;; @syntax (MSMTP:encode64-widthsafe <buff-data>)
;; <p>Useful for attaching binary data such as images. Converts the data into base64
;; and chops it up so that the lines are not longer than 76 characters long, making
;; it safe to include in the body of emails.</p>
;; <p>If the attachment's encoding to "base64" (which it is by default), this function
;; will automatically applied to the <str-content> of the email.</p>
(define (encode64-widthsafe data)
  (join (explode (base64-enc data) 76) "\n")
)

;; @syntax (MSMTP:encode64-line <str-line>)
;; <p>Creates a base64 UTF-8 compatible string, suitable for including foreign characters
;; in the subjects of emails. This is used by 'send-mail' automatically on the filename
;; of any attachments, as well as the subject of the email.</p>
(define (encode64-line str)
  (string "=?UTF-8?B?" (base64-enc str) "?=")
)

; ---------------------------------------------------------------
; !Attachments - Private API
; ---------------------------------------------------------------

(setf boundary (string "newLISP-" (nth -2 (sys-info)) "--65z64F4n654"))

(setf mail-body-wrapper (string
{--} boundary {
Content-Type: text/plain; charset=utf-8
Content-Transfer-Encoding: base64

%s
  
--} boundary {%s}))

; filename madness. We actually do not need the *=utf-8 weirdness
; if we're using the encode64-line func instead of utf8-urlencode

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
    (if (zero? (length attachments)) "--" "")))
  
(define (mail-send-header) 
  (build-message (string "FROM: " mail-from)) 
  (build-message (string "TO: " mail-to)) 
  (build-message (string "SUBJECT: " (encode64-line mail-subject)))
  (build-message (string "MIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=" boundary))
  (build-message (string "X-Mailer: newLISP v." (nth -2 (sys-info)) "\r\n")))

(define (mail-send-body)
  (build-message "")
  (build-message (prepared-body))
  (send-attachments)
  (build-message "."))


(define (send-attachments , encoding filename)
  (dolist (attachment attachments)
    (set 'encoding (attachment 4) 'filename (attachment 1))
    (build-message (format attachment-wrapper
                      (attachment 2)
                      (encode64-line filename)
                      (attachment 3)
                      (encode64-line filename)
                      encoding
                      (if (= encoding "base64")
                        (encode64-widthsafe (attachment 0))
                        (attachment 0))
                      ; indicate this is the last boundary if no more
                      (if (= (+ 1 $idx) (length attachments)) "--" "")))))

(context MAIN) 


; 'process' function needs full path according to docs
; (constant 'msmtp-exe (first (exec "which msmtp"))) ; will throw if it doesn't exist
; (define (run-msmtp , pid wpid)
;   (map set '(myin msout) (pipe))
;   (map set '(msin myout) (pipe))
;   (map set '(errin errout) (pipe))
;   (setf pid (process
;               (format {%s --auth=plain --tls=on --tls-starttls=off --host %s --port %d --user %s --read-envelope-from -t --passwordeval "echo %s"} msmtp-exe SMTP-server port user-name password)
;               msin msout errout))
;   (setf wpid (wait-pid pid nil))
;   (println "pid: " pid " wpid: " wpid "| vs: " (wait-pid -1 nil))
;   (write myout send-buffer)
;   (close myout)
;   ; (close msout)

;   ; (do-while (and (= (peek myin) 0) (= (peek errin) 0))
;   ;   (println "wait-pid: " (wait-pid pid nil) "| vs: " (wait-pid -1 nil))
;   ;   (sleep 1000))
;   ; (println "wait-pid: " (wait-pid pid))

;   (if (> (peek errin) 0)
;     (begin
;       (while (> (peek errin) 0)
;         (extend fail-reason (string (read-line errin) "\n")))
;       (DF:log-warn "failed to send email to " mail-to ":\n" fail-reason)
;       nil) ; return nil to indicate failure
;     (if (= (peek myin) 0)
;       true
;       (begin
;         (DF:log-info "msmtp success. returned additional output: " (read-line myin))
;         (close msout)
;         true))))
