#!/usr/bin/newlisp
;; @author Jeff Ober <jeffober@gmail.com>

(context 'Session)

(define SESSION_DIR "/tmp")
(define SESSION_MAX_AGE (* 60 60 24 7)) ; seconds
(define SESSION_KEY "NLWSESID")
(define SESSION_PREFIX "NLWSES")
(define SESSION_STARTED)
(define SESSION_ID) ; stores the current session id

;===============================================================================
; !Session control
; notes:
;  * sessions require cookies to function
;  * close-session or MAIN:exit must be called to save session changes to disk
;===============================================================================

;; @syntax (Session:define-session-handlers <fn-open> <fn-close> <fn-delete> <fn-clear> <fn-clean>)
;; @param <fn-open> function to begin a new session
;; @param <fn-close> function to close a session, saving changes
;; @param <fn-delete> function to delete a session
;; @param <fn-clean> function to prune old sessions
;; <p>Defines handler functions to be called when various session control
;; functions are used, making custom session storage a fairly simple matter.</p>
;; The required handler functions are:
;; <ul>
;; <li>'fn-open': called by 'open-session'; resumes or starts a new session storage instance, initializing the context tree</li>
;; <li>'fn-close': called by 'close-session'; writes changes to a session to storage</li>
;; <li>'fn-delete': called by 'delete-session'; deletes the entire session from storage</li>
;; <li>'fn-clean': called by 'clean-sessions'; prunes old stored sessions</li>
;; </ul>
;; Some useful functions and variables for handler functions:
;; <ul>
;; <li>'session-id': function that returns the current session id and sets the session cookie when necessary</li>
;; <li>'session-context': function that returns the session context dictionary</li>
;; <li>'SESSION_MAX_AGE': a variable storing the number of seconds after which an orphan session should be deleted</li>
;; </ul>
(define (define-session-handlers fn-open fn-close fn-delete fn-clean)
  (setf _open-session fn-open
        _close-session fn-close
        _delete-session fn-delete
        _clean-sessions fn-clean))

;; @syntax (Session:session-id [<str-sid>])
;; @param <str-sid> (optional) the session ID to use
;; @return a unique session id for the client
;; <p>Creates or retrieves the client's session id. If this is a new session id,
;; a cookie is set in the client's browser to identify it on future loads.</p>
;; <p>If <str-sid> is provided, it will be used as the new session ID.</p>
(define (session-id sid)
  (setf SESSION_ID
    (or (when sid
          ($COOKIES SESSION_KEY sid)
          sid)
        SESSION_ID
        ($COOKIES SESSION_KEY)
        (begin
          (setf sid (string SESSION_PREFIX "-" (uuid)))
          ($COOKIES SESSION_KEY sid)
          sid))))

;; @syntax (Session:session-context)
;; @return a symbol pointing to the current session's context dictionary
;; <p>Run-time session data is stored in a context tree. 'session-context'
;; returns the current session tree or creates a new one when necessary.
;; This function is primarily intended for session handlers' use; it is
;; typically more useful to call 'session' on its own to retrieve an association
;; list of key/value pairs in an application.</p>
(define (session-context , ctx)
  (setf ctx (sym (session-id) 'MAIN))
  (unless (context? ctx)
    (context ctx))
  ctx)

;; @syntax (Session:open-session)
;; <p>Initializes the client's session.</p>
(define (open-session)
  (_open-session)
  (setf SESSION_STARTED true)
  (session-id))

;; @syntax (close-session)
;; <p>Writes any changes to the session to file. This is automatically called
;; when the distribution function 'exit' is called.</p>
(define (close-session)
  (when SESSION_STARTED
    (_close-session)))

;; @syntax (delete-session)
;; <p>Deletes the session. Sessions are then turned off and 'open-session'
;; must be called again to use sessions further.</p>
(define (delete-session)
  (unless SESSION_STARTED (throw-error "session is not started"))
  (_delete-session)
  (delete (session-context))
  ($COOKIES SESSION_KEY "" 0)
  (setf SESSION_STARTED nil))

;; @syntax (clear-session)
;; <p>Clears all session variables.</p>
(define (clear-session)
  (when SESSION_STARTED
    (dotree (s (session-context))
      (delete (sym s (session-context))))))

;; @syntax (clean-sessions)
;; <p>Cleans old session files. This function is not currently called automatically;
;; note that there is the possibility of a race condition with this function and other
;; session handling functions.</p>
(define (clean-sessions)
  (_clean-sessions))

;; @syntax (session [<str-key> [<str-value>]])
;; @param <str-key> the session key
;; @param <str-value> the new value
;; When called with both <str-key> and <str-value>, sets the session variable. When
;; called with only <str-key>, returns the value of <str-key>. Otherwise, returns
;; an association list of session variables. Returns nil if the session is not
;; opened.
(define (session key value)
  (cond
    ((not SESSION_STARTED) nil)
    ((and key value) (context (session-context) key value))
    ((true? key) (context (session-context) key))
    (true (let ((alist '()))
            (dotree (s (session-context))
              (push (list (term s) (context (session-context) (term s))) alist -1))
            alist))))

;===============================================================================
; !Default session handlers
; 
; The default session handlers use newLISP's 'save' and 'load' functions to
; easily serialize and import context data to and from file records. The files
; are stored unencrypted, so a custom handler should be used on a shared
; system.
;===============================================================================

; Returns the name of the file in which the session data is stored.
(define (default-session-file)
  (string SESSION_DIR "/" (session-id) ".lsp"))

; Loads/creates the session file; creates a new context tree when
; necessary.
(define (default-open-session)
  (if (file? (default-session-file))
    (load (default-session-file))
    (save (default-session-file) (session-context))))

; Saves the session context to the session file.
(define (default-close-session)
  (save (default-session-file) (session-context)))

; Deletes the session file.
(define (default-delete-session)
  (when (file? (default-session-file))
    (delete-file (default-session-file))))

; Deletes old session files.
(define (default-clean-sessions , f)
  (dolist (tmp-file (directory SESSION_DIR))
    (when (starts-with tmp-file SESSION_PREFIX)
      (setf f (string SESSION_DIR "/" tmp-file))
      (when (> (- (date-value) (file-info f 5 nil)) SESSION_MAX_AGE)
        (delete-file f)))))

; Install default session handlers
(define-session-handlers
  default-open-session
  default-close-session
  default-delete-session
  default-clean-sessions)

(context 'MAIN)

; This function wraps the distribution exit routine to ensure that sessions are
; written when the application exits. It is only called when the 'exit' function
; is explicitly called. The 'exit' function is renamed 'sys-exit'. The 'Web'
; function 'close-session' is only called on a normal exit (exit code 0.)
(define (exit-with-session-close (n 0))
  (when (zero? n)
    (Session:close-session))
  (MAIN:sys-exit))

(constant 'sys-exit exit)
(constant 'exit exit-with-session-close)
