;; @module database_sqlite3
;; @description SQLite3 subclass of DF.DB. Only lists Sqlite3 specific functions.
;; @version 1.2
;; @author Greg Slepak 
;; @location http://www.taoeffect.com/newlisp/database_sqlite3.lsp.txt
;; <h3>Features not found in newLISP's sqlite3.lsp:</h3>
;; <ul>
;; <li>Multiple connections</li>
;; <li>Multiple active SQL statements</li>
;; <li>Supports reuse of SQL statements through parameter rebinding</li>
;; <li>Supports BLOB data-type as per 'DF.DB' and 'DF.BLOB' specification</li>
;; <li>Supports 'true' by converting it to value 1 (as integer)</li>
;; <li>Conforms to generic 'DF.DB' interface</li>
;; <li>Grabs integers directly through 64-bit function</li>
;; <li>Can go through results row-by-row</li>
;; <li>Allows specification of custom sqlite3 library page with 'SQLITE3_LIBRARY_PATH'</li>
;; <li>Uses the latest Sqlite3 library functions if they are available</li>
;; <li>Better error handling</li>
;; </ul>
;; <h3>Requirements</h3>
;; See module @link http://www.taoeffect.com/newlisp/database.lsp.html DF.DB for requirements.
;; <h3>Version history</h3>
;; <b>1.2.0</b> &bull; temporary fix for handling of floats, sqlite3 functions globally for speed, binding 'true' is handled as 1
;; <b>1.1.2</b> &bull; fixed a bug in 'get-string-cast' and implemented 'DF.SQL:col-name'
;; <b>1.1.1</b> &bull; improved readability in error logging, fixed binding of integers on 32-bit newlisp builds<br/>
;; <b>1.1.0</b> &bull; support for 'DF.BLOB'<br/>
;; <b>1.0.0</b> &bull; initial release

(DF:activate-plugin "db/database")

(new-class 'Sqlite3 DF.DB)
(new-class 'Sqlite3.SQL DF.SQL)

(context Sqlite3)

;---------------------------------------------------------------
; !Private imports
;---------------------------------------------------------------

(set 'lib-paths (list
	"/usr/lib/libsqlite3.so" ; SuSE Linux
	"/usr/local/lib/libsqlite3.so" ; Linux and BSDs
	"/usr/pkg/lib/libsqlite3.so" ; NetBSD
	"/usr/lib/libsqlite3.0.dylib" ; Mac OSX Darwin
	"/usr/local/lib/libsqlite3.so" ; Solaris
	"/usr/local/lib/libsqlite3.so.11.0" ; OpenBSD
	(string (env "PROGRAMFILES") "/sqlite3/sqlite3.dll") ; Win32/MinGW
	(string MAIN:SQLITE3_LIBRARY_PATH) ; allow user to define their own
))

(set 'library (lib-paths (or 
				(find true (map file? lib-paths))
				(throw-error "sqlite3 library not found. Set SQLITE3_LIBRARY_PATH."))))

(set 'lib-funcs '(
	; functions for dealing with sqlite3 databases
	"sqlite3_open"           "sqlite3_last_insert_rowid" "sqlite3_changes"
	"sqlite3_close"          "sqlite3_busy_timeout"      "sqlite3_libversion_number"
	"sqlite3_prepare"        "sqlite3_errmsg"
	; functions for dealing with sqlite3 statements
	"sqlite3_column_count"   "sqlite3_finalize"          "sqlite3_bind_parameter_index"
	"sqlite3_column_name"    "sqlite3_reset"             "sqlite3_transfer_bindings"  
	"sqlite3_column_type"    "sqlite3_errmsg"            "sqlite3_step"
	"sqlite3_bind_int64"     "sqlite3_column_int64"      "sqlite3_bind_int"
	"sqlite3_bind_double"    "sqlite3_column_double"     "sqlite3_column_name"
	"sqlite3_bind_null"      "sqlite3_column_text"       "sqlite3_column_count"
	"sqlite3_bind_text"      "sqlite3_column_blob"
	"sqlite3_bind_blob"      "sqlite3_column_bytes"   
))

; Switch to MAIN context and import them as global functions so that we're not copying
; these symbols each time we create a new sql object. This is OK because they're qualified names.
(context MAIN)

(dolist (func Sqlite3:lib-funcs)
	(global (sym func))
	(import Sqlite3:library func "cdecl")
)

; import possibly missing functions
(catch (begin (import library "sqlite3_open_v2" "cdecl") (global 'sqlite3_open_v2)) 'Sqlite3:open_v2)
(catch (begin (import library "sqlite3_prepare_v2" "cdecl") (global 'sqlite3_prepare_v2)) 'Sqlite3:prepare_v2)

(context Sqlite3)

; if open_v2 = sqlite3_open_v2 then sqlite3_open_v2 is available
(set 'open_v2 (= open_v2 sqlite3_open_v2)
     'prepare_v2 (= prepare_v2 sqlite3_prepare_v2)
)

;---------------------------------------------------------------
; !Private constants and variables
;---------------------------------------------------------------

; useful error codes
(set 'error-codes
  '(SQLITE_OK SQLITE_ERROR SQLITE_BUSY SQLITE_SCHEMA
    SQLITE_ROW SQLITE_DONE)
)
(map constant error-codes '(0 1 5 17 100 101))
; convenience for 'failable'
(constant 'good-errors (list SQLITE_OK SQLITE_ROW SQLITE_DONE))
; used for getting addresses
(constant 'ptr-template "\000\000\000\000\000\000\000\000")
; for use with 'last-error'
(set 'error-code SQLITE_OK 'error-msg "No problems.")


; cache the prepare function we'll be using
(if prepare_v2
	(set 'prepare_sql_func sqlite3_prepare_v2)
	(set 'prepare_sql_func sqlite3_prepare)
)

;---------------------------------------------------------------
; !Sqlite3 - Private API
;---------------------------------------------------------------

(define-macro (failable action)
	(unless (find (set 'error-code (eval action)) good-errors)
		(set 'error-msg (sqlite3_errmsg db))
		(if (zero? error-msg)
			(set 'error-msg nil)
			(set 'error-msg (get-string error-msg))
		)
		(setf action (string action))
		(replace (string @self ":") action "") ; make it more readable
		(DF:log-err "[" @self "] " action " => " (last-error))
		nil ; indicate failure
	)
)

(define (assert-connected)
	(unless db (throw-error "Database connection not open!"))
)

;---------------------------------------------------------------
; !Public Sqlite3 API
;---------------------------------------------------------------

;; @syntax Sqlite3
;; <p>Represents a connection to an SQLite3 database. Create one like so:</p>
;; <pre>(instantiate Sqlite3 [<str-filepath> [<flags> [<vfs-module>]]])</pre>
;; <p>If <str-filepath> is specified a connected instance will be returned or 'nil'
;; upon failure to connect. If they are not specified then an unconnected instance
;; will be returned.</p>
;; <p><b>see:</b> documentation for 'Sqlite3:open' for an explanation of the parameters.</p>
(define (Sqlite3:Sqlite3)
	(unless (zero? (length $args))
		; we can't simply call 'open' because it's a builtin primitive
		; we also could do @self:open, but that's not necessary
		; and could be slower. When an instance of this is created it
		; will be rewritten to point to the new context.
		(apply Sqlite3:open $args)
	)
)

(define (Sqlite3:dealloc)
	(Sqlite3:close)
)

;; @syntax (Sqlite3:open <str-filepath> [<flags> [<vfs-module>]])
;; <p><str-filepath> specifies the path to the sqlite3 database, but it can also be
;; ":memory:" to indicate an in-memory database.</p>
;; <p><flags> and <vfs-module> are optional parameters as defined in the sqlite
;; reference @link http://www.sqlite.org/c3ref/open.html documentation for
;; 'sqlite3_open_v2'. Your installation of sqlite3 may need to be updated for this to be available.</p>
;; <p>For return values see 'DF.DB:open'.</p>
(define (Sqlite3:open filepath flags vfs-module , cmd (dbp ptr-template))
	(if db (throw-error "Already connected!"))
	(if flags
		(if open_v2
			(set 'cmd '(failable (sqlite3_open_v2 filepath dbp flags vfs-module)))
			(throw-error "sqlite3_open_v2 not avaliable! Update your sqlite3 installation!")
		)
		(set 'cmd '(failable (sqlite3_open filepath dbp)))
	)
	
	(when (eval cmd)
		(set 'db (get-ptr dbp))
		(Sqlite3:set-timeout 30000) ; 30 seconds
		true ; return true regardless of whether we succeeded in setting the timeout
	)
)

(define (Sqlite3:close)
	(if (or (not db) (failable (sqlite3_close db)))
		(begin (set 'db nil) true)
		(when (= error-code SQLITE_BUSY)
			; leaked statements are programmer error, therefore we throw an error
			(throw-error "cannot close connection due to leaked prepared statement(s)!")
		)
	)
)

(define (Sqlite3:connected?)
	(!= db nil)
)

(define (Sqlite3:prepare-sql sql , (stmtp ptr-template) (tailp ptr-template))
	(assert-connected)
	(when (failable (prepare_sql_func db sql -1 stmtp tailp))
		(instantiate Sqlite3.SQL @self (get-ptr stmtp) sql)
	)
)

(define (Sqlite3:execute-update sql params , result)
	(assert-connected)
	(when (setf sql (prepare-sql sql))
		(setf result (or (not params) (sql:bind-params params)))
		(when result (setf result (sql:next-row)))
		(deallocate sql)
		result
	)
)

(define (Sqlite3:execute-query sql params)
	(assert-connected)
	(when (setf sql (prepare-sql sql))
		(if (or (not params) (sql:bind-params params))
			sql
			(begin (deallocate sql) nil)
		)
	)
)

(define (Sqlite3:rows-for-query sql params , rows row)
	(assert-connected)
	(when (setf sql (prepare-sql sql))
		(when (and (or (not params) (sql:bind-params params))
		         (setf row (sql:next-row)))
			(setf rows '())
			(while (list? row)
				(push row rows -1)
				(setf row (sql:next-row))
			)
		)
		(deallocate sql)
		rows
	)
)

(define (Sqlite3:rowid)
	(assert-connected)
	(sqlite3_last_insert_rowid db)
)

(define (Sqlite3:changes)
	(assert-connected)
	(sqlite3_changes db)
)

(define (Sqlite3:version)
	(sqlite3_libversion_number)
)

(define (Sqlite3:table-exists? table-name)
	(assert-connected)
	(not (null? (rows-for-query "SELECT NULL FROM sqlite_master WHERE tbl_name = ?" (list table-name))))
)

(define (Sqlite3:last-error)
	(list error-code error-msg)
)

;---------------------------------------------------------------
; !Sqlite3.SQL - Sqlite3 Specific Public API
;---------------------------------------------------------------

;; @syntax (Sqlite3:set-timeout <int-ms>)
;; <p>Sets the sqlite3's busy timeout for this connection in milliseconds.</p>
;; <p>By default the timeout is set to 30 seconds.</p>
;; @return nil or non-nil on success
(define (Sqlite3:set-timeout ms)
	(assert-connected)
	(failable (sqlite3_busy_timeout db ms))
)

;---------------------------------------------------------------
; !Sqlite3.SQL - Private definitions
;---------------------------------------------------------------

(context Sqlite3.SQL)

; import some declaractions from Sqlite3
(def-new 'Sqlite3:error-codes)
(def-new 'Sqlite3:good-errors)
(def-new 'Sqlite3:failable)

; sqlite3 types
(constant 'SQLITE_INTEGER 1 'SQLITE_BLOB    4
          'SQLITE_FLOAT   2 'SQLITE_NULL    5
          'SQLITE_TEXT    3
)

;---------------------------------------------------------------
; !Sqlite3.SQL - Private Constructor
;---------------------------------------------------------------

(define (Sqlite3.SQL:Sqlite3.SQL _db _stmt _sql)
	(set 'db       _db                         ; the Sqlite3 db context (or sub-context)
	     'stmt     _stmt                       ; the sqlite3_stmt pointer
	     'sql      _sql                        ; the original SQL (in case of SQLITE_SCHEMA)
	     'num-cols (sqlite3_column_count stmt) ; the number of columns in this table
	)
	; the column types
	(dotimes (idx num-cols)
		; idx is a double so we use 'int' to convert it
		(push (sqlite3_column_type stmt (int idx)) col-types -1)
	)
	true ; it's possible for num-cols to be 0, i.e. in an update
)

;---------------------------------------------------------------
; !Sqlite3.SQL - Public API
;---------------------------------------------------------------

(define (Sqlite3.SQL:bind-params params)
	(let (	first-param	(params 0)
			strategy	binding-strategy-incremental
		)
		; choose strategy based on the form of the params
		(if (and (list? first-param) (= 2 (length first-param)))
			; if it's a doublet then we use one of these strategies:
			(if (starts-with (first first-param) "?")
				(set 'strategy binding-strategy-specific)
				(set 'strategy binding-strategy-keyword)
			)
		)
		; passing in no arguments resets the strategy
		(strategy)
		; go through the parameters and bind them
		(dolist (param params)
			(zero? (strategy stmt param))
		)
	)
)

(define (Sqlite3.SQL:next-row)
	((eval next-row-sym))
)

(define (Sqlite3.SQL:reset)
	(zero? (failable (sqlite3_reset stmt)))
)

(define (Sqlite3.SQL:col-name col-num)
	(get-string-cast string (sqlite3_column_name stmt col-num))
)

(define (Sqlite3.SQL:col-count)
	(sqlite3_column_count stmt)
)

(define (Sqlite3.SQL:close)
	(when (and stmt (failable (sqlite3_finalize stmt)))
		(setf stmt nil)
		true ; indicate success
	)
)

(define (Sqlite3.SQL:dealloc)
	; we can't simply call 'close' because it's a built-in function
	(Sqlite3.SQL:close)
)

;---------------------------------------------------------------
; !Sqlite3.SQL - Binding
;---------------------------------------------------------------

(define (bind-int64)
	(failable (sqlite3_bind_int64 stmt idx value))
)
(define (bind-int32)
	(failable (sqlite3_bind_int stmt idx value))
)
(define (bind-float32)
	(failable (sqlite3_bind_double stmt idx value))
)
(define (bind-float64)
	(bind-param-at-index stmt (string value) idx)
)
;; TODO: figure out how to do this properly, test on a bunch of different architectures
;;       for bind-float it may actually be the opposite (string for 32, double for 64)
(if NEWLISP64
	(set 'bind-int bind-int64 'bind-float bind-float64)
	(set 'bind-int bind-int32 'bind-float bind-float32)
)

(define (bind-param-at-index stmt value idx)
	(cond
		((integer? value) (bind-int))
		((string? value) (failable (sqlite3_bind_text stmt idx value (length value) -1)))
		; ((float? value) (bind-float))
		((float? value) (bind-param-at-index stmt (string value) idx))
		((nil? value) (failable (sqlite3_bind_null stmt idx)))
		; DF.BLOB is the vehicle for using 'sqlite3_bind_blob' instead of 'sqlite3_bind_text'
		((context? value) (failable (sqlite3_bind_blob stmt idx value:blob (length value:blob) -1)))
		((true? value) (bind-param-at-index stmt 1 idx))
		(true (throw-error "can't bind; unhandled type for value: " value))
	)
)

(define (binding-strategy-keyword stmt param , idx)
	(when stmt
		(set 'idx (sqlite3_bind_parameter_index stmt (first param)))
		(bind-param-at-index stmt (last param) idx)
	)
)

(define (binding-strategy-incremental stmt param)
	(if stmt
		(bind-param-at-index stmt param (++ .bsi-idx))
		(set '.bsi-idx 0) ; reset it
	)
)

(define (binding-strategy-specific stmt param)
	(when stmt
		(bind-param-at-index stmt (last param) (int (rest (first param))))
	)
)
;---------------------------------------------------------------
; !Sqlite3.SQL - next-row-sym stuff
;---------------------------------------------------------------

(define (get-string-cast cast ptr , temp)
	(if-not (zero? ptr) (cast (get-string ptr)))
)

(define (get-row , (row '()) type i ptr len buf)
	(dotimes (idx num-cols)
		(set 'i (int idx)) ; all loop vars are float
		(set 'type (sqlite3_column_type stmt i))
		(if (= type SQLITE_INTEGER)
			(push (sqlite3_column_int64 stmt i) row -1)
			(= type SQLITE_TEXT)
			(push (get-string-cast string (sqlite3_column_text stmt i)) row -1)
			(= type SQLITE_BLOB)
			(begin
				(set 'ptr (sqlite3_column_blob stmt i)
				     'len (sqlite3_column_bytes stmt i)
				     'buf (dup "\000" len))
				(if (zero? ptr)
					(push nil row -1)
					(begin (cpymem ptr buf len) (push (DF.BLOB buf) row -1)))
			)
			; newLISP can't handle sqlite3_column_double
			(= type SQLITE_FLOAT)
			(push (get-string-cast float (sqlite3_column_text stmt i)) row -1)
			(= type SQLITE_NULL)
			(push nil row -1)
		)
	)
	row
)

(define (next-row-regular)
	(when (failable (sqlite3_step stmt))
		(if (= error-code SQLITE_ROW)
			(get-row)
			(= error-code SQLITE_DONE)
		)
	)
)

(define (next-row-for-v1)
	(failable (sqlite3_step stmt))
	(if (= error-code SQLITE_ROW)
		(begin
			; we've obtained a lock on the table, we should no longer get SQLITE_SCHEMA
			(setf next-row-sym 'next-row-regular)
			(get-row)
		)
		(= error-code SQLITE_DONE)
		true
		(= error-code SQLITE_ERROR)
		(begin
			; this can only happen on the *first* call to step
			; and we need to reload the statement
			(failable (sqlite3_reset stmt))
			(if (= SQLITE_SCHEMA error-code)
				(and (rerun-stmt) (next-row-for-v1)) ; try again
				(begin
					(DF:log-err "Error " error-code " running: " sql)
					(Sqlite3.SQL:close) ; clean up
					nil ; this indicates failure
				)
			)
		)
	)
)

(define (rerun-stmt , tmp)
	(when (setf tmp (db:prepare-sql sql))
		(if (failable (sqlite3_transfer_bindings stmt tmp:stmt))
			(begin
				(Sqlite3.SQL:close)   ; finalize the old one
				(setf stmt tmp:stmt)  ; grab the new pointer
				(setf tmp:stmt nil)   ; set theirs to nil so that we can safely deallocate it
				(deallocate tmp)      ; this should return true
			)
			(begin (deallocate tmp) nil)
		)
	)
)

; we use a symbol because next-row-for-v1 changes the value
; of next-row-sym, and if we made it a direct reference, i.e.:
;   (setf next-row-func next-row-regular)
; then newLISP would crash because a function that is currently executing,
; that being next-row-func, is being redefined while it's executing.
(if Sqlite3:prepare_v2
	(setf next-row-sym 'next-row-regular)
	(setf next-row-sym 'next-row-for-v1)
)

(context MAIN)
