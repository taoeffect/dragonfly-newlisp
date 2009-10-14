;;  Copyright (C) <2009> <Marc Hildmann>
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; @module Dragonfly
;; @author Marc Hildmann <marc.hildmann at gmail.com>
;; @version 0.20
;; 
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; just like newLISP itself.</p>

;===============================================================================
; !Defining new context
;===============================================================================

(context 'Dragonfly)

;===============================================================================
; !sqlite3 Wrapper
;===============================================================================


;; @syntax (Dragonfly:sqlite-open <databasename>)
;; @param <databasename> string containing the database name
;; <p>Open the SQLite database or creates it, if it does not exist.</p>
;;
(define (sqlite-open databasename)
	;; close old connections
	(sql3:close)
	
	(if (sql3:open databasename) 
		(set 'flashnotice "Database was successfully opened or created.") 
	) 	
)

;; @syntax (Dragonfly:sqlite-tables)
;; <p>Shows the existing tables in the current database.</p>
;;
(define (sqlite-tables) 
	(println "
	<div id='dragonfly_database-information' style='border:1px dotted #00aeef; width:800px; padding:8px; margin-top:20px;margin-bottom:20px' >
	<h2>Existing tables in SQLite Database</h2><pre>
	"
	(sql3:tables)"
	</pre></div>")
)

;; @syntax (Dragonfly:sqlite-columns)
;; <p>Shows the existing columns in a given table.</p>
;;
(define (sqlite-columns table) 
	(println "
	<div id='dragonfly_database-information' style='border:1px dotted #00aeef; width:800px; padding:8px; margin-top:20px;margin-bottom:20px' >
	<h2>Existing columns in SQLite table <i>"table"</i></h2><pre>
	"
	(sql3:columns table)"
	</pre></div>")
)

(define (sqlite-empty-table table) 
	(set 'query (append "DELETE FROM "table))
	(sql3:sql query)
)

(define (sqlite-query query) 
	(set 'sqlarray (sql3:sql query)) ; results of query
	;; close old connections
	(sql3:close)	
)

(define (sqlite-insert table values)
	(set 'query (append "INSERT INTO "table" VALUES ("values")"))
	(sql3:sql query)
)

(define (sqlite-get-tabledata query) 
	(set 'sqlarray (sql3:sql query)) ; results of query
	
)

(context Dragonfly)
