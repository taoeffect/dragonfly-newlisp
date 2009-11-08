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
; !Loading modules and defining new context
;===============================================================================

(context 'Dragonfly)

(constant 'databases-path (string DOCUMENT_ROOT "/databases/"))

;===============================================================================
; !nldb Wrapper for a pure newLISP Database (flat)
;===============================================================================

;; @syntax (Dragonfly:load-database <database>)
;; @param <database> database filename
;; <p>Loads the specified nldb database in the default
;; directory /databases.</p>
;; 
(define (load-database database)
	(nldb:load-db (string databases-path database))
)

;; @syntax (Dragonfly:save-database <database>)
;; @param <database> database filename
;; <p>Saves the specified nldb database in the default
;; directory /databases.</p>
;; 
(define (save-database database)
	(nldb:save-db (string databases-path database))
)

;; @syntax (Dragonfly:show-database)
;; <p>Writes some information about the currently used nldb</p>
;;
(define (show-database)
	(println "
	<div id='dragonfly_database-information' style='border:1px dotted #00aeef; width:700px; padding:8px; margin-top:20px;' >
	<h2>Database information</h2><pre>
	")
	(with-wrapped-print (nldb:show))
	(println "</pre></div>")
)

;; @syntax (Dragonfly:count-columns database tablename)
;; <p>Returns the number of columns of the given table.</p>
;;
(define (count-columns database tablename)
	(Dragonfly:load-database database)
	(set 'table tablename)
	(print (nldb:count-columns table))
)

;; @syntax (Dragonfly:count-columns <database>)
;; @param <database> database filename
;; <p>Saves the specified nldb database in the default
;; directory /databases.</p>
;; 
(define (save-database database)
	(nldb:save-db (string databases-path database))
)

;; @syntax (Dragonfly:add-row <database> <tablename> <data>)
;; @param <database> the database filename
;; @param <tablename> target table
;; @param <data> data supplied as a list - all fields must be given
;; <p>Saves the data into the specified table.</p>
;; 
(define (add-row database tablename data)
	(Dragonfly:load-database database)
	(set 'table tablename)
	(nldb:add-row table data)
	(Dragonfly:save-database database)
)

;; @syntax (Dragonfly:new-row <database> <tablename> <data>)
;; @param <database> the database filename
;; @param <tablename> target table
;; @param <data> data supplied as a list - all fields must be given
;; <p>Saves the data into the specified table.</p>
;; 
(define (new-row database tablename data)
		(Dragonfly:load-database database)
		(set 'table tablename)
		(nldb:new-row table data)
		(Dragonfly:save-database database)
)

;; @syntax (Dragonfly:create-database database)
;; <p>Creates a new nldb database in the default
;; directory /databases or loads an existing database.</p>
;;
(define (use-database databasename)
	(if (not (find databasename ( directory databases-path )) )
		(begin
			(set 'nldb:tables '()) ; prepare a clean table
			(Dragonfly:save-database databasename) ; creating database
		)
		(Dragonfly:load-database databasename) ; else load existing database
	)
)

(define (create-table database tablename fields)
	(Dragonfly:load-database database)
	(set 'existing-tables nldb:tables)
	(set 'table tablename)
	(if (not (find table existing-tables))
		(begin
			(nldb:create-table table fields)
			(Dragonfly:save-database database)
		)
		;;(println "Table not created. Found existing table.")
	)
)

(context Dragonfly)