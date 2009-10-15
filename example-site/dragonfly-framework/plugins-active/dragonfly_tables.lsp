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

;===============================================================================
; !Table Functions
;===============================================================================

;; @syntax (Dragonfly:table_open <head_elements>)
;; @param <head_elements> a list containing the table head
;; <p>Writes a standard table open element including a head, generated from a list.</p>
;; 
(define (table_open table_head_elements)
  (set 'table_colspan table_head_elements) ;count of table_head_elements for defining a correct table-colspan
  (print "
	<!-- BEGIN table -->
	<table>
	<thead><tr>"
		(dolist (th-element table_head_elements)
			(if (= $idx 0) (print "<th class='table_firstcolumn'>"th-element"</th>") (print "<th>"th-element"</th>"))
		)
	"</tr></thead>"
  )
)

;; @syntax (Dragonfly:table_data <table_elements>)
;; @param <table_elements> a list containing all table elements (row by row, from left to right), including empty cells
;; <p>Writes the table data generated from a list.</p>
;; 

(define (table_data table_elements)
	; initialize maximum length per row with offset -1, because counting starts at "0"	
	(set 'rowlength (length table_colspan))	
	(set 'rowlength (- rowlength 1))
	(print "<tbody>"
			(dotimes (i 1)
				(dolist (td-element table_elements)
					; here we do some modulo calculation to determine if a new row begins
					(if (= (mod $idx (length table_colspan)) 0) (print "<tr><td class='table_firstcolumn'>"td-element"</td>") (print "<td>"td-element"</td>"))
					(if (= (mod $idx (length table_colspan)) rowlength) (println "</tr>"))
				)
			)	
			"</tbody>"
  	)
)

;; @syntax (Dragonfly:table_footer <table_footer_text>)
;; @param <table_footer_text> a string containing the table footer text
;; <p>Writes a standard table footer with number of colspan generated from the last table-head.</p>
;; 
(define (table_footer table_footer_text)
  (print "
	<tfoot><tr><td class='table_foot' colspan='"(length table_colspan)"'>"table_footer_text"</td></tr></tfoot>"
  )
)

;; @syntax (Dragonfly:table_close)
;; <p>Writes a standard table close element.</p>
;; 
(define (table_close)
  (print "</table><!-- END table -->")
)

(context Dragonfly)