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
;; @module Dragonfly Twitter search plugin
;; @author Marc Hildmann <marc.hildmann at gmail.com>
;; @version 0.70
;; 
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; just like newLISP itself.</p>

;===============================================================================
; !Loading plugin into Dragonfly context
;===============================================================================

(context 'Dragonfly)

;===============================================================================
; !twitter functions
;===============================================================================

;; @syntax (Dragonfly:twitter-search <keyword> <max-items>)
;; @param <keyword> string containing the keyword for search
;; @param <max-items> INTEGER, maximum of items you want to show
;; <p>Writes the results of a Twitter search.</p>

(define (twitter-search keyword max-items)
	(set 'xml (get-url (string "http://search.twitter.com/search.atom?rpp="max-items"&q="keyword) ))
	(xml-type-tags nil nil nil nil) ; no extra tags
	(set 'sxml (xml-parse xml 31)) ; turn on SXML options
	(set 'entry-index (ref-all '(entry *) sxml match))
	(when (empty? entry-index)
		(println "No entries found")
	)
	(dolist (idx entry-index)
		(set 'entry (sxml idx))
		(set 'dateseconds (parse-date (lookup 'published entry) "%Y-%m-%dT%H:%M:%SZ")) ; convert string date to seconds
		
		(println
			"<div class='entry'>"
			"<div class='headline'>" (lookup 'title entry) "</div><br/>"
			"<div class='published'>" (date dateseconds 0 "%a %d %b %Y %H:%M:%S") "</div><div class='author'>By&nbsp;" (lookup '(author name) entry) "</div><br/>"
			"</div>"
		)
	)
)

(context MAIN)
