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
; !twitter functions
;===============================================================================

;; @syntax (Dragonfly:twitter-search <keyword>)
;; @param <keyword> string containing the keyword for search
;; <p>Writes the results of the search in nice speech bubbles.</p>
;;

(define (twitter-search keyword rpp)
  (set 'xml (get-url (string "http://search.twitter.com/search.atom?rpp="rpp"&q="keyword) ))
  (xml-type-tags nil nil nil nil) ; no extra tags
  (set 'sxml (xml-parse xml 31)) ; turn on SXML options
  (set 'entry-index (ref-all '(entry *) sxml match))
  (when (empty? entry-index)
    (println "No entries found")
  )
  (println "<div id='twitter_search_results'>")
  (dolist (idx entry-index)
	(println "<div class='bubble'><blockquote><p>")    
	(set 'entry (sxml idx))
    (println 
				(lookup 'title entry) 
				"</p></blockquote>"
				"<cite><strong>"
				(lookup '(author name) entry ) "</strong> on " 
			 	(lookup 'published entry) "</cite></div>")
	)
	(println "</div>")
)

(context Dragonfly) "debug"