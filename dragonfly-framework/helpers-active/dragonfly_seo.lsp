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
; !SEO functions (just ideas - really alpha!!!)
;===============================================================================

(define (clean-html url)
	(set 'page (get-url url))
	(replace "<[^>]*>" page "" 0)
	(println page)
)
   
(define (check-meta-description)
	
	(set 'description "<meta name=\"description\"`
	      content=\"newLISP is a general purpose scripting language for developing web applications and programs in general and in the domains of artificial intelligence (AI) and statistics.\">")
	;;(println (regex "<meta name=\"description\" content=\"(.*)\">/i" "<meta name=\"description\"`
	;;	      content=\"newLISP is a general purpose scripting language for developing web applications and programs in general and in the domains of artificial intelligence (AI) and statistics.\">"))
	;;(println $0)
	(set 'description-length (length description))
	(or
		(if (< description-length 60) (println "Your meta description with "description-length" characters is too short."))
		(if (> description-length 170) (println "Your meta description with "description-length" characters is too long."))
	)
	(and
		(if (>= description-length 60))
		(if (<= description-length 170))
		(println "Your meta description with "description-length" characters is ideal.")
	)
)

(define (check-meta-keywords keywords)
	
	(set 'number-keywords (length keywords))
	(or
		(if (< number-keywords 5) (println "The number of meta-keywords "number-keywords" are too small."))
		(if (> number-keywords 10) (println "The number of meta-keywords "number-keywords" are too much."))
	)
	(and
		(if (>= number-keywords 5))
		(if (<= number-keywords 10))
		(println "Your number of meta-keywords "number-keywords" are ideal.")
	)
)

;===============================================================================
; !Google functions
;===============================================================================

(define (google-results-domain domain)
	(set 'json (get-url (string "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="domain)))
	(begin
		(regex "\"estimatedResultCount\":\"(.*)\"," json)
		(set 'erc $1)	
	)
)

(context Dragonfly)