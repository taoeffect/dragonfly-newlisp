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
; !Image Functions
;===============================================================================

;; @syntax (Dragonfly:read-exif <image-url>)
;; @param <image-url> a string containing the image url
;; <p>Reads EXIF information stored in the image.</p>
;; 
(define (read-exif image-url)
  (set 'imagepath (string DOCUMENT_ROOT image-url))

  (print "Path to image: ")
  	(print imagepath)
	(print "<br/>")

  (print "Photo taken on ")
  	(set-locale "de_DE")
  	(print (date (file-info imagepath 6)) " UTC")
	(print "<br/>")

	
)

(context Dragonfly)
