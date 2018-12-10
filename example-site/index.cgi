#!/usr/bin/env newlisp
;; Dragonfly web framework
;; 
;;  Copyright (C) <2009>  <Marc Hildmann>
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
;;
;; To test this locally:
;; 
;; $ cd /path/to/example-site
;; $ ./newlispServer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load Dragonfly web framework

; create this file to override any values in the default config.lsp file
(constant 'CONFIG_OVERRIDE "dragonfly-framework/config-override.lsp")

(load "dragonfly-framework/config.lsp")
(when (file? CONFIG_OVERRIDE) (load CONFIG_OVERRIDE))
(load "dragonfly-framework/dragonfly.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main entry point
;;

(define (run)

	(Dragonfly:listener)

)

(run)
