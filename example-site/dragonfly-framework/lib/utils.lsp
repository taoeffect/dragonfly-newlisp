;; NOTE: it's OK to load this file multiple times

;;  Copyright (C) <2009> <Greg Slepak>
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
;; @author Greg Slepak <greg at taoeffect.com>

(context 'Dragonfly)

(define (load-once)
	; check if the last argument is a context (to behave like 'load' does)
	(let (ctx (let (_ctx (last $args)) (if (context? _ctx) _ctx MAIN)))
		(doargs (file)
			(unless (or (context? file) (find file _loaded))
				(push file _loaded)
				(Dragonfly:saved-load file ctx)
			)
		)
	)
)

; We define our own module function so that we can easily support
; shared hosting services where the modules directory might not be
; in /usr/share/newlisp/modules.
(define (module module-to-load)
	(if-not newlisp-dir (throw-error "need value 'newlisp-dir' from dragonfly_config.lsp!"))
	(load-once (append newlisp-dir "/modules/" module-to-load))
)

(context 'MAIN)

; swap the MAIN functions for ours
(unless Dragonfly:saved-load
	(constant 'Dragonfly:saved-load MAIN:load)
	(constant 'MAIN:load Dragonfly:load-once)
	(constant 'MAIN:module Dragonfly:module)
)
