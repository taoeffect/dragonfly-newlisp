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

; protect against situation where one of the load functions is used to
; load this file, thereby redefining the function itself while it's running
; and causing newlisp to crash.
; This may also speed up the loading of this file the second time around.
(unless load-once
	
	(define (load-once)
		; check if the last argument is a context (to behave like 'load' does)
		(let (ctx (let (_ctx (last $args)) (if (context? _ctx) _ctx MAIN)))
			(doargs (file)
				(unless (or (context? file) (find file load-once.loaded))
					(push file load-once.loaded)
					(sys-load file ctx)
				)
			)
		)
	)
	
	(define (Dragonfly:println)
		(apply Dragonfly:print (push "\n" (args) -1))
	)
	
	(define (Dragonfly:print)
		(write-buffer Dragonfly:STDOUT (apply string (args)))
		(last (args)) ; to behave the same way as print
	)
	
	;; @syntax (define-subclass (<sym-subclass> <ctx>) <method-1> ...])
	;; @param <sym-subclass> Symbol representing name of the subclass
	;; @param <ctx> The FOOP class you'll be subclassing
	;; <p>This macro must be called in the MAIN context.</p>
	;; @example
	;; (new Class 'Foo)
	;; (define (Foo:get x) (x 1))
	;; (define (Foo:set x v) (setf (x 1) v) x)
	;; 
	;; (define-subclass (Bar Foo)
	;; 	((get x) (x 2))
	;; 	((set x v) (setf (x 2) v) x)
	;; 	((str x) (string x))
	;; )
	;; 
	;; (:get (Foo 1 2)) => 1
	;; (:get (Bar 1 2)) => 2
	;; (:str (:set (Bar 1 2) 3)) => (Bar 1 3)
	(define-macro (define-subclass)
		(new (args 0 1) (args 0 0))
		(dolist (method (rest $args))
			(setf (method 0 0) (sym $it (args 0 0)))
			(eval (push 'define method))
		)
	)

	(define (regex-captcha regex-str str (options 0) (captcha 1))
		(if (regex regex-str str options)
			($ captcha)
		)
	)
	
	(define (load-files-in-dir dir regex-match)
		(dolist (x (directory dir regex-match))
			(load-once (string dir "/" x))
		)
	)
	
	; these two functions should be global (define-subclass should not)
	(global 'load-files-in-dir 'regex-captcha 'load-once)
	
	; swap these functions for ours and save the originals
	(constant (global 'sys-load) load)
	(constant 'load load-once)
	(constant (global 'sys-print) print)
	(constant 'print Dragonfly:print)
	(constant (global 'sys-println) println)
	(constant 'println Dragonfly:println)
)

