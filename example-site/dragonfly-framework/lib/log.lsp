;  Copyright (C) <2009> <Marc Hildmann, Greg Slepak>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;; @module log.lsp
;; @description Provides convenient logging facility for all of Dragonfly.
;; @author Greg Slepak
;; <p>To avoid checking the value of 'Dragonfly:LOG_LEVEL' each time something is logged
;; the logging functions that correspond to a lower logging level are disabled
;; when this file is loaded.</p>
;; <p>It is possible to change the 'LOG_LEVEL' at runtime, but the way it is done
;; is slightly tricky because 'MAIN:load' is overwritten by Dragonfly
;; to provide once-only loading. If you want to dynamically change the
;; log-level (say in a plugin), then you will need to use the remapped 'sys-load'
;; function to force a reload of this file:</p>
;; <pre> (context 'DF)
;; (constant 'LOG_LEVEL 'LOG_DEBUG) ; enable debug logging at runtime
;; (sys-load (string DRAGONFLY_ROOT "/lib/log.lsp"))
;; (context MAIN)</pre>
;; There are four log levels defined:
;; <pre> (map set '(LOG_DEBUG LOG_INFO LOG_WARN LOG_ERROR)
;;          '(        0        1        2         3))</pre>

(context 'Dragonfly)

(map set '(LOG_DEBUG LOG_INFO LOG_WARN LOG_ERROR)
         '(        0        1        2         3))

;; @syntax (Dragonfly:log-func <str-level> <str-msg>)
;; @param <str-level> a string representing the log level (ex: "[DEBUG]: ")
;; @param <str-msg> a string containing the message
;; <br/>This function appends to the file specified by 'Dragonfly:LOG_FILE_PATH'
;; and prepends date information to the log.
;; <pre> (log-func "[CRAZY!]: " (string user " just did something crazy!"))
;; => Oct 26 01:44:24 [CRAZY!]: johnny-b just did something crazy!</pre>
;; All other log functions call this one. You can override this function
;; at runtime to provide your own logging behavior.
(define (log-func level msg)
	(append-file LOG_FILE_PATH (string (date (date-value) 0 "%Y-%m-%d %H:%M:%S ") level msg "\n"))
)

;; @syntax (Dragonfly:log-debug)
;; <br>String-concats its arguments and calls 'log-func' with <level> set to "[DEBUG]: "
(define (log-debug)
	(log-func "[DEBUG]: " (apply string $args))
)

;; @syntax (Dragonfly:log-info)
;; <br>String-concats its arguments and calls 'log-func' with <level> set to "[INFO]: "
(define (log-info)
	(log-func "[INFO]: " (apply string $args))
)

;; @syntax (Dragonfly:log-warn)
;; <br>String-concats its arguments and calls 'log-func' with <level> set to "[WARNING]: "
(define (log-warn)
	(log-func "[WARNING]: " (apply string $args))
)

;; @syntax (Dragonfly:log-err)
;; <br>String-concats its arguments and calls 'log-func' with <level> set to "[ERROR]: "
(define (log-err)
	(log-func "[ERROR]: " (apply string $args))
)

(if (> (eval LOG_LEVEL) LOG_DEBUG) (define (log-debug)))
(if (> (eval LOG_LEVEL) LOG_INFO) (define (log-info)))
(if (> (eval LOG_LEVEL) LOG_WARN) (define (log-warn)))

(context MAIN)
