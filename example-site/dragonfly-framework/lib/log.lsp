;;  Copyright (C) <2009> <Marc Hildmann, Greg Slepak>
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
;; @author Greg Slepak

(context 'Dragonfly)

(map set '(LOG_DEBUG LOG_INFO LOG_WARN LOG_ERROR)
         '(        0        1        2         3))


(define (log-func level msg)
	(append-file LOG_FILE_PATH (string (date (date-value) 0 "%b %d %H:%M:%S ") level msg "\n"))
)

(define (log-debug)
	(log-func "[DEBUG]: " (apply string $args))
)

(define (log-info)
	(log-func "[INFO]: " (apply string $args))
)

(define (log-warn)
	(log-func "[WARNING]: " (apply string $args))
)

(define (log-err)
	(log-func "[ERROR]: " (apply string $args))
)

(if (> (eval LOG_LEVEL) LOG_DEBUG) (define (log-debug)))
(if (> (eval LOG_LEVEL) LOG_INFO) (define (log-info)))
(if (> (eval LOG_LEVEL) LOG_WARN) (define (log-warn)))

(context MAIN)
