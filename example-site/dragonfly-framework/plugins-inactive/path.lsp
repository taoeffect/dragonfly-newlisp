; TODO: implement string based and get rid of the class based

; (define (sanitize-path p)
; 	(replace {([^\\])?//} p "/" 0)
; 	(replace {(.*/?)([^/]+/)?\.\./} p 0)
; 	(replace ".\./" p "/" 0)
; 	(trim p)
; )
; 
; (define (abs-path p (base (real-path)))
; 	(sanitize-path (string base separator p))
; )
; 
; (define (add-path-components path)
; 	()
; )

(context 'Path)

(set 'separator		"/")
(set 'ext-regex		(regex-comp {.*\.(.+)$}))
(set 'ext-del-regex	(regex-comp {(.*)\..+$}))
(constant 'S_IFLNK	40960)

(define (Path:Path)
	(let ((p (apply MAIN:string (args))) (comps nil))
		(when (!= p separator)
			(setq p (trim p " " separator))
			(if (starts-with p "./")
				(pop p 0 2))
			(replace "/./" p "/")
		)
		; TODO: handle ../
		; do this *after* the above
		(replace {/+}  p "/" 0) ; TODO: change to use separator

		;; deal with the path components
		(case p
			(""         (setq comps '("")))
			("/"        (setq comps (list separator))) ; TODO: is this a bug? why can't I use separator?
			(true       (setq comps (parse p separator))
			            (if (= "" (comps 0)) ;; this can happen if p is "/foo" for example
			            	(setf (comps 0) "/")))
		)
		; note sure why this can't be (list Path p comps) ... bug?
		(list (context) p comps)
	)
)
; returns: string (duh..)
(define (Path:string p)
	(p 1)
)
; returns: list
(define (Path:components p)
	(p 2)
)
; returns: string
(define (Path:extension p)
	(if (regex ext-regex (:string p) 0x10000)
		$1
		""
	)
)
; returns: string
(define (Path:last-comp p)
	(last (:components p))
)
; returns: true or nil
(define (Path:exists? p)
	(file? (:string p))
)
; returns: true if the last path component starts with a dot
(define (Path:invisible? p)
	(starts-with (:last-comp p) ".")
)
; returns: true if p is a link
(define (Path:link? p)
	(let (info (file-info (:string p) 1))
		(if info (!= 0 (& S_IFLNK info)))
	)
)
; returns: true if p is subpath of 'parent'
(define (Path:subpath? p parent)
	(if (string? parent)
		(set 'parent (Path parent)))
	(and (starts-with (:string p) (:string parent))
		 (> (length (:components p)) (length (:components parent))))
)
; returns: int
(define (Path:touch p)
	(! (format {touch "%s"} (:string p)))
)
; returns: Path
(define (Path:del-comp p (num-to-delete 1))
	(let (comps (:components p))
		(while (and (>= (dec num-to-delete) 0)
					(> (length comps) 0)
		            (or (!= 1 (length comps)) (!= (comps 0) "/")))
			(pop comps -1)
		)
		(setq comps (join comps separator))
		((p 0) comps)
	)
)
; returns: Path
(define (Path:del-ext p)
	(if (regex ext-del-regex (:string p) 0x10000)
		((p 0) $1)
		p
	)
)
; returns: Path
(define (Path:add-comp p component)
	((p 0) (:string p) separator (if (string? component) component (:string component)))
)
; returns: Path
(define (Path:add-ext p ext)
	((p 0) (:string p) "." ext)
)
(context MAIN)
