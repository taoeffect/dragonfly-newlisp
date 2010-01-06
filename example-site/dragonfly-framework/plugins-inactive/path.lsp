;; @module path.lsp
;; @description Global convenience functions for dealing with file paths
;; @author Greg Slepak
;; @version 1.0
;; <br/>All of these functions are made global, so watch out for name conflicts.

(set 'file-ext-regex (regex-comp {^.*\.(.+)$})
     'del-ext-regex  (regex-comp {^(.*)\..+$})
     'basename-regex (regex-comp {^(.*/)?(.+)$})
     'dirname-regex  (regex-comp {^(.*)/([^/]+/?)$})
)

;; @syntax (file-ext <str-path>)
;; <p>Returns the file extension of the file in <str-path></p>
;; <pre> (file-ext "")
;; ;=> ""
;; (file-ext "asdf")
;; ;=> ""
;; (file-ext "asdf.")
;; ;=> ""
;; (file-ext "asdf.jpg")
;; ;=> "jpg"</pre>
(define (file-ext path)
	(if (regex file-ext-regex path 0x10000)
		$1
		""
	)
)

;; @syntax (del-ext <str-path>)
;; <p>Returns the <str-path> without the file extension removed.</p>
;; <pre> (del-ext "")
;; ;=> ""
;; (del-ext "asdf")
;; ;=> "asdf"
;; (del-ext "asdf.")
;; ;=> "asdf."
;; (del-ext "asdf.jpg")
;; ;=> "asdf"</pre>
(define (del-ext path)
	(if (regex del-ext-regex path 0x10000)
		$1
		path
	)
)

;; @syntax (basename <str-path>)
;; <p>Returns the filename component of <str-path>.</p>
;; <pre> (basename "")
;; ;=> ""
;; (basename "/")
;; ;=> ""
;; (basename "asdf")
;; ;=> "asdf"
;; (basename "/foo/bar")
;; ;=> "bar"
;; (basename "/foo/bar/")
;; ;=> "bar"</pre>
(define (basename path)
	(if (regex basename-regex path 0x10000)
		(trim $2 "/")
		""
	)
)

;; @syntax (dirname <str-path>)
;; <p>Returns the directory path component of <str-path>.</p>
;; <pre> (dirname "")
;; ;=> ""
;; (dirname "/")
;; ;=> "/"
;; (dirname "asdf")
;; ;=> "."
;; (dirname "/asdf/")
;; ;=> "/"
;; (dirname "asdf/foo")
;; ;=> "asdf"</pre>
(define (dirname path)
	(if (empty? path)
		""
		(if (regex dirname-regex path 0x10000)
			(if (empty? $1) "/" $1)
			(if (starts-with path "/") "/" ".")
		)
	)
)

(global 'file-ext 'del-ext 'basename 'dirname)
