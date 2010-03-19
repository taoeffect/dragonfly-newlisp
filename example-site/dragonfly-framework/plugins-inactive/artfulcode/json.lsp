;; @module Json
;; @author Jeff Ober <jeffober@gmail.com>, Greg Slepak added fix for proper JSON (" instead of ')
;; @version 2.0
;; @location http://static.artfulcode.net/newlisp/json.lsp
;; @package http://static.artfulcode.net/newlisp/json.qwerty
;; @description JSON parser and encoder; requires util.lsp (updated for newlisp 10)
;; <p>Library for parsing JSON data and serializing lisp into JSON.</p>
;; <h4>Version history</h4>
;; <b>2.0</b>
;; &bull; completely rewrite of decoder (thanks to Andrew Pennebaker for pointing out the bugs in the original)
;; 
;; <b>1.2</b>
;; &bull; fixed incompatibilities with newlisp 10
;; 
;; <b>1.1</b>
;; &bull; added simple escape routine to outputted string values
;; 
;; <b>1.0</b>
;; &bull; initial release

(DF:activate-plugin "artfulcode/util")

(context 'Json)

;; @syntax (Json:lisp->json <expr>)
;; @param <expr> expression to be converted to JSON
;; <p>Converts expression <expr> to JSON.  Association lists and
;; contexts are converted into objects.  Other lists and arrays are
;; converted into JSON arrays.</p>
;; @example
;; (Json:lisp->json '((a 1) (b 2)))
;; => "{ 'A': 1, 'b': 2 }"
;; (Json:lisp->json '(1 2 3 4 5))
;; => "[1, 2, 3, 4, 5]"
(define (lisp->json lisp)
  (case (type-of lisp)
    ("boolean" (if lisp "true" "false"))
    ("quote" (lisp->json (eval lisp)))
    ("symbol" (format {"%s"} (term lisp)))
    ("string" (format {"%s"} (simple-escape lisp)))
    ("integer" (string lisp))
    ("float" (string lisp))
    ("list" (if (assoc? lisp)
                (format "{ %s }"
                        (join (map (fn (pair)
                                     (format {"%s": %s}
                                             (if (symbol? (pair 0))
                                                 (term (pair 0))
                                                 (string (pair 0)))
                                             (lisp->json (pair 1))))
                                   lisp)
                              ", "))
                (string "[" (join (map lisp->json lisp) ", ") "]")))
    ("array" (string "[" (join (map lisp->json lisp) ", ") "]"))
    ("context" (let ((values '()))
                 (dotree (s lisp)
                   (push (format {"%s": %s}
                                 (term s)
                                 (lisp->json (eval s)))
                         values -1))
                 (format "{ %s }" (join values ", "))))
    (true (throw-error (format "invalid lisp->json type: %s" lisp)))))

(define (simple-escape str)
  (replace {[\n\r]+} str {\n} 4)
  ;(replace {'} str {\'} 4)
	(replace {"} str {\"})
  str)

;; @syntax (Json:json->lisp <str-json>)
;; @param <str-json> a valid JSON string
;; <p>Parses a valid JSON string and returns a lisp structure.
;; Arrays are converted to lists and objects are converted to 
;; assocation lists.</p>
;; @example
;; (Json:json->lisp "[1, 2, 3, 4]")
;; => (1 2 3 4)
;; (Json:json->lisp "{ 'x': 3, 'y': 4, 'z': [1, 2, 3] }")
;; => (("x" 3) ("y" 4) ("z" (1 2 3)))
(define (json->lisp json)
  (first (lex (tokenize json))))

(define number-re (regex-comp {^([-+\deE.]+)} 1))
(define identifier-re (regex-comp {([$_a-zA-Z][$_a-zA-Z0-9]*)(.*)} 4))

(define (read-number text , matched n)
  "Reads in a number in any Javascript-permissible format and attempts to
  convert it to a newLISP float. If the number's absolute value is greater
  than 1e308 (defined as +/-INF in newLISP), the number is returned as a
  string."
  (setf text (trim text))
  (when (setf matched (regex number-re text 0x10000))
    (setf n (pop text 0 (matched 5)))
    (list (if (> (abs (float n)) 1e308) n (float n)) text)))

(define (read-string text , quot c escaped split-index str)
  (setf quot (pop text) str "")
  (catch
    (until (empty? (setf c (pop text)))
      (if (and (= c quot) (not escaped))
        (throw $idx)
        (extend str c))
      (setf escaped (and (not $it) (= c {\})))))
  (list str text))

(define (read-identifier text , matched)
  (setf text (trim text))
  (setf matched (regex identifier-re text 0x10000))
  (list (case (nth 3 matched)
          ("true" true) ("TRUE" true)
          ("false" nil) ("FALSE" nil)
          ("null" nil) ("NULL" nil)
          (true (nth 3 matched)))
        (nth 6 matched)))

(define (tokenize text (acc '()) , tok tail n)
  (setf text (trim text))
  (cond
    ((empty? text) acc)
    ((regex {^\s+} text 4)
     (tokenize (replace {^\s+} text "" 0) acc))
    ((regex number-re text 0x10000)
     (map set '(tok tail) (read-number text))
     (push tok acc -1)
     (tokenize tail acc))
    ((regex {^['"]} text)
     (map set '(tok tail) (read-string text))
     (push tok acc -1)
     (tokenize tail acc))
    ((regex [text]^[{}\[\]:,][/text] text)
     (setf tok (pop text))
     (case tok
       ("{" (push 'OPEN_BRACE acc -1))
       ("}" (push 'CLOSE_BRACE acc -1))
       ("[" (push 'OPEN_BRACKET acc -1))
       ("]" (push 'CLOSE_BRACKET acc -1))
       (":" (push 'COLON acc -1))
       ("," (push 'COMMA acc -1)))
     (tokenize text acc))
    (true
     (map set '(tok tail) (read-identifier text))
     (push tok acc -1)
     (tokenize tail acc))))

(define (lex tokens, (tree '()) (loc '(-1)) (depth 0) (mark 0))
  ;; Note: mark is used to match colon-pairings' depth against the current
  ;; depth to prevent commas in a paired value (e.g. foo: [...] or foo: {})
  ;; from popping the stack.
  (unless (find (first tokens) '(OPEN_BRACKET OPEN_BRACE))
    (throw-error "A JSON object must be an object or array."))
  (dolist (tok tokens)
    (case tok
      (OPEN_BRACKET
        (++ depth)
        (push (list) tree loc)
        (push -1 loc))
      (OPEN_BRACE
        (++ depth)
        (push (list) tree loc)
        (push -1 loc))
      (CLOSE_BRACKET
        (-- depth)
        (pop loc))
      (CLOSE_BRACE
        (-- depth)
        (pop loc))
      (COLON
        (push (list (pop tree loc)) tree loc)
        (push -1 loc)
        (setf mark depth))
      (COMMA
        (when (= mark depth)
          (setf mark nil)
          (pop loc)))
      (true
        (push tok tree loc))))
  tree)

(context MAIN)
