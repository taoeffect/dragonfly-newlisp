;; @module util
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 2.0
;; @location http://static.artfulcode.net/newlisp/util.lsp
;; @package http://static.artfulcode.net/newlisp/util.qwerty
;; @description Various functions that the other libraries depend on (updated for newlisp 10).
;; Various helpful utilities for newLISP.  Requires newlisp 10+.
;; 
;; <h4>Version history</h4>
;; <b>2.1</b>
;; &bull; added with-open-device, partial
;; &bull; added make-array, array-iter and array-map
;; 
;; <b>2.0</b>
;; &bull; updated for newlisp 10 (backwards-incompatible)
;; &bull; refactored assoc? (now permits any key that satisfies 'atom?')
;; &bull; get-assoc is now a regular function whose arguments must be quoted
;; &bull; slot functions removed as new association list features make them redundant
;; &bull; dict-keys refactored and renamed to keys
;; &bull; refactored dokeys for a slight speed improvement
;; 
;; <b>1.4</b>
;; &bull; added <slot-value>
;; &bull; <with-slots> now supports string keys
;; &bull; fixed bug when calling <with-slots> from within a context
;; &bull; <with-slots> now permits renaming of variables in binding to avoid clashes in nested calls
;; &bull; added <get-assoc>
;; &bull; added <dict-keys>
;; &bull; added <dokeys>
;; 
;; <b>1.3</b>
;; &bull; <with-slots> now supports assoc data in the format '(key val-1 val-2 ...)' and '(key val)
;; 
;; <b>1.2</b>
;; &bull; fixed bug that caused <with-slots> to return only the first value of a list
;; 
;; <b>1.1</b>
;; &bull; added <fmap>, <with-slots>, and <add-assoc>
;; 
;; <b>1.0</b>
;; &bull; initial release

;; @syntax (type-of <object>)
;; @param <object> any object
;; <p>'type-of' introspects the type of the passed argument, object, and returns a string
;; representation of its type.  Correctly identifies FOOP types as well, returning the
;; string value of the first argument (by calling 'name' on the context of the list).</p>
;; @example
;; (type-of 10) => "integer"
;; (type-of "hello world") => "string"
;; (type-of true) => "boolean"
;; (type-of '(1 2 3)) => "list"
;; (type-of (fn (x) (+ x x))) => "lambda"
(setq type-of:types '("boolean" "boolean" "integer" "float" "string" "symbol" "context"
					  "primitive" "primitive" "primitive" "quote" "list" "lambda" "macro"
					  "array"))

(define (type-of:type-of x)
  (let ((type (type-of:types (& 0xf ((dump x) 1)))))
	  (if (and (= "list" type) (context? (first x)))
	      (name (first x))
		    type)))

;; @syntax (gensym [<ctx>])
;; @param <ctx> optional; the context in which to create the symbol (default: MAIN)
;; <p>Returns a symbol unique to the context passed.  If 'ctx' is nil, the symbol is
;; created in MAIN.  There is a hard limit on the number of possible symbols generated based on
;; the max integer value of the system.  Since newLISP wraps into negative numbers when passing
;; the max value, the effective max value is twice the systems maximum integer value.</p>
;; @example
;; (gensym) => gensym-1
;; (gensym) => gensym-2
;; 
;; (define foo:foo)
;; (gensym foo) => foo:gensym-1
;; (gensym foo) => foo:gensym-2
(define _gensym:_gensym)

(define (gensym:gensym (ctx MAIN) , ctx-name new-sym)
  (setf ctx-name (name ctx))
  (if (_gensym ctx-name)
    (begin
      (setf new-sym (string "gensym-" (_gensym ctx-name (+ 1 (_gensym ctx-name)))))
      (or (sym new-sym ctx) (gensym ctx)))
    (begin
      (_gensym ctx-name 0)
      (gensym ctx))))

;; @syntax (assoc? <expr>)
;; @param <expr> expression to be tested as an association list
;; <p>Predicates that <expr> is an association list with a structure of
;; '((key val) (key val) ...).  To evaluate true key may have only one
;; value, and keys must be symbols or strings.  Only the first level
;; is tested for associativity.</p>
;; @example
;; (assoc? '(1 2 3 4))
;; => nil
;; (assoc? '((a 1) (b 2) (c 3)))
;; => true
;; (assoc? '((a 1) (b 2) (c (1 2 3 4))))
;; => true
(define (assoc? lst)
  (when (list? lst)
    (for-all
      (lambda (elt)
        (and (= 2 (length elt))
             (atom? (first elt))))
      lst)))

(global 'assoc?)

;; @syntax (get-assoc <expr>)
;; @param <expr> association indexing of (<assoc-list> <key-1> [<key-2> ...])
;; <p>Extracts the value of the association expression.  Expressions are in the same
;; format as with the 'assoc' function, but the result is the same as the 'lookup'
;; function, except the multiple values are returned correctly.</p>
;; @example
;; (set 'data '((name "Joe") (friends "John" "Steve")))
;; (get-assoc (data 'name))
;; => "Joe"
;; (get-assoc (data 'friends))
;; => '("John" "Steve")
(define (get-assoc expr , found)
  (setf found (apply assoc expr))
  (when found
    (if (= 2 (length found))
      (last found)
      (rest found))))

(global 'get-assoc)

;; @syntax (fmap <sym-fun> <inst> <lst*>)
;; @param <sym-fun> quoted symbol naming a context function
;; @param <inst> a FOOP instance
;; @param <lst*> one or more lists
;; <p>FOOP methods cannot be easily mapped, since 'map' would require that the function
;; be passed as 'context:function', curried for the intended FOOP instance.  However,
;; currying truncates a function's lambda list to two parameters, the first being the
;; second argument to curry.</p>
;; <p>'fmap' solves this, although not extremely efficiently, with a lambda that wraps
;; the context function.</p>
;; @example
;; (define (Foo:Foo) (list (context)))
;; (define (Foo:make-list inst a b) (list a b)) ; pairs two elements
;; 
;; (let ((a '(1 2 3)) (b '(4 5 6)) (inst (Foo)))
;;   (fmap 'Foo:make-list inst a b))
;; 
;; => ((1 4) (2 5) (3 6))
(define (fmap fun inst)
  (eval (append
          (list 'map (fn () (apply fun (cons inst (args)))))
          (map 'quote (args)))))

(global 'fmap)

;; @syntax (keys <context-dict>)
;; @param <context-dict> context dictionary
;; <p>Returns a list of keys in the dictionary <context-dict>.</p>
;; @example
;; (define dict:dict)
;; (dict "x" 10)
;; (dict "y" 20)
;; (keys dict)
;; => '("x" "y")
(define (keys ctx)
  (map (fn (key) (trim key "_" ""))
       (filter (fn (key) (starts-with key "_"))
               (map name (symbols ctx)))))

(global 'keys)

;; @syntax (dokeys (<var> <dict>) <body>)
;; @param <var> variable to which the key name will be bound
;; @param <dict> dictionary from which the keys will be extracted
;; @param <body> the body forms to be executed with <var> bound to <dict>'s keys
;; <p>Evaluates <body> in a local block in which <var> is sequentially bound to each
;; of dict's keys.  Note that there is no special ordering of the keys.</p>
;; @example
;; (define dict:dict)
;; (dict "x" 10)
;; (dict "y" 20)
;; (dokeys (key dict)
;;   (println key ": " (dict key)))
;; => x: 10
;; => y: 20
(define-macro (dokeys)
  (letex ((var (args 0 0)) (ctx (args 0 1)) (body (cons 'begin (rest (args)))))
    (dolist (key (keys ctx))
      (setf var key)
      body)))

(global 'dokeys)

;; @syntax (make-array <int-size> <fn-init>)
;; @param <int-size> the size of the new array
;; @param <pass-index> when true (nil by default), passes the position index to <fn-init>
;; <p>Generates a new  one-dimensional array of size <int-size> and initializes
;; each array index by repeatedly calling <fn-init>. The current index is
;; available in $idx.</p>
;; @example
;; (setf arr (make-array 4 (gensym)))
;; => '(gensym-1 gensym-2 gensym-3 gensym-4)
(define (make-array size fun , arr i)
  (setf arr (array size) i -1)
  (until (= (inc i) size)
    (setf (arr i) (fun)))
  arr)

(global 'make-array)

;; @syntax (array-iter <fn-func> <array-arr>)
;; @param <fn-func> a function to call on each index of the array
;; @param <array-arr> an array
;; <p>Calls <fn-func> on each index of <array-arry>. Returns the value of the
;; last iteration. The current index is available in $idx.</p>
;; @example
;; (setf arr (array 4 (sequence 0 4))) ; => (0 1 2 3)
;; (array-iter (fn (i) (println (+ i $idx))) arr)
;; 0
;; 2
;; 4
;; 6
(define (array-iter fun arr , size i)
  (setf i -1 size (length arr))
  (until (= (inc i) size)
    (fun (arr i))))

(global 'array-iter)

;; @syntax (array-map <fn-func> <array-arr>) !
;; @param <fn-func> a function to call on each index of the array
;; @param <array-arr> an array
;; <p>Similar to the built-in function map, array-map applies <fn-func> to each
;; index of <array-arr>. array-map modifies <array-arr> in place.</p>
;; @example
;; (setf arr (array 10 (sequence 0 10))) ; => (0 1 2 3 4 5 6 7 8 9)
;; (array-map (fn (i) (+ i $idx)))
;; (println arr) ; => '(0 2 4 6 8 10 12 14 16 18)

(define-macro (array-map)
  (letex ((i (gensym)) (size (gensym)) (fun (eval (args 0)) (arr (args 1))))
    (setf i -1 size (length arr))
    (until (= (inc i) size)
      (setf (arr i) (fun $it)))))

(global 'array-map)

;; @syntax (with-open-device <descriptor> [<body-expressions>])
;; @param <descriptor> an open file descriptor
;; @param <body-expressions> any number of expressions
;; <p>Evaluates <body-expressions> with <descriptor> as the default device.
;; Catches errors during evaluation and closes <descriptor> once complete,
;; restoring the previous default device.</p>
;; @example
;; ; read one line from file and close
;; (with-open-device (open "somefile.txt")
;;   (println (read-line)))
(define-macro (with-open-device)
  (let ((old-dev (device)) (dev (eval (args 0))) (return) (result))
    (device dev)
    (setf return (catch (eval (cons begin (rest (args)))) 'result))
    (close (device))
    (device old-dev)
    (if return result (throw-error result))))

(global 'with-open-device)

;; @syntax (partial <func> <expr>)
;; @param <func> a function to be partially applied
;; @param <expr> an expression to replace the first argument of <func>
;; <p>Returns a new function that has been partially applied to <expr>. Unlike
;; curry, partial evaluates its arguments and does no damage to the parameter
;; list of the resulting function, which continues to accept the rest of the
;; parameters it would typically accept. This is particularly useful to fudge
;; closures over FOOP methods by partially applying them to their instances.
;; Note that macros and functions that do not evaluate their arguments may not
;; be partially applied, due to the use of the apply function in this
;; implementation.</p>
;; @example
;; (define (foo a b c)
;;   (join (list "foo" a b c) "|"))
;; (setf foobar (partial foo "bar"))
;; (foobar "baz" "bat") ; => "foo|bar|baz|bat"
(define (partial func arg)
  (letex ((func func) (arg arg))
    (lambda ()
      (apply func (cons 'arg (args))))))

(global 'partial)
