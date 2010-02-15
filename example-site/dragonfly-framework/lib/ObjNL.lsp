;; @module ObjNL.lsp
;; @description Objective newLISP - Real Object Oriented Programming for newLISP
;; @version 1.0
;; @author Greg Slepak
;; @location http://www.taoeffect.com/newlisp/ObjNL.lsp.txt
;; <h3>Introductory Guide</h3>
;; The @link http://www.taoeffect.com/blog/2009/12/introducing-objective-newlisp/ official&nbsp;guide
;; is highly recommended reading if you are planning on using Objective newLISP.
;; <h3>What is Objective newLISP?</h3>
;; Objective newLISP is a new and exciting way of doing <b>real</b>
;; object oriented programming in newLISP where instances are passed
;; by reference and can easily hold references to other objects while
;; maintaining their own mutable state.
;; <p>It supports most of the object oriented concepts you'll find
;; in other languages. It supports inheritance, interfaces (aka protocols),
;; as well as class and instance variables.</p>
;; <p>Objects are passed <i>by reference</i>, so there's no problem with passing
;; an object through multiple user-defined functions and modifying it.</p>
;; <p>Accessing instance variables no longer requires a function call plus a
;; list traversal. Simply access the symbol directly.</p>
;; <p>Objective newLISP also enhances newLISP by providing convenient and safe
;; macros for deep reference access.</p>
;; <p>With Objective newLISP it is possible to take full advantage of everything
;; object-oriented programming has to offer.</p>
;; <h3>Conventions</h3>
;; There are very few conventions in ObjNL, but there are some:
;; <ul>
;; <li>Classes should be written in camel-case and begin with a capital letter.</li>
;; <li>ObjNL reserves the @ character for itself to prefix symbols of special meaning.
;; You should avoid prefixing your symbols with it if possible.</li>
;; </ul>
;; <h3>Requirements</h3>
;; newLISP 10.1.9 or higher is <b>strongly recommended</b>, but any version after 10.1 should work.
;; <h3>Version history</h3>
;; <b>1.0</b> &bull; initial release

;; @syntax ObjNL
;; <p>'ObjNL' is the root class for Objective newLISP. All other classes ultimately
;; inherit from it. It defines several instance and class variables:</p>
;; <ul>
;; <li>'@self' to refer to the current object (be it an instance or class).</li>
;; <li>'@self-sym' is the symbol that represents the '@self' context.</li>
;; <li>'@class' is the context representing the class this object belongs to.</li>
;; <li>'@super' refers to the super-class of this object.</li>
;; <li>'@interfaces' a list of interfaces that this object conforms to.</li>
;; <li>'@rc' an integer representing the retain count of this object.</li>
;; </ul>
(set 'ObjNL:@super nil                 ; ObjNL has no super class
     'ObjNL:@self ObjNL                ; similar to ObjC, can be instance or class
     'ObjNL:@self-sym 'ObjNL           ; symbol referencing name of this context
     'ObjNL:@class ObjNL               ; always refers to class, never instance
     'ObjNL:@interfaces (list ObjNL)   ; ObjNL implements ObjNL
     'ObjNL:@rc 1                      ; the object's retain (or 'reference') count
)

(context ObjNL)
;; @syntax (ObjNL:ObjNL)
;; <p>The constructor is the default function. It is called by 'instantiate'.</p>
;; <p>The default implementation simply returns 'true'.</p>
(define (ObjNL:ObjNL) true)

;; @syntax (ObjNL:dealloc)
;; <p>Called by 'deallocate' to give the object an opportunity to release resources and objects.</p>
(define (ObjNL:dealloc))

;; @syntax (ObjNL:equals <ctx-obj>)
;; <p>Provides a method for classes to define what it means for objects to be equal.</p>
;; <p>The default implementation returns 'true' if two objects are the same instance.</p>
(define (ObjNL:equals obj) (= obj @self))
(context MAIN)

; it's possible to even implement reference counting :-p

;; @syntax (new-class <sym-class> [<ctx-super> [<list-interfaces>]])
;; @param <sym-class> The name of the class
;; @param <ctx-super> The superclass, accessible through '@super'
;; @param <list-interfaces> Any contexts to "mixin", accessible through '@interfaces'
;; @return The context of the new class created.
(define (new-class sym-class (super ObjNL) (interfaces '()) , class)
	(set 'class            (new super sym-class)
	     'class:@super     super
	     'class:@class     class
	     'class:@self      class
	     'class:@self-sym  sym-class
	)
	; NOTE: newLISP Bug? Why does pushing to the back result in odd behavior?
	; (push class class:@interfaces -1)
	(push class class:@interfaces)
	(dolist (iface interfaces)
		(setf iface (eval iface))
		(new  iface sym-class)
		(push iface class:@interfaces)
	)
	class
)

;; @syntax (instantiate <ctx-class> [<arg-1> ...])
;; <p>Returns a new instance of <ctx-class> by calling its
;; constructor and passing in any arguments. If the constructor
;; returns nil then the instance is deallocated and nil is returned.</p>
;; <p>The returned object <b>must</b> be deallocated using the 'deallocate'
;; function.</p>
(define (instantiate class)
	(letn (	obj-sym	(sym (string class "#" (++ class:@instance-counter)))
			obj		(new class obj-sym)
		)
		; set these prior to calling the constructor
		(set 'obj:@self obj 'obj:@self-sym obj-sym)
		(if (apply obj $args)
			obj
			(begin (deallocate obj) nil)
		)
	)
)
;; @syntax (add-interface <ctx-iface> <ctx-obj>)
;; <p>Uses the function 'new' to add <ctx-iface> to the object and
;; adds the interface to <ctx-obj>s '@interfaces'.</p>
(define (add-interface iface obj)
	(new iface obj:@self-sym)
	(push iface obj:@interfaces)
)
;; @syntax (deallocate <ctx-obj>)
;; <p>Calls the objects 'dealloc' method and then 'delete'&apos;s the object.</p>
;; <p><b>NOTE:</b> On versions of newLISP prior to 10.1.9 this is a fairly slow
;; operation, make sure to use at least version 10.1.9 with Objective newLISP.</p>
(define (deallocate obj)
	(obj:dealloc)
	(let (obj-sym obj:@self-sym)
		(delete obj-sym nil) ; delete the context
		(delete obj-sym nil) ; delete the symbol in MAIN
	)
)

;; @syntax (implements? <ctx-interface> <ctx-obj>)
;; @return true or nil as to whether this <ctx-obj> implements <ctx-interface>.
(define (implements? iface obj)
	(not (nil? (find iface obj:@interfaces)))
)

;; @syntax (retain <ctx-obj>)
;; <p>Increment's <ctx-obj>&apos;s retain count and returns the object.</p>
(define (retain obj)
	(++ obj:@rc)
	obj
)

;; @syntax (release <ctx-obj>)
;; <p>Decrement's <ctx-obj>&apos;s retain count. Deallocates the object if the retain count hits zero.</p>
(define (release obj)
	(when (zero? (-- obj:@rc))
		(deallocate obj)
	)
)

;; @syntax (autorelease <ctx-obj>)
;; <p>Adds <ctx-obj> to the current 'MAIN:@autorelease' pool and returns the object.</p>
(define (autorelease obj)
	(push obj (first @autorelease))
	obj
)

;; @syntax (push-autorelease-pool)
;; <p>Pushes a new autorelease pool onto the 'MAIN:@autorelease' stack.</p>
(define (push-autorelease-pool)
	(push '() @autorelease)
)

;; @syntax (pop-autorelease-pool)
;; <p>Pops the current 'MAIN:@autorelease' pool and releases the objects in it.</p>
(define (pop-autorelease-pool , obj)
	(dolist (obj (pop @autorelease))
		(release obj)
	)
)

(global 'new-class 'instantiate 'deallocate 'implements?
	'retain 'release 'autorelease 'push-autorelease-pool 'pop-autorelease-pool '@autorelease)

;; @syntax (. <obj> <field-1> [<field-2> [<field-n>]])
;; <p>The dot macro is used for "deep value access":</p>
;; <b>example:</b>
;; <pre>
;; (new-class 'Foo)
;; (new-class 'Bar)
;; (context Bar)
;; (define (Bar:Bar f)
;; 	(setf foo f)
;; 	true ; -> do not deallocate us if 'f' is nil
;; )
;; (context Foo)
;; (define (Foo:Foo b)
;; 	(setf bar b)
;; 	true ; -> do not deallocate us if 'b' is nil
;; )
;; (context MAIN)
;; (setf f (instantiate Foo (instantiate Bar)))
;; (set (.& f bar foo) f) ; => Foo#1
;; (. f bar foo bar)      ; => Bar#1</pre>
(context '.)
(define-macro (.:. obj)
	(doargs (field)
		(setf obj (eval (sym field (eval obj) nil)))
	)
)

;; @syntax (.& <obj> <field-1> [<field-2> [<field-n>]])
;; <p>The dot-reference macro is similar to the dot macro, except it returns the
;; context-qualified symbol for the final field instead of its value ("deep symbol access").
;; This allows you to combine it with 'set'.</p>
;; @see '.' macro for example usage.
(context '.&)
(define-macro (.&:.& obj)
	(doargs (field)
		(setf obj (sym field (eval obj)))
	)
)

(context MAIN)
