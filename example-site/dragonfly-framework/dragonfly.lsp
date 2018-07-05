;; @module dragonfly.lsp
;; @description The heart of Dragonfly - A newLISP web framework for rapid web development.
;; @version 0.74
;; @author Team Dragonfly 2009
;; @location http://code.google.com/p/dragonfly-newlisp/
;; <br>This file is the main entry-point of the Dragonfly framework and
;; contains several important functions, as well as the default route
;; definitions. The functions here are in the 'Dragonfly' context (alias 'DF'),
;; which is the context your static files will be evaluated in by default.
;; Therefore all of the functions here can be called in your templates without
;; needing to be context-qualified.
;; <br><br>Dragonfly's design is very simple, you can actually read through its
;; source in very little time to get a great understanding of exactly how
;; it works, and to get an idea of what sorts of tricks you can do to
;; customize it to your liking (remember, newLISP is <extremely> dynamic!).
;; <h3>The 'listener' function</h3>
;; The 'listener' function is called in 'index.cgi'. It is the function that
;; kicks everything off by looping through the available routes, finding a
;; match, running it, sending the output to the browser, and then exiting.
;; <br><br>Before all of that, the very *first* thing it does is load the
;; plugins in the 'dragonfly-framework/plugins-active' folder, giving them
;; an opportunity to do any special customization that they might require.
;; <h3>Environment Variables</h3>
;; At the very top of the 'config.lsp' file there is the following line:
;; <pre> (dolist (x (env)) (constant (global (sym (upper-case (first x)))) (last x)))</pre>
;; This line simply loops through every environment variable and makes a
;; global symbol out of it. This makes it extremely simple to access environment
;; variables, simply type their name! If you prefer PHP-style, you can
;; access them through the '$SERVER' function (simply a synonym for 'env').
;; <br><br>To access any web parameters, files, and cookies use the functions '$GET', '$POST',
;; '$FILES', and '$COOKIES', respectively. See 'Request.lsp' for more information.
;; <h3>Routes</h3>
;; <p>Routes are FOOP objects inheriting from the Route class. They should have the 'Route.' prefix.
;; Currently they only need to support two functions: 'matches?' and 'run'.</p>
;; <p>The listener loops through the available routes and calls 'matches?' on them
;; with no arguments. The route must decide, based on any data available to it,
;; whether or not it to return a non-nil value from 'matches?'.</p>
;; Here, for example, is the 'matches?' function for 'Route.Resource':
;; <pre> (define (matches?)
;;     (when (regex {^([a-z]\w+)(/([a-z]\w+))?(/(\d+))?(\.([a-z]+))?} QUERY_STRING 1)
;;         (set 'resource_name $1 'resource_action $3 'resource_id $5 'response_format $7)
;;         (file? (set 'path (DF:resource-path resource_name)))
;;     )
;; )
;; </pre>
;; There are two default routes: 'Route.Static' and 'Route.Resource'. See the
;; documentation on the example-site and in 'config.lsp' for more information on
;; what they do.
;; <h3>Resources</h3>
;; 'Route.Resource' handles URLs that refer to RESTful resources, represented as FOOP objects
;; deriving from the 'Resource' class. The resources reside in the 'RESOURCES_PATH' as .lsp files.
;; The URL scheme works in a similar manner to twitter's RESTful API:
;; <pre>http://mysite.com/<resource>[/<action>][/<id>][.<response_format>][?get paramters...]</pre>
;; 'resource' maps to a context name in a special way. First 'Resource.' is prepended
;; to the name, then the underscores are removed and the name is written in title case.
;; The 'resource' may only have the letters A-Z (lowercase or uppercase), 0-9, the underscore,
;; and it must begin with a letter.
;; <br/><pre> my_resource => Resource.MyResource</pre>
;; The name also maps to a real file located in 'RESOURCES_PATH' by appending ".lsp" to the name:
;; <br/><pre> my_resource => load file: RESOURCES_PATH/my_resource.lsp</pre>
;; If 'resource' implements 'action', then that function is called.
;; Like 'resource', 'action' may only contain letters, numbers, and the underscore.
;; If no 'action' is specified, then the resource's default function is called instead.
;; If it is specified but no method of that name is found, 'catch-all' will be called, which
;; by default calls 'Dragonfly:die'. You can override 'catch-all' and do dispatch within
;; it, as it takes the action name (as a string) as the first argument, followed by the
;; 'id' and the 'response_format'.
;; <p>The optional paramters 'id' and 'response_format' are passed in to the function
;; as parameters (in that order, unless it is the 'catch-all' function, in which case
;; 3 parameters are passed in as described above).</p>
;; <p>'id' may only contain numbers, and 'response_format' may only contain letters.</p>
;; <h3>Plugins</h3>
;; There are two types of plugins, those in the 'plugins-active' folder, and those
;; in the 'plugins-inactive' folder. The ones in the former are loaded when 'listener'
;; is called, prior to running the routes. Every .lsp file in the 'plugins-active' folder
;; is loaded at that point, so you'll only want your most frequently used files in there.
;; <p>A good example of an active plugin is a custom route. Defining a custom route consists
;; of two basic steps: creating your 'Route' "subclass", and adding an instance of
;; it to 'Dragonfly:dragonfly-routes'. Take a look at how it's done in the source of
;; 'dragonfly.lsp' for more info.</p>
;; <p>Inactive plugins are simply those that should be loaded on a "need to use" basis.
;; Most plugins will probably fall into this category. Use 'Dragonfly:activate-plugin'
;; to load them. All plugins are loaded exactly once, no matter how many times
;; 'activate-plugin' is called on them.</p>


;===============================================================================
; !Compatibility with older versions of newLISP
;===============================================================================

(when (< (sys-info -2) 10110)
  (constant (global '++) inc)
  (constant (global '--) dec)
  (constant (global 'extend) write-buffer)
)
(when (< (sys-info -2) 10111)
  (constant (global 'term) name)
  (constant (global 'read) read-buffer)
  (constant (global 'write) write-buffer)
)

;===============================================================================
; !Basic Setup, Global Vars, and Sanity Checks
;===============================================================================

; $SERVER is a synonym for env, for $GET, $POST, and $FILES see lib/request.lsp
(constant (global '$SERVER) env)

; DF is a convenient shorthand to the Dragonfly context
(constant (global 'DF) Dragonfly)
(constant (global 'DRAGONFLY_MAJOR) 0)
(constant (global 'DRAGONFLY_MINOR) 74)
(constant (global 'DRAGONFLY_VERSION) (format "Version %d.%d" DRAGONFLY_MAJOR DRAGONFLY_MINOR))

; make sure these two are defined
(if-not DOCUMENT_ROOT (throw-error "Environment variable DOCUMENT_ROOT missing!"))
(unless QUERY_STRING
  (constant (global 'QUERY_STRING) "")
  (env "QUERY_STRING" QUERY_STRING)
)

;; @syntax DF_PAGE
;; <p>The web-friendly, host-unqualified URL to the "current page"</p>
;; <pre> ; load http://www.mysite.com/foo/bar?baz
;; DF_PAGE ;=> "/foo/bar"
;; ; load http://www.mysite.com
;; DF_PAGE ;=> "/"</pre>
(if (empty? QUERY_STRING)
  (constant (global 'DF_PAGE) "/")
  (constant (global 'DF_PAGE) (string "/" ((parse QUERY_STRING {[?&]} 0) 0)))
)

;; @syntax DF_SELF
;; <p>The full, local path (on the server) to the currently loaded file
;; or view being displayed.</p>
;; <b>example:</b>
;; <pre> ; load http://www.mysite.com/foo/bar?baz
;; DF_SELF ;=> "/home/www/mysite.com/foo/bar.html"</pre>
;; <b>see:</b> the 'SET_DF_SELF' function in utils.lsp for more info
(global 'DF_SELF)

;; @syntax DF_SELF_DIR
;; <p>The full, local path (on the server) to the directory holding
;; the currently loaded file or view being displayed.</p>
;; <b>example:</b>
;; <pre> ; load http://www.mysite.com/foo/bar?baz
;; DF_SELF_DIR ;=> "/home/www/mysite.com/foo"</pre>
;; <b>see:</b> the 'SET_DF_SELF' function in utils.lsp for more info
(global 'DF_SELF_DIR)

; seed the random number generator immediately.
(seed (time-of-day))

(context 'Dragonfly)

;===============================================================================
; !Public Constants and Variables
;===============================================================================

;; @syntax STDOUT
;; This is the buffer that contains the content that will get written
;; to STDOUT if no errors are thrown. 'MAIN:print' and 'MAIN:println'
;; are globally overridden to write to this buffer.
;; Normally you should never need to modify this variable, however it is
;; documented for reference's sake.
(define STDOUT "")

; you can customize this variable with your own routes, note
; that you might need to clear the default routes out of it (added below)
(define dragonfly-routes '())

;===============================================================================
; !Load Libraries and Plugins
;===============================================================================

; load utils.lsp before loading anything else
(load (string DRAGONFLY_ROOT "/lib/utils.lsp"))
; load all our essential stuff
(load-files-in-dir (string DRAGONFLY_ROOT "/lib") "\.lsp$")
; plugins are loaded when listener is called so that they
; can modify the variables in this file if they want.
; you can also load the inactive plugins on a need-to-load basis
; by using the 'activate-plugin' function.

;===============================================================================
; !Public Functions
;===============================================================================

;; @syntax (DF:activate-plugin <str-plugin-name> [<str-plugin-name-2> ...])
;; @param <str-plugin-name> The name of the plugin to load, without the ".lsp" extension.
;; <br>Loads (once only) a the named plugin from the 'plugins-inactive' folder.
;; <br>If <str-plugin-name> refers to a directory, then loads all of the ".lsp" files in that directory.
(define (activate-plugin)
  (doargs (plugin-name)
    (let (plugin-name (string DRAGONFLY_ROOT "/plugins-inactive/" plugin-name))
      (if (directory? plugin-name)
        (load-files-in-dir plugin-name "\.lsp$")
        (load-once (string plugin-name ".lsp"))
      )
    )
  )
)

;; @syntax (DF:web-root [<str-path> [<bool-question-mark>]])
;; @param <str-path> Path relative to the folder containing 'index.cgi'.
;; @param <bool-question-mark> Whether to return a URL with /? prepended.
;; <p>This function is quite handy for making working links when your 'index.cgi' file
;; is not in 'DOCUMENT_ROOT' but a subfolder of it.</p>
;; @example
;; ; index.cgi is located in /home/user/site.com/examples-site
;; ; Users visit http://www.site.com/example-site
;; (web-root "about") => "/example-site/about"
;; (web-root "/foo" true) => "/example-site/?foo"
;; (web-root) => /example-site/
(define (web-root (path "") question-mark)
  ; WEB_ROOT should have a "/" on the end
  (if (starts-with path "/") (pop path))
  (string WEB_ROOT (if question-mark "?" "") path)
)

;; @syntax (DF:view-path <str-view-name>)
;; @param <str-view-name> Name of view in 'VIEWS_PATH', without any extension.
;; <br>Returns the absolute path to the view as a string, appending 'VIEW_EXTENSION' if necessary.
(define (view-path view-name)
  (string VIEWS_PATH "/" view-name (if VIEW_EXTENSION VIEW_EXTENSION ""))
)

;; @syntax (DF:partial-path <str-partial-name>)
;; <br>Just like 'view-path', except for partials in 'PARTIALS_PATH'.
(define (partial-path partial-name)
  (string PARTIALS_PATH "/" partial-name (if VIEW_EXTENSION VIEW_EXTENSION ""))
)

;; @syntax (DF:resource-path <str-resource-name>)
;; <br>Similar to 'view-path', except for resources in 'RESOURCES_PATH'.
;; Don't include the .lsp extension.
(define (resource-path resource-name)
  (string RESOURCES_PATH "/" resource-name ".lsp")
)

;; @syntax (DF:include)
;; <br>Like 'display-file' but does not pass the file through 'eval-template'.
(define (include)
  (print (read-file (apply string $args)))
)

;; @syntax (DF:display-file)
;; <br>String-concats its arguments and displays the file
;; at that path after passing it through 'eval-template'.
(define (display-file)
  (let (path (apply string $args))
    (and (file? path true)
         (eval-template (read-file path))))
)

;; @syntax (DF:display-partial <str-partial-name>)
;; Displays the partial named <str-partial-name> using 'display-file' and 'partial-path'.
(define (display-partial partialname)
    (display-file (partial-path partialname))
)

;; @syntax (DF:display-view <str-view-name>)
;; Displays the view named <str-view-name> using 'display-file' and 'view-path'.
(define (display-view viewname)
  (display-file (view-path viewname))
)

;; @syntax (DF:display-error <int-error-code>)
;; <br>Sends the <int-error-code> and, if it exists, displays the view named
;; <int-error-code> using 'display-view'. Otherwise, displays the built-in error
;; template 'Dragonfly:ERROR_TEMPLATE'.
;;
;; If an error is thrown with 'throw-error', this is automatically called
;; with an <int-error-code> of 500 (Internal Server Error).
(define (display-error error-code (clear-stdout true))
  (Response:status error-code)
  (Response:content-type Response:html-type)
  (set 'STDOUT "")

  (unless (display-view (string error-code))
    (log-info "display-error using ERROR_TEMPLATE for error-code " error-code)
    (eval-template ERROR_TEMPLATE)
  )
)

;; @syntax (DF:eval-template <str> [<ctx>])
;; @param <str> A string containing the template.
;; @param <ctx> Optional. Represents the context the template is evaluted in. Defaults to Dragonfly.
;; <br>newLISP code in the template between the 'OPEN_TAG' and 'CLOSE_TAG' (see 'config.lsp') is
;; evaluated, and the result, along with the text outside of the "code islands" will be sent if no errors occur.
(define (eval-template str (ctx Dragonfly) , start end block (buf ""))
  (while (and (setf start (find OPEN_TAG str)) (setf end (find CLOSE_TAG str)))
    (extend buf (string "(print [text]" (slice str 0 start) "[/text]) "))
    (setf block (slice str (+ start 2) (- end start 2)))
    (if (starts-with block "=")
      (extend buf (string "(print " (rest block) ") "))
      (extend buf block)
    )
    (setf str (slice str (+ end 2)))
  )
  (when str
    (extend buf (string "(print [text]" str "[/text])"))
    (eval-string buf ctx)
  )
)

;; @syntax (DF:die)
;; <br>String-concats its arguments, logs them as an error via 'log-err', and calls
;; 'throw-error' with the same string.
;;
;; @see Dragonfly:display-error
(define (die)
  (let (msg (apply string $args))
    (log-err msg)
    (throw-error msg)
  )
)

; our main entry-point. this calls exit.
(define (listener)
  ; we load these here so that they can modify any of the variables in this file
  (load-files-in-dir (string DRAGONFLY_ROOT "/plugins-active") "\.lsp$")
  ; go through all the routes, if one matches, run it and we're done!
  (dolist (route dragonfly-routes)
    (when (:matches? route)
      (:run route)
      (send-and-exit)
    )
  )
  (log-info "no route matched for QUERY_STRING: " QUERY_STRING)
  (display-error 404)
  (send-and-exit)
)

;===============================================================================
; !Setup Default Routes
;===============================================================================

; newLISP can't handle calling 'new' outside of MAIN context, nor does it currently
; allow switching contexts in a function call. If it does one day, route defintions
; will be specified through a 'define-route' macro.
(context MAIN)
(new Route 'Route.Static)
(new Route 'Route.Resource)

(context 'Route.Static)

(define (matches?)
  (set 'chunks (parse QUERY_STRING {[?&]} 0))
  (if (empty? chunks)
    (push DF:DEFAULT_VIEW chunks))
  (set 'path (set 'DF:_ (first chunks)))

  (if (set 'ext (exists (curry ends-with path) DF:STATIC_TRIGGER_EXTENSIONS))
    ; if the path ends with one of the trigger extensions, match if it exists
    (file? path)
    ; otherwise, check if one of the transformations exists
    (exists (fn (x) (file? (setf path (eval x)))) DF:STATIC_TRANSFORMATIONS)
  )
)
(define (run)
  (replace {\.\.[/|\\]} path "" 0) ; we don't want them getting at things they shouldn't
  (SET_DF_SELF path)
  (unless ext (setf ext (regex-captcha {.*\.(\w+)$} path)))
  (when ext (Response:content-type (Response:extension->type ext)))
  (unless (DF:display-file path)
    (DF:die "Failed to get: " path)
  )
)

(context 'Route.Resource)

(define (matches?)
  (when (regex {^([a-z]\w+)(/([a-z]\w+))?(/(\d+))?(\.([a-z]+))?} QUERY_STRING 1)
    (set 'resource_name $1 'resource_action $3 'resource_id $5 'response_format $7)
    (file? (set 'path (DF:resource-path resource_name)))
  )
)
(define (run)
  (SET_DF_SELF path)
  (load path)
  (letn (
      ctx-str (string "Resource." (join (map title-case (parse resource_name "_"))))
        ctx-sym (sym ctx-str)
    )
    ; If no action is specified, use the default function
    (when (null? resource_action) (setf resource_action ctx-str))
    (setf action (eval (sym resource_action ctx-sym)))
    ; if the requested action doesn't exist we call the catch-all method
    (unless (lambda? action)
      (setf action (lambda () (eval (append (list (sym 'catch-all ctx-sym) resource_action) $args))))
    )
    ; call the action on the resource with the optional parameters
    (action (int resource_id) (if-not (null? response_format) response_format))
  )
)

(context 'Dragonfly)

(when ENABLE_STATIC_TEMPLATES (push (Route.Static) dragonfly-routes -1))
(when ENABLE_RESTFUL_HANDLER  (push (Route.Resource) dragonfly-routes -1))

;===============================================================================
; !Private Functions (i.e. you shouldn't ever call these)
;===============================================================================

(define (send-and-exit (code 0))
  (Response:send-headers)
  (sys-print STDOUT)
  (exit code)
)

; setup our error handler
(setf Dragonfly:error-event.called nil)
(error-event (fn ()
  (when Dragonfly:error-event.called
    (send-and-exit 1099))
  (setf Dragonfly:error-event.called true)
  ;(log-err "Got error (" (last (last-error)) ") with STDOUT contents:\n{" STDOUT "}")
  (log-err (last (last-error)))
  (display-error 500)
  (send-and-exit)
))

;===============================================================================
; !Private Variables
;===============================================================================

(set 'ERROR_TEMPLATE
[text]
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title><%= (join (map string (Response:status)) " ") %></title>
</head><body>
<h1><%= (last (Response:status)) %></h1>
<p>The requested URL <%= (web-root QUERY_STRING) %> resulted in error <%= (join (map string (Response:status)) " ") %>.</p>
<p>Additionally, a 404 Not Found
error was encountered while trying to use an ErrorDocument to handle the request.</p>
</body></html>
[/text]
)

(set 'WEB_ROOT (slice DOCUMENT_ROOT (length ORIGINAL_ROOT)))
(push "/" WEB_ROOT -1)

(context MAIN)
