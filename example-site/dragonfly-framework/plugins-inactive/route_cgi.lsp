;; @author Greg Slepak

(new Route 'Route.CGI)
(context Route.CGI)

(constant 'CGI_EXTENSION ".nl")

(define (matches?)
	(set 'chunks (parse QUERY_STRING "?"))
	(and (not (empty? chunks))
		(ends-with (set 'path (first chunks)) CGI_EXTENSION)
		(file? path)
	)
)
(define (run)
	(SET_DFLY_SELF path)
	(load path)
	(DF:send-and-exit)
)

; add the route to the end of the list of routes
(push (Route.CGI) DF:dragonfly-routes -1)

(context MAIN)
