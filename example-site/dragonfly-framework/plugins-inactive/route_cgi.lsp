;; @author Greg Slepak

(new Route 'Route.CGI)
(context Route.CGI)

(constant 'CGI_EXTENSION ".nl")

(define (matches?)
	(setf path (1 DF_PAGE))
	(and (ends-with path CGI_EXTENSION) (file? path))
)
(define (run)
	(SET_DF_SELF path)
	(load path)
	(DF:send-and-exit)
)

; add the route to the end of the list of routes
(push (Route.CGI) DF:dragonfly-routes -1)

(context MAIN)
