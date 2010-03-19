;; @author Greg Slepak

(new Resource 'Resource.Api)
(context 'Resource.Api)

(define (Resource.Api:Resource.Api)
	(catch-all)
)

(define (catch-all action)
	(setf api-path (string "dragonfly-api" (if action (string "/" action) "" )))
	(Response:content-type Response:html-type)
	(DF:display-view "dragonfly_api")
)

(context MAIN)
