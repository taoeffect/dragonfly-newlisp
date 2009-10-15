(context 'Dragonfly)

; location of newlisp shared directory (for module support)
(constant 'newlisp-dir "/usr/share/newlisp")

; location of RESTful resources (relative to DOCUMENT_ROOT)
(constant 'restful-dir "views")


; setting a defaultview
(constant 'defaultview "dragonfly_welcome")
; setting a defaultaction
(constant 'defaultaction "index") ; display all
; setting a default rss view
(constant 'defaultrss "dragonfly_rssfeed")

; setting a 404 view
(constant 'default404 "404")


(context Dragonfly)
