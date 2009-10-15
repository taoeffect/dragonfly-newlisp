;; @module Http
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 1.1
;; @location http://static.artfulcode.net/newlisp/http.lsp
;; @package http://static.artfulcode.net/newlisp/http.qwerty
;; @description A bare-bones HTTP 1.0 library (updated for newlisp 10).
;; Http is an extremely bare-bones HTTP 1.0 library. Not all functionality
;; is implemented. In particular, the ability to parse an HTTP response is not
;; yet finished, but the ability to parse requests and send both requests and
;; responses is finished.
;; This module has not been rigorously tested. Your mileage may vary. Requires
;; newlisp 10.
;; <h4>Version history</h4>
;; <b>1.1</b>
;; &bull; updated for newlisp 10
;; &bull; code clean-up
;; 
;; <b>1.0</b>
;; &bull; initial release

(context 'Http)

(constant 'request-init-re (regex-comp {^(GET|POST|HEAD|PUT) (.+?) HTTP/(1.\d)$}))
(constant 'request-header-re (regex-comp {^(.+?):\s+(.+?)$}))
(constant 'line-ending-re (regex-comp [text][\r\n]{2,4}[/text]))
(constant 'response-template "HTTP/1.0 %d OK\r\nConnection: close\r\nContent-Type: %s\r\nDate: %s\r\nContent-Length: %d%s\r\n\r\n%s")

(define (format-header pair)
  (format "%s: %s" (title-case (string (pair 0))) (string (pair 1))))

;; @syntax (Http:parse-request <str-request>)
;; @param <str-request> an HTTP request received
;; <p>Parses an HTTP request and returns an association list.</p>
;; @example
;; (parse-request
;;   (format-request "POST"
;;                   "/cgi-bin/post_comment.cgi"
;;                   '(("Host" "www.somesite.com"))
;;                   "name=Some+Person&comment=Hello+world!"))
;;
;; => (("method" "POST")
;;     ("path" "/cgi-bin/post_comment.cgi")
;;     ("http-version" "1.0")
;;     ("headers" (("host" "www.somesite.com")
;;                 ("content-length" "37") nil)) 
;;     ("content" ""))
(define (parse-request req , lines request headers)
  (when (and (string? req) (not (empty? req)))
    (setf lines (map trim (parse req line-ending-re 0x10000)))
    (setf headers '())
    (setf request
      (first (find-all request-init-re (first lines)
               (list (list "method" $1) (list "path" $2) (list "http-version" $3))
               0x10000)))
    (when request
      (dolist (line (slice lines 1 -1))
        (push (first (find-all request-header-re line (list (lower-case $1) $2) 0x10000))
          headers -1))
      (push (list "headers" headers) request -1)
      (push (list "content" (slice (last lines) 0)) request -1)
      request)))

;; @syntax (Http:format-response <str-response> [<int-code> [<str-content-type> [<assoc-headers>]]])
;; @param <str-response> the text of the HTTP response
;; @param <int-code> the HTTP response code; default is 200 (success)
;; @param <str-content-type> MIME type of response; default is "text/html"
;; @param <assoc-headers> association list of headers to add to response
;; <p>Formats an HTTP/1.0 response.</p>
;; @example
;; (format-response binary-file-content 200 "audio/mp3")
;; => "HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: audio/mp3\r\nDate: Tue, 08 Jul 2008 10:30:09 EDT\r\nContent-Length: 17\r\n\r\n11000101010101..."
(define (format-response response (code 200) (content-type "text/html") (extra-headers '()))
  (format response-template
          code
          content-type
		      (date (date-value) 0 "%a, %d %b %Y %H:%M:%S %Z")
		      (length response)
		      (if-not (empty? extra-headers)
				    (string "\r\n" (join (map format-header extra-headers) "\r\n"))
				    "")
		      response))

;; @syntax (Http:format-request <str-method> [<str-path> [<assoc-headers> [<str-content>]]])
;; @param <str-method> request method (GET, POST, HEAD, or PUT)
;; @param <str-path> request path; default is "/"
;; @param <assoc-headers> association list of headers to add to request
;; @param <str-content> for POST and PUT methods, string containing request content
;; <p>Formats an appropriate HTTP/1.0 request. Note that the "Host" header must be added explicitly if required.</p>
;; @example
;; (format-request "POST"
;;                 "/cgi-bin/post_comment.cgi"
;;                 '(("Host" "www.somesite.com"))
;;                 "name=Some+Person&comment=Hello+world!"))
;; => "HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: text/html\r\nDate: Tue, 08 Jul 2008 10:28:03 EDT\r\nContent-Length: 46\r\n\r\n<html><body><h1>Hello world</h2></body></html>"
(define (format-request method (path "/") (headers '()) content, (buf ""))
  (if-not (and (string? method) (find (upper-case method) '("GET" "POST" "HEAD" "PUT")))
	  (throw-error "Invalid or unimplemented HTTP method"))
  (setf method (upper-case method))
  (write-buffer buf (format "%s %s HTTP/1.0\r\n" method (string path)))
  (dolist (header headers)
    (write-buffer buf (format "%s\r\n" (format-header header))))
  (when content
		(write-buffer buf (format "Content-Length: %d\r\n\r\n" (length content)))
		(write-buffer buf content))
  (write-buffer buf "\r\n\r\n")
  buf)

(context MAIN)