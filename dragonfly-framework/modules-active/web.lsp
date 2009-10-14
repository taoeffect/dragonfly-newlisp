#!/usr/bin/newlisp
;; @module Web
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 0.3.1 beta
;; @location http://static.artfulcode.net/newlisp/web.lsp
;; @package http://static.artfulcode.net/newlisp/web.qwerty
;; @description A collection of functions for writing web-based software.
;; <b>Features:</b>
;; <ul>
;;   <li>ASP/PHP-style templates</li>
;;   <li>Cookies</li>
;;   <li>Entities translation</li>
;;   <li>GET/POST parameters</li>
;;   <li>HTTP header control</li>
;;   <li>Sessions</li>
;;   <li>URL building and parsing</li>
;;   <li>URL encoding and decoding</li>
;;   <li>query building and parsing</li>
;; </ul>
;; <b>Known issues</b>
;; <ul>
;;   <li>
;;     When used in conjunction with the official @link http://newlisp.nfshost.com/code/modules/cgi.lsp.html CGI
;;     module, @link http://newlisp.nfshost.com/code/modules/cgi.lsp.html CGI must be loaded first. In the case of
;;     identical GET and POST parameters, the value is stored in GET, but the value will be POST. This
;;     is due to the fact that CGI stores both GET and POST in the same association list and overwrites
;;     GET values with POST.
;;   </li>
;; </ul>
;; 
;; <b>Note:</b> for JSON encoding and decoding, see the @link http://static.artfulcode.net/newlisp/json.lsp.html Json module.
;; 
;; <h4>To do</h4>
;; &bull; add MIME decoding for multipart posts
;;
;; <h4>Version history</h4>
;; <b>0.3.1</b>
;; &bull; fixed ineffective usage of set/setf
;;
;; <b>0.3</b>
;; &bull; made parse-query more tolerant and fixed parsing bug
;; &bull; cookie now accepts an additional parameter that only permits access during HTTPS sessions
;; 
;; <b>0.2</b>
;; &bull; build-url now accepts query strings in addition to assoc lists
;; &bull; session-id now accepts an optional parameter to set the session id
;; &bull; fixed some typos with 'clean-sessions'
;; &bull; fixed extra parameter in 'define-session-handlers'
;; 
;; <b>0.1</b>
;; &bull; initial release
;; 
(context 'Web)

;===============================================================================
; !Constants and definitions
;===============================================================================

(constant 'POST_LIMIT 4096)

(define GET)
(define POST)
(define COOKIE)

(define SESSION_DIR "/tmp")
(define SESSION_MAX_AGE (* 60 60 24 7)) ; seconds
(define SESSION_KEY "NLWSESID")
(define SESSION_PREFIX "NLWSES")
(define SESSION_STARTED)
(define SESSION_ID) ; stores the current session id

;===============================================================================
; !Encoding and decoding
;===============================================================================

(define ENTITIES
  (list
    (list 34 {&quot;})       (list 38 {&amp;})        (list 39 {&apos;})       (list 60 {&lt;})
    (list 62 {&gt;})         (list 160 {&nbsp;})      (list 161 {&iexcl;})     (list 162 {&cent;})
    (list 163 {&pound;})     (list 164 {&curren;})    (list 165 {&yen;})       (list 166 {&brvbar;})
    (list 167 {&sect;})      (list 168 {&uml;})       (list 169 {&copy;})      (list 170 {&ordf;})
    (list 171 {&laquo;})     (list 172 {&not;})       (list 173 {&shy;})       (list 174 {&reg;})
    (list 175 {&macr;})      (list 176 {&deg;})       (list 177 {&plusmn;})    (list 178 {&sup2;})
    (list 179 {&sup3;})      (list 180 {&acute;})     (list 181 {&micro;})     (list 182 {&para;})
    (list 183 {&middot;})    (list 184 {&cedil;})     (list 185 {&sup1;})      (list 186 {&ordm;})
    (list 187 {&raquo;})     (list 188 {&frac14;})    (list 189 {&frac12;})    (list 190 {&frac34;})
    (list 191 {&iquest;})    (list 192 {&Agrave;})    (list 193 {&Aacute;})    (list 194 {&Acirc;})
    (list 195 {&Atilde;})    (list 196 {&Auml;})      (list 197 {&Aring;})     (list 198 {&AElig;}) 
    (list 199 {&Ccedil;})    (list 200 {&Egrave;})    (list 201 {&Eacute;})    (list 202 {&Ecirc;})
    (list 203 {&Euml;})      (list 204 {&Igrave;})    (list 205 {&Iacute;})    (list 206 {&Icirc;})
    (list 207 {&Iuml;})      (list 208 {&ETH;})       (list 209 {&Ntilde;})    (list 210 {&Ograve;})
    (list 211 {&Oacute;})    (list 212 {&Ocirc;})     (list 213 {&Otilde;})    (list 214 {&Ouml;})
    (list 215 {&times;})     (list 216 {&Oslash;})    (list 217 {&Ugrave;})    (list 218 {&Uacute;})
    (list 219 {&Ucirc;})     (list 220 {&Uuml;})      (list 221 {&Yacute;})    (list 222 {&THORN;})
    (list 223 {&szlig;})     (list 224 {&agrave;})    (list 225 {&aacute;})    (list 226 {&acirc;})
    (list 227 {&atilde;})    (list 228 {&auml;})      (list 229 {&aring;})     (list 230 {&aelig;})
    (list 231 {&ccedil;})    (list 232 {&egrave;})    (list 233 {&eacute;})    (list 234 {&ecirc;})
    (list 235 {&euml;})      (list 236 {&igrave;})    (list 237 {&iacute;})    (list 238 {&icirc;})
    (list 239 {&iuml;})      (list 240 {&eth;})       (list 241 {&ntilde;})    (list 242 {&ograve;})
    (list 243 {&oacute;})    (list 244 {&ocirc;})     (list 245 {&otilde;})    (list 246 {&ouml;})
    (list 247 {&divide;})    (list 248 {&oslash;})    (list 249 {&ugrave;})    (list 250 {&uacute;})
    (list 251 {&ucirc;})     (list 252 {&uuml;})      (list 253 {&yacute;})    (list 254 {&thorn;})
    (list 255 {&yuml;})      (list 338 {&OElig;})     (list 339 {&oelig;})     (list 352 {&Scaron;})
    (list 353 {&scaron;})    (list 376 {&Yuml;})      (list 402 {&fnof;})      (list 710 {&circ;})
    (list 732 {&tilde;})     (list 913 {&Alpha;})     (list 914 {&Beta;})      (list 915 {&Gamma;})
    (list 916 {&Delta;})     (list 917 {&Epsilon;})   (list 918 {&Zeta;})      (list 919 {&Eta;})
    (list 920 {&Theta;})     (list 921 {&Iota;})      (list 922 {&Kappa;})     (list 923 {&Lambda;})
    (list 924 {&Mu;})        (list 925 {&Nu;})        (list 926 {&Xi;})        (list 927 {&Omicron;})
    (list 928 {&Pi;})        (list 929 {&Rho;})       (list 931 {&Sigma;})     (list 932 {&Tau;})
    (list 933 {&Upsilon;})   (list 934 {&Phi;})       (list 935 {&Chi;})       (list 936 {&Psi;})
    (list 937 {&Omega;})     (list 945 {&alpha;})     (list 946 {&beta;})      (list 947 {&gamma;})
    (list 948 {&delta;})     (list 949 {&epsilon;})   (list 950 {&zeta;})      (list 951 {&eta;})
    (list 952 {&theta;})     (list 953 {&iota;})      (list 954 {&kappa;})     (list 955 {&lambda;})
    (list 956 {&mu;})        (list 957 {&nu;})        (list 958 {&xi;})        (list 959 {&omicron;})
    (list 960 {&pi;})        (list 961 {&rho;})       (list 962 {&sigmaf;})    (list 963 {&sigma;})
    (list 964 {&tau;})       (list 965 {&upsilon;})   (list 966 {&phi;})       (list 967 {&chi;})
    (list 968 {&psi;})       (list 969 {&omega;})     (list 977 {&thetasym;})  (list 978 {&upsih;})
    (list 982 {&piv;})       (list 8194 {&ensp;})     (list 8195 {&emsp;})     (list 8201 {&thinsp;})
    (list 8204 {&zwnj;})     (list 8204 {&zwj;})      (list 8204 {&lrm;})      (list 8204 {&rlm;})
    (list 8211 {&ndash;})    (list 8212 {&mdash;})    (list 8216 {&lsquo;})    (list 8217 {&rsquo;})
    (list 8218 {&sbquo;})    (list 8220 {&ldquo;})    (list 8221 {&rdquo;})    (list 8222 {&bdquo;})
    (list 8224 {&dagger;})   (list 8225 {&Dagger;})   (list 8226 {&bull;})     (list 8230 {&hellip;})
    (list 8240 {&permil;})   (list 8242 {&prime;})    (list 8243 {&Prime;})    (list 8249 {&lsaquo;})
    (list 8250 {&rsaquo;})   (list 8254 {&oline;})    (list 8260 {&frasl;})    (list 8364 {&euro;})
    (list 8465 {&image;})    (list 8472 {&weierp;})   (list 8476 {&real;})     (list 8482 {&trade;})
    (list 8501 {&alefsym;})  (list 8592 {&larr;})     (list 8593 {&uarr;})     (list 8594 {&rarr;})
    (list 8595 {&darr;})     (list 8596 {&harr;})     (list 8629 {&crarr;})    (list 8656 {&lArr;})
    (list 8657 {&uArr;})     (list 8658 {&rArr;})     (list 8659 {&dArr;})     (list 8660 {&hArr;})
    (list 8704 {&forall;})   (list 8706 {&part;})     (list 8707 {&exist;})    (list 8709 {&empty;})
    (list 8711 {&nabla;})    (list 8712 {&isin;})     (list 8713 {&notin;})    (list 8715 {&ni;})
    (list 8719 {&prod;})     (list 8721 {&sum;})      (list 8722 {&minus;})    (list 8727 {&lowast;})
    (list 8730 {&radic;})    (list 8733 {&prop;})     (list 8734 {&infin;})    (list 8736 {&ang;})
    (list 8743 {&and;})      (list 8744 {&or;})       (list 8745 {&cap;})      (list 8746 {&cup;})
    (list 8747 {&int;})      (list 8756 {&there4;})   (list 8764 {&sim;})      (list 8773 {&cong;})
    (list 8776 {&asymp;})    (list 8800 {&ne;})       (list 8801 {&equiv;})    (list 8804 {&le;})
    (list 8805 {&ge;})       (list 8834 {&sub;})      (list 8835 {&sup;})      (list 8836 {&nsub;})
    (list 8838 {&sube;})     (list 8839 {&supe;})     (list 8853 {&oplus;})    (list 8855 {&otimes;})
    (list 8869 {&perp;})     (list 8901 {&sdot;})     (list 8968 {&lceil;})    (list 8969 {&rceil;})
    (list 8970 {&lfloor;})   (list 8971 {&rfloor;})   (list 9001 {&lang;})     (list 9002 {&rang;})
    (list 9674 {&loz;})      (list 9824 {&spades;})   (list 9827 {&clubs;})    (list 9829 {&hearts;})
    (list 9830 {&diams;})))

(define UNENTITIES
  (map reverse ENTITIES))

(define JS_ESCAPE_CHARS
  (list
    (list {\} {\\})
    (list {"} {\"})
    (list {'} {\'})
    (list "\n" {\n})
    (list "\r" {\r})
    (list "</" {<\/})))

;; @syntax (Web:escape-js <str>)
;; @param <str> a string to escape
;; <p>Escapes a string for output in javascript. Does not encode entities;
;; just prevents control characters from causing syntax errors in javascript.</p>
(define (escape-js str)
  (dolist (ch JS_ESCAPE_CHARS)
    (replace (first ch) str (last ch)))
  str)

;; @syntax (Web:escape <str>)
;; @param <str> a string to escape
;; @return the escaped string
;; <p>Escapes characters that are part of the (X)HTML and XML syntax to prevent
;; characters from confusing browsers' parsing of markup. Escapes single and
;; double quotes, ampersands, and left and right angle brackets
;; ('&quot;', '&apos;', '&amp;', '&lt;', and '&gt;').</p>
(define (escape str)
  (replace {"} str {&quot;})
  (replace {'} str {&apos;})
  (replace {&} str {&amp;})
  (replace {<} str {&lt;})
  (replace {>} str {&gt;})
  str)

;; @syntax (Web:unescape <str>)
;; @param <str> an entity-escaped string
;; @return the unescaped string
;; <p>Unescapes the basic (X)HTML and XML character entities in a string.</p>
(define (unescape str)
  (replace {&quot;} str {"})
  (replace {&apos;} str {'})
  (replace {&amp;} str {&})
  (replace {&lt;} str {<})
  (replace {&gt;} str {>})
  str)

;; @syntax (Web:encode-entities <str>)
;; @param <str> a string to escape
;; @return the escaped string
;; <p>Escapes characters with a much larger set of character entities than
;; 'escape' using a table derived from 
;; @link http://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references Wikipedia.
(define (encode-entities str , ent (buf ""))
  (dostring (c str)
    (write-buffer buf
      (if (setf ent (lookup c ENTITIES)) ent (char c))))
  buf)

;; @syntax (Web:decode-entities <str>)
;; @param <str> an entity-encoded string
;; @return the decoded string
;; <p>Translates character entities to their character equivalents as well as
;; numeric entities.</p>
(define (decode-entities str)
  (replace {&(\d+);} str (char (int $1)) 0)
  (replace {(&\S+?;)} str (char (lookup $1 UNENTITIES)) 0))

; Translates a single character into a hex-encoded string suitable for a URL.
(define (hex-encode-char ch)
  (if (= " " ch) "+" (format "%%%x" (char ch))))

; Translates a URL-encoded hex into a string character.
(define (hex-decode-char ch)
  (when (starts-with ch "%")
    (pop ch))
  (char (int (append "0x" $1))))

;; @syntax (Web:url-encode <str>)
;; @param <str> a string token to encode for use in a URL
;; @return the URL-encoded string
;; <p>Encodes a string for use in a URL.</p>
(constant 'REGEX_HTTP_SPECIAL_CHAR (regex-comp {([^-_.$+!*'()0-9a-z])} 1))

(define (url-encode str)
  (replace " " str "+")
  (replace REGEX_HTTP_SPECIAL_CHAR str (hex-encode-char $1) 0x10000))

;; @syntax (Web:url-decode <str>)
;; @param <str> a URL-encoded string
;; @return the decoded string
;; <p>Decodes hexidecimals and spaces (represented as '+') in a URL-encoded string.</p>
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))

(define (url-decode str)
  (replace "+" str " ")
  (replace REGEX_HEX_ENCODED_CHAR str (hex-decode-char $1) 0x10000))

;; @syntax (Web:parse-query <query-string>)
;; @param <query-string> a URL-encoded query string
;; @return an association list of decoded key-value pairs
;; <p>Parses a URL-encoded query string and returns a list of key-values pairs.</p>
(constant 'REGEX_QUERY (regex-comp {&([^&=]+?)=([^&=]+?)(?=&|$)} 1))

(define (parse-query query)
  (when (starts-with query "?")
    (pop query))
  (push "&" query)
  (find-all REGEX_QUERY query (list (url-decode $1) (url-decode $2)) 0x10000))

;; @syntax (Web:build-query <a-list>)
;; @param <a-list> an association list
;; @return a URL-encoded query string
;; <p>Builds a URL-encoded query string using <a-list>. Does not include the leading
;; question mark (so queries may be easily built of association list fragments.)</p>
(define (build-query alist , query)
  (join (map (fn (pair) (join (map url-encode pair) "=")) alist) "&"))

;; @syntax (Web:parse-url <str-url>)
;; @param <str-url> a URL
;; @return an association list with the decomposed URL's parts
;; <p>Parses a URL and returns an association list of its decomposed parts. The list's
;; keys (as strings) are: scheme, user, pass, host, port, path, query, and fragment.
;; Also handles IPV6 addresses. Modeled on the PHP function of the same name.</p>
;; 
;; Parsing based on code from @link http://us3.php.net/manual/en/function.parse-url.php#90365 this&nbsp;comment.
(constant 'REGEX_URL
  (regex-comp
    [text]
      (?:([a-z0-9+-._]+)://)?
      (?:
        (?:((?:[a-z0-9-._~!$&'()*+,;=:]|%[0-9a-f]{2})*)@)?
        (?:\[((?:[a-z0-9:])*)\])?
        ((?:[a-z0-9-._~!$&'()*+,;=]|%[0-9a-f]{2})*)
        (?::(\d*))?
        (/(?:[a-z0-9-._~!$&'()*+,;=:@/]|%[0-9a-f]{2})*)?
        |
        (/?
          (?:[a-z0-9-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+
          (?:[a-z0-9-._~!$&'()*+,;=:@/]|%[0-9a-f]{2})*
        )?
      )
      (?:\?((?:[a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9a-f]{2})*))?
      (?:\#((?:[a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9a-f]{2})*))?
    [/text]
    (| 1 8)))

(define (parse-url url)
  ;; clear indices of previous matches
  (dolist (idx '($0 $1 $2 $3 $4 $5 $6 $7 $8 $9))
    (set idx nil))
  (when (regex REGEX_URL url 0x10000)
    (let ((user-pass (parse $2 ":")))
      (list
        (list "scheme" (if (null? $1) "http" $1))
        (list "user" (when user-pass (first user-pass)))
        (list "pass" (when (and user-pass (= (length user-pass) 2)) (last user-pass)))
        (list "host" (if-not (null? $3) $3 $4))
        (list "port" (if (null? $5) nil $5))
        (list "path" (if (and (null? $6) (null? $7)) "/" (string $6 $7)))
        (list "query" (if (null? $8) nil $8))
        (list "fragment" (if (null? $9) nil $9))))))

;; @syntax (Web:build-url <str-url> [<list-query-params> ...])
;; @param <str-url> a string URL
;; @param <list-query-params> one or more association lists of query parameters and their values
;; 
;; @syntax (Web:build-url <list-url> [<list-query-params> ...])
;; @param <list-url> an association list of URL components using the structure of <parse-url>'s return value
;; @param <list-query-params> one or more association lists of query parameters and their values
;; @return a URL string composed of the initial URL data plus subsequently superseding query parameters
;; <p>In the first syntax, builds a URL from an existing URL string.
;; In the second syntax, builds a URL from an association list in the same
;; format as the return value of <parse-url>, with both keys and values being
;; strings. In both syntaxes, any number of additional association lists of
;; key/value pairs may be passed, which are serialized as query parameters, with
;; each list overriding the previous. If there are query parameters in the
;; initial URL, they are used as the initial list with the lowest priority.</p>
(define (build-url url)
  (when (string? url)
    (setf url (parse-url url)))

  (local (params)
    ;; Build parameter list
    (setf params '())
    (dolist (pairs (cons (lookup "query" url) (args)))
      (when (string? pairs) (setf pairs (parse-query pairs)))
      (dolist (pair pairs)
        (if (assoc (first pair) params)
          (setf (assoc (first pair) params) pair)
          (push pair params))))
    
    (format "%s://%s%s%s%s%s%s"
      (or (lookup "scheme" url) "http")
      (cond
        ((and (lookup "user" url) (lookup "pass" url))
         (string (lookup "user" url) ":" (lookup "pass" url) "@"))
        ((lookup "user" url)
         (string (lookup "user" url) "@"))
        (true ""))
      (lookup "host" url)
      (if (lookup "port" url) (string ":" (lookup "port" url)) "")
      (lookup "path" url)
      (if (null? params) "" (string "?" (build-query params)))
      (if (lookup "fragment" url) (string "#" (lookup "fragment" url)) ""))))

;===============================================================================
; !Headers, COOKIES, GET, and POST
;===============================================================================

;; @syntax (Web:header <str-key> <str-value>)
;; @param <str-key> the header name (e.g., "Content-type")
;; @param <str-value> the header value (e.g., "text/html")
;; <p>Sets an HTTP output header. Headers are printed using 'Web:send-headers'.</p>
(define headers '(("Content-type" "text/html")))

(define (header key value)
  (if (lookup key headers)
    (setf (assoc key headers) value)
    (push (list key value) headers -1)))

;; @syntax (Web:redir <str-url>)
;; @param <str-url> a URL string
;; <p>Redirects the client to <str-url>.</p>
(define (redir url)
  (header "Location" url))

;; @syntax (Web:send-headers)
;; <p>Writes the HTTP headers to stdout. This function should be called regardless
;; of whether any headers have been manually set to ensure that the minimum HTTP
;; headers are properly sent. Note: no check is made to verify that output has not
;; already begun.</p>
(define (send-headers)
  (dolist (header headers)
    (print (format "%s: %s\n" (first header) (last header))))
  (println))

;; @syntax (Web:cookie <str-key>)
;; @param <str-key> the cookie's name
;; 
;; @syntax (Web:cookie <str-key> <str-value> [<int-expires> [<str-path> [<str-domain> [<bool-http-only> [<bool-secure-only>]]]])
;; @param <str-key> the cookie's name
;; @param <str-key> the cookie's value
;; @param <int-expires> (optional) the expiration date of the cookie as a unix timestamp; default is a session cookie
;; @param <str-path> (optional) the cookie's path; default is the current path
;; @param <str-domain> (optional) the cookie's domain; default is the current host
;; @param <bool-http-only> (optional) whether the cookie may be read by client-side scripts
;; @param <bool-secure-only> (optional) whether the cookie may be accessed/set outside of HTTPS
;; <p>In the first syntax, 'cookie' returns the value of the cookie named <str-key> or 'nil'. If
;; <str-key> is not provided, an association list of all cookie values is returned.</p>
;; <p>In the second syntax, 'cookie' sets a new cookie or overwrites an existing cookie in the
;; client's browser. Note that <bool-http-only> defaults to true, but is not standard and
;; therefore is not necessarily implemented in all browsers. <bool-secure-only> defaults to nil.
;; Cookies use the 'header' function and must be sent before calling 'send-headers'.</p>
(define (cookie key value expires path domain http-only secure)
  (cond
    ((null? key) COOKIES)
    ((and (null? value) COOKIE)
     (lookup key COOKIE))
    (true
      (when (or (not secure) (and secure (starts-with (lower-case (env "SERVER_PROTOCOL")) "https")))
        (header "Set-Cookie"
          (format "%s=%s%s%s%s%s"
            (url-encode (string key))
            (url-encode (string value))
            (if expires (string "; expires=" (date expires 0 "%a, %d-%b-%Y %H:%M:%S %Z")) "")
            (if path (string "; path=" path) "")
            (if domain (string "; domain=" domain) "")
            (if-not http-only "; HttpOnly" "")))))))

;; @syntax (Web:get <str-key>)
;; <p>Returns the value of <str-key> in the query string or 'nil' if not present.
;; If <str-key> is not provided, returns an association list of all GET values.</p>
(define (get key)
  (when GET (if key (lookup key GET) GET)))

;; @syntax (Web:post <str-key>)
;; <p>Returns the value of <str-key> in the client-submitted POST data or 'nil' if
;; not present. If <str-key> is not provided, returns an association list of all
;; POST values.</p>
(define (post key)
  (when POST (if key (lookup key POST) POST)))

;===============================================================================
; !Session control
; notes:
;  * sessions require cookies to function
;  * close-session or MAIN:exit must be called to save session changes to disk
;===============================================================================

;; @syntax (Web:define-session-handlers <fn-open> <fn-close> <fn-delete> <fn-clear> <fn-clean>)
;; @param <fn-open> function to begin a new session
;; @param <fn-close> function to close a session, saving changes
;; @param <fn-delete> function to delete a session
;; @param <fn-clean> function to prune old sessions
;; <p>Defines handler functions to be called when various session control
;; functions are used, making custom session storage a fairly simple matter.</p>
;; The required handler functions are:
;; <ul>
;; <li>'fn-open': called by 'open-session'; resumes or starts a new session storage instance, initializing the context tree</li>
;; <li>'fn-close': called by 'close-session'; writes changes to a session to storage</li>
;; <li>'fn-delete': called by 'delete-session'; deletes the entire session from storage</li>
;; <li>'fn-clean': called by 'clean-sessions'; prunes old stored sessions</li>
;; </ul>
;; Some useful functions and variables for handler functions:
;; <ul>
;; <li>'session-id': function that returns the current session id and sets the session cookie when necessary</li>
;; <li>'session-context': function that returns the session context dictionary</li>
;; <li>'SESSION_MAX_AGE': a variable storing the number of seconds after which an orphan session should be deleted</li>
;; </ul>
(define (define-session-handlers fn-open fn-close fn-delete fn-clean)
  (setf _open-session fn-open
        _close-session fn-close
        _delete-session fn-delete
        _clean-sessions fn-clean))

;; @syntax (Web:session-id [<str-sid>])
;; @param <str-sid> (optional) the session ID to use
;; @return a unique session id for the client
;; <p>Creates or retrieves the client's session id. If this is a new session id,
;; a cookie is set in the client's browser to identify it on future loads.</p>
;; <p>If <str-sid> is provided, it will be used as the new session ID.</p>
(define (session-id sid)
  (setf SESSION_ID
    (or (when sid
          (cookie SESSION_KEY sid)
          sid)
        SESSION_ID
        (cookie SESSION_KEY)
        (begin
          (setf sid (string SESSION_PREFIX "-" (uuid)))
          (cookie SESSION_KEY sid)
          sid))))

;; @syntax (Web:session-context)
;; @return a symbol pointing to the current session's context dictionary
;; <p>Run-time session data is stored in a context tree. 'session-context'
;; returns the current session tree or creates a new one when necessary.
;; This function is primarily intended for session handlers' use; it is
;; typically more useful to call 'session' on its own to retrieve an association
;; list of key/value pairs in an application.</p>
(define (session-context , ctx)
  (setf ctx (sym (session-id) 'MAIN))
  (unless (context? ctx)
    (context ctx))
  ctx)

;; @syntax (Web:open-session)
;; <p>Initializes the client's session.</p>
(define (open-session)
  (_open-session)
  (setf SESSION_STARTED true)
  (session-id))

;; @syntax (close-session)
;; <p>Writes any changes to the session to file. This is automatically called
;; when the distribution function 'exit' is called.</p>
(define (close-session)
  (when SESSION_STARTED
    (_close-session)))

;; @syntax (delete-session)
;; <p>Deletes the session. Sessions are then turned off and 'open-session'
;; must be called again to use sessions further.</p>
(define (delete-session)
  (unless SESSION_STARTED (throw-error "session is not started"))
  (_delete-session)
  (delete (session-context))
  (cookie SESSION_KEY "" 0)
  (setf SESSION_STARTED nil))

;; @syntax (clear-session)
;; <p>Clears all session variables.</p>
(define (clear-session)
  (when SESSION_STARTED
    (dotree (s (session-context))
      (delete (sym s (session-context))))))

;; @syntax (clean-sessions)
;; <p>Cleans old session files. This function is not currently called automatically;
;; note that there is the possibility of a race condition with this function and other
;; session handling functions.</p>
(define (clean-sessions)
  (_clean-sessions))

;; @syntax (session [<str-key> [<str-value>]])
;; @param <str-key> the session key
;; @param <str-value> the new value
;; When called with both <str-key> and <str-value>, sets the session variable. When
;; called with only <str-key>, returns the value of <str-key>. Otherwise, returns
;; an association list of session variables. Returns nil if the session is not
;; opened.
(define (session key value)
  (cond
    ((not SESSION_STARTED) nil)
    ((and key value) (context (session-context) key value))
    ((true? key) (context (session-context) key))
    (true (let ((alist '()))
            (dotree (s (session-context))
              (push (list (name s) (context (session-context) (name s))) alist -1))
            alist))))

;===============================================================================
; !Default session handlers
; 
; The default session handlers use newLISP's 'save' and 'load' functions to
; easily serialize and import context data to and from file records. The files
; are stored unencrypted, so a custom handler should be used on a shared
; system.
;===============================================================================

; Returns the name of the file in which the session data is stored.
(define (default-session-file)
  (string SESSION_DIR "/" (session-id) ".lsp"))

; Loads/creates the session file; creates a new context tree when
; necessary.
(define (default-open-session)
  (if (file? (default-session-file))
    (load (default-session-file))
    (save (default-session-file) (session-context))))

; Saves the session context to the session file.
(define (default-close-session)
  (save (default-session-file) (session-context)))

; Deletes the session file.
(define (default-delete-session)
  (when (file? (default-session-file))
    (delete-file (default-session-file))))

; Deletes old session files.
(define (default-clean-sessions , f)
  (dolist (tmp-file (directory SESSION_DIR))
    (when (starts-with tmp-file SESSION_PREFIX)
      (setf f (string SESSION_DIR "/" tmp-file))
      (when (> (- (date-value) (file-info f 5 nil)) SESSION_MAX_AGE)
        (delete-file f)))))

;===============================================================================
; !Templating
;===============================================================================

;; @syntax (Web:eval-template <str-template> <ctx-context>)
;; @param <str-template> a string containing the template syntax
;; @param <ctx-context> the context in which to evaluate the template
;; <p>Translates a template using ASP-like tags, creating small islands of
;; newLISP code in an HTML (or other) document. This is similar to the
;; distribution CGI module&apos;s 'put-page' function, except that the short-cut
;; &lt;%= foo %&gt; is used to simply output the value of 'foo' and tags
;; may span multiple lines.</p>
;; <p>Note that the opening and closing tags may be changed by setting the
;; values of 'Web:OPEN_TAG' and 'Web:CLOSE_TAG' if desired. The shortcut
;; print tag will be 'Web:OPEN_TAG' + '='.</p>
;; @example
;; (Web:eval-template "&lt;p&gt;&lt;%= (* 3 3) %&gt;&lt;/p&gt;")
;; =&gt; "&lt;p&gt;9&lt;/p&gt;"
;; (Web:eval-template "&lt;p&gt;&lt;% (println (* 3 3)) %&gt;&lt;/p&gt;")
;; =&gt; "&lt;p&gt;9&lt;/p&gt;"
(define OPEN_TAG "<%")
(define CLOSE_TAG "%>")

(define (eval-template str (ctx MAIN) , start end next-start next-end block (buf ""))
  (setf start (find OPEN_TAG str))
  (setf end (find CLOSE_TAG str))
  
  ;; Prevent use of code island tags inside code island from breaking parsing.
  (when (and start end)
    (while (and (setf next-end (find CLOSE_TAG (slice str (+ end 2))))
                (setf next-start (find OPEN_TAG (slice str (+ end 2))))
                (< next-end next-start))
      (inc end (+ next-end 2)))
    (when (and start (not end)) (throw-error "Unbalanced tags.")))
  
  (while (and start end)
    (write-buffer buf (string "(print [text]" (slice str 0 start) "[/text])"))
    (setf block (slice str (+ start 2) (- end start 2)))
    (if (starts-with block "=")
      (write-buffer buf (string "(print " (rest block) ")"))
      (write-buffer buf (trim block)))
    (setf str (slice str (+ end 2)))
    (setf start (find OPEN_TAG str))
    (setf end (find CLOSE_TAG str))
    
    ;; Prevent use of code island tags inside code island from breaking parsing.
    (when (and start end)
      (while (and (setf next-end (find CLOSE_TAG (slice str (+ end 2))))
                  (setf next-start (find OPEN_TAG (slice str (+ end 2))))
                  (< next-end next-start))
        (inc end (+ next-end 2)))
      (when (and start (not end)) (throw-error "Unbalanced tags."))))
  
  (write-buffer buf (string "(print [text]" str "[/text])"))
  (eval-string buf ctx))

;===============================================================================
; !Module initialization
; 
; Install default session handlers and create the GET, POST, and COOKIE data
; structures.
;===============================================================================

; Content-Disposition: form-data; name="file"; filename="white-napkin.jpg"\r\nContent-Type: image/jpeg\r\n\r\n\253\152\191\160\128\144JFIF
; Content-Disposition: form-data; name="text"\r\n\r\nadsf\r\n
(define (mime-decode str , content-type parts re decoded)
  (when (setf content-type (regex {^multipart/form-data; boundary=(.+?)$} (env "CONTENT_TYPE") 1))
    (setf parts (find-all (string "--" (content-type 3) {\r\n(.+?)(?=--)}) str $1 (| 2 4)))
    (dolist (part parts)
      (cond
        ((regex {Content-Disposition: form-data; name="(.+?)"\r\n\r\n(.*?)\s+} part 1)
         (push (list $1 $2) decoded -1))
        ((regex {Content-Disposition: form-data; name="(.+?)"; filename="(.+?)"\r\nContent-Type: (.+?)\r\n\r\n(.*)$} part (| 1 2 4))
         (push (list $1 (list (list "filename" $2) (list "content-type" $3) (list "bytes" $4))) decoded -1))))
    decoded))


; Install default session handlers
(define-session-handlers
  default-open-session
  default-close-session
  default-delete-session
  default-clean-sessions)

; Read GET data
(setf GET
  (when (env "QUERY_STRING")
    (parse-query (env "QUERY_STRING"))))

; Read POST data
(if-not (context? CGI)
  ;; CGI module not present; read and parse the POST data ourselves
  (let ((post "") (buffer ""))
   (unless (zero? (peek (device)))
    (while (read-buffer (device) buffer POST_LIMIT)
     (write-buffer post buffer)))

   (setf POST (when post (parse-query post))))

;This will replace the above line once mim-decode actually works.
;(setf POST
; (when post
;  (if (env "CONTENT_TYPE")
;   (mime-decode post)
;   (parse-query post)))))
  
  ;; CGI module present; try to guess which values in CGI:params are
  ;; from GET and which are from POST.
  (begin
    (setf POST '())
    (dolist (param CGI:params)
      (unless (lookup (first param) GET)
        (push param POST)))))

; Read COOKIE data
(setf COOKIE
  (when (env "HTTP_COOKIE")
    (map
      (lambda (cookie , n)
        (setf n (find "=" cookie))
        (list (url-decode (slice cookie 0 n))
              (url-decode (slice cookie (+ 1 n)))))
      (parse (env "HTTP_COOKIE") "; *" 0))))

(context 'MAIN)

; This function wraps the distribution exit routine to ensure that sessions are
; written when the application exits. It is only called when the 'exit' function
; is explicitly called. The 'exit' function is renamed 'sys-exit'. The 'Web'
; function 'close-session' is only called on a normal exit (exit code 0.)
(define (exit-with-session-close (n 0))
  (when (zero? n)
    (Web:close-session))
  (MAIN:sys-exit))

(constant 'sys-exit exit)
(constant 'exit exit-with-session-close)
