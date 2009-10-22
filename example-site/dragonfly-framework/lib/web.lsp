#!/usr/bin/newlisp
;; @module Web
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 0.3.1 beta
;;
;; Modifictations (in C-style) by Greg Slepak <greg at taoeffect.com>
;; Based on version 0.3.1 beta by Jeff, located here:
;; http://static.artfulcode.net/newlisp/web.lsp.html
(context 'Web)

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
  (char (int (string "0x" $1))))

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
; !Templating
;===============================================================================

;; @syntax (Web:eval-template <str-template> [<ctx-context>])
;; @param <str-template> a string containing the template syntax
;; @param <ctx-context> Unless you have a very good reason for it, leave this alone.
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

(define (eval-template str (ctx Dragonfly) , start end next-start next-end block (buf ""))
	(while (and (setf start (find OPEN_TAG str)) (setf end (find CLOSE_TAG str)))
		(write-buffer buf (string "(print [text]" (slice str 0 start) "[/text])"))
		(setf block (slice str (+ start 2) (- end start 2)))
		(if (starts-with block "=")
			(write-buffer buf (string "(print " (rest block) ")"))
			(write-buffer buf block)
		)
		(setf str (slice str (+ end 2)))
	)
	(when str
		(write-buffer buf (string "(print [text]" str "[/text])"))
		(eval-string buf ctx)
	)
)

(context 'MAIN)
