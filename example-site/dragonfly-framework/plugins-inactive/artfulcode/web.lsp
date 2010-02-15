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

(define ENTITIES '(
    (34 {&quot;})       (38 {&amp;})        (39 {&apos;})       (60 {&lt;})
    (62 {&gt;})         (160 {&nbsp;})      (161 {&iexcl;})     (162 {&cent;})
    (163 {&pound;})     (164 {&curren;})    (165 {&yen;})       (166 {&brvbar;})
    (167 {&sect;})      (168 {&uml;})       (169 {&copy;})      (170 {&ordf;})
    (171 {&laquo;})     (172 {&not;})       (173 {&shy;})       (174 {&reg;})
    (175 {&macr;})      (176 {&deg;})       (177 {&plusmn;})    (178 {&sup2;})
    (179 {&sup3;})      (180 {&acute;})     (181 {&micro;})     (182 {&para;})
    (183 {&middot;})    (184 {&cedil;})     (185 {&sup1;})      (186 {&ordm;})
    (187 {&raquo;})     (188 {&frac14;})    (189 {&frac12;})    (190 {&frac34;})
    (191 {&iquest;})    (192 {&Agrave;})    (193 {&Aacute;})    (194 {&Acirc;})
    (195 {&Atilde;})    (196 {&Auml;})      (197 {&Aring;})     (198 {&AElig;}) 
    (199 {&Ccedil;})    (200 {&Egrave;})    (201 {&Eacute;})    (202 {&Ecirc;})
    (203 {&Euml;})      (204 {&Igrave;})    (205 {&Iacute;})    (206 {&Icirc;})
    (207 {&Iuml;})      (208 {&ETH;})       (209 {&Ntilde;})    (210 {&Ograve;})
    (211 {&Oacute;})    (212 {&Ocirc;})     (213 {&Otilde;})    (214 {&Ouml;})
    (215 {&times;})     (216 {&Oslash;})    (217 {&Ugrave;})    (218 {&Uacute;})
    (219 {&Ucirc;})     (220 {&Uuml;})      (221 {&Yacute;})    (222 {&THORN;})
    (223 {&szlig;})     (224 {&agrave;})    (225 {&aacute;})    (226 {&acirc;})
    (227 {&atilde;})    (228 {&auml;})      (229 {&aring;})     (230 {&aelig;})
    (231 {&ccedil;})    (232 {&egrave;})    (233 {&eacute;})    (234 {&ecirc;})
    (235 {&euml;})      (236 {&igrave;})    (237 {&iacute;})    (238 {&icirc;})
    (239 {&iuml;})      (240 {&eth;})       (241 {&ntilde;})    (242 {&ograve;})
    (243 {&oacute;})    (244 {&ocirc;})     (245 {&otilde;})    (246 {&ouml;})
    (247 {&divide;})    (248 {&oslash;})    (249 {&ugrave;})    (250 {&uacute;})
    (251 {&ucirc;})     (252 {&uuml;})      (253 {&yacute;})    (254 {&thorn;})
    (255 {&yuml;})      (338 {&OElig;})     (339 {&oelig;})     (352 {&Scaron;})
    (353 {&scaron;})    (376 {&Yuml;})      (402 {&fnof;})      (710 {&circ;})
    (732 {&tilde;})     (913 {&Alpha;})     (914 {&Beta;})      (915 {&Gamma;})
    (916 {&Delta;})     (917 {&Epsilon;})   (918 {&Zeta;})      (919 {&Eta;})
    (920 {&Theta;})     (921 {&Iota;})      (922 {&Kappa;})     (923 {&Lambda;})
    (924 {&Mu;})        (925 {&Nu;})        (926 {&Xi;})        (927 {&Omicron;})
    (928 {&Pi;})        (929 {&Rho;})       (931 {&Sigma;})     (932 {&Tau;})
    (933 {&Upsilon;})   (934 {&Phi;})       (935 {&Chi;})       (936 {&Psi;})
    (937 {&Omega;})     (945 {&alpha;})     (946 {&beta;})      (947 {&gamma;})
    (948 {&delta;})     (949 {&epsilon;})   (950 {&zeta;})      (951 {&eta;})
    (952 {&theta;})     (953 {&iota;})      (954 {&kappa;})     (955 {&lambda;})
    (956 {&mu;})        (957 {&nu;})        (958 {&xi;})        (959 {&omicron;})
    (960 {&pi;})        (961 {&rho;})       (962 {&sigmaf;})    (963 {&sigma;})
    (964 {&tau;})       (965 {&upsilon;})   (966 {&phi;})       (967 {&chi;})
    (968 {&psi;})       (969 {&omega;})     (977 {&thetasym;})  (978 {&upsih;})
    (982 {&piv;})       (8194 {&ensp;})     (8195 {&emsp;})     (8201 {&thinsp;})
    (8204 {&zwnj;})     (8204 {&zwj;})      (8204 {&lrm;})      (8204 {&rlm;})
    (8211 {&ndash;})    (8212 {&mdash;})    (8216 {&lsquo;})    (8217 {&rsquo;})
    (8218 {&sbquo;})    (8220 {&ldquo;})    (8221 {&rdquo;})    (8222 {&bdquo;})
    (8224 {&dagger;})   (8225 {&Dagger;})   (8226 {&bull;})     (8230 {&hellip;})
    (8240 {&permil;})   (8242 {&prime;})    (8243 {&Prime;})    (8249 {&lsaquo;})
    (8250 {&rsaquo;})   (8254 {&oline;})    (8260 {&frasl;})    (8364 {&euro;})
    (8465 {&image;})    (8472 {&weierp;})   (8476 {&real;})     (8482 {&trade;})
    (8501 {&alefsym;})  (8592 {&larr;})     (8593 {&uarr;})     (8594 {&rarr;})
    (8595 {&darr;})     (8596 {&harr;})     (8629 {&crarr;})    (8656 {&lArr;})
    (8657 {&uArr;})     (8658 {&rArr;})     (8659 {&dArr;})     (8660 {&hArr;})
    (8704 {&forall;})   (8706 {&part;})     (8707 {&exist;})    (8709 {&empty;})
    (8711 {&nabla;})    (8712 {&isin;})     (8713 {&notin;})    (8715 {&ni;})
    (8719 {&prod;})     (8721 {&sum;})      (8722 {&minus;})    (8727 {&lowast;})
    (8730 {&radic;})    (8733 {&prop;})     (8734 {&infin;})    (8736 {&ang;})
    (8743 {&and;})      (8744 {&or;})       (8745 {&cap;})      (8746 {&cup;})
    (8747 {&int;})      (8756 {&there4;})   (8764 {&sim;})      (8773 {&cong;})
    (8776 {&asymp;})    (8800 {&ne;})       (8801 {&equiv;})    (8804 {&le;})
    (8805 {&ge;})       (8834 {&sub;})      (8835 {&sup;})      (8836 {&nsub;})
    (8838 {&sube;})     (8839 {&supe;})     (8853 {&oplus;})    (8855 {&otimes;})
    (8869 {&perp;})     (8901 {&sdot;})     (8968 {&lceil;})    (8969 {&rceil;})
    (8970 {&lfloor;})   (8971 {&rfloor;})   (9001 {&lang;})     (9002 {&rang;})
    (9674 {&loz;})      (9824 {&spades;})   (9827 {&clubs;})    (9829 {&hearts;})
    (9830 {&diams;})
))

(define UNENTITIES
  (map reverse ENTITIES))

(define JS_ESCAPE_CHARS '(
    ({\} {\\})
    ({"} {\"})
    ({'} {\'})
    ("\n" {\n})
    ("\r" {\r})
    ("</" {<\/})
))

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
    (extend buf
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

(context 'MAIN)
