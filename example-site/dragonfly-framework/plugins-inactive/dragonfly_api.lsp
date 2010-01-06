;;  Copyright (C) <2009> <Marc Hildmann, Greg Slepak>
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; @module Dragonfly
;; @author Marc Hildmann <marc.hildmann at gmail.com>, Greg Slepak <greg at taoeffect.com>
;; @version 0.50
;; 
;; @location http://code.google.com/p/dragonfly-newlisp/
;; @description A newLISP web framework for rapid web development
;; <h4>About Dragonfly web framework</h4>
;; <p>Dragonfly is a small web framework which is currently under heavy development.
;; Its's features are a short learning curve, lightweight and fun in programming - 
;; just like newLISP itself.</p>

(context 'Dragonfly)

(define (api-browser api-dir)
	(print (format [text]
		<div id="api-browser">&nbsp</div>
		<script type="text/javascript">
			(function api_browse(path, anchor) {
				$.post("%s", "path=" + path,
					function (data) {
						$("#api-browser").html(data);
						$("#api-browser a").click(function () {
							// don't fix external links
							if ( this.href.indexOf(location.host) != -1 )
							{
								var url = this.href.split('/').pop();
								var parts = url.split('#');
								api_browse('%s/'+parts[0],
									parts.length == 2 ? parts[1] : null);
								return false;
							}
						});
						if (anchor)
							$.scrollTo("#api-browser a[name='"+anchor+"']", 400);
					}
				);
			})("%s/index.html");
		</script>
		[/text] (web-root "dragonfly_ajax-api") api-dir api-dir)
	)
)

(context Dragonfly)
