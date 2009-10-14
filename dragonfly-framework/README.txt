;;
;; Dragonfly - a newLISP web framework
;; Version 0.20
;; 
;;  Copyright (C) <2009>  <Marc Hildmann>
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
;;
;; Start this application on your localhost OS X
;; newlisp -http -d PORT -w /Users/USERNAME/Sites/DIRECTORY &
;;
;; Open Your browser and type localhost:8080 - have FUN!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Version 0.20

  * changed license from MIT to GNU (GPL v3)
  * updated web.lsp to version 0.3.1 beta
  * updated nldb.lsp (draft of 2009-09-11 20:42:48)
  * added a directory "js"
  * added the file "dragonfly.js" which includes some small javascripts which support the Dragonfly framework (AJAX, iPhone, Dragonfly Guide Menu)
  * added the first AJAX function to Dragonfly: ajax-updater (check out the twitter demo)
  * added a Dragonfly guide (it can be opened with /dragonfly_welcome/index)
  * doing some tests with a new Listener (it's called listener2 in the dragonfly.lsp)
  * trying to implement an image-helper for reading EXIF data from images
  

Version 0.19

  * modified Dragonfly:view; now it displays an error view if a file is not found
  * moved the constant "default404" into dragonfly_config.lsp
  * added the function autoload-css; this function checks automatically for an iPhone and loads a different stylesheet


Version 0.18

  * added some functions to send HTTP headers - we don't use Web:send-headers anymore
  * added HTTP Status Codes constants for Dragonfly:listener
  * added a condition to Dragonfly:listener for sending correct XML-Headers
  * added the action parameter to Dragonfly:link_to
 
Version 0.17

  * changed path to newLISP (/usr/bin/env newlisp) 
  * added a config directory, so updating of Dragonfly would be easier now (we auto-load the config files ;)

Version 0.16

  * improved the directory structure: added three folders (dragonfly-framework, modules-active, modules-inactive)
  * added an "auto-loader for modules" to Dragonfly: to load a module just put it into "modules-active", to NOT load a module put it into "modules-inactive"

Version 0.15

  * testing some ideas about custom routes

Version 0.14

  * removed the directory "actions" (it's now obsolet)
  * renamed the directory "templates" into "views"
  * renamed the function "Dragonfly:template" into "Dragonfly:view"
  * added the constants "defaultview" and "defaultaction"
  * added the constants "views-path" and "partials-path"
  * added an improved version of Dragonfly:listener contributed by cormullion
  * modified the .htaccess (also contributed by cormullion)
  * improved the link_to function - it automatically checks if it can use .htaccess or not; it also uses the defaultaction now

Version 0.13

  * added a .htaccess to remove the question mark "?"
  * bugfixing some paths after using mod_rewrite
  * testing on nfshost.com

Version 0.12

  * renamed directory - it's called Dragonfly now
  * thinking about a directory structure to handle multiple apps with one installation of Dragonfly
  * added a memory calculation to benchmark function
  * added a listener function
  * removed the "case" functions in index.cgi for calling template - this work is now done by the listener
  * did some bugfixing on the SQLite functions

Version 0.11

  * removed global flash notice
  * removed start.bat and newlisp.exe

Version 0.10

  * added some function for SEO (I needed that at my work)
  * I split up the css - there will be a separate dragonfly.css for some nice layouts
  * added a benchmark function
  * added the MIT License
  * added a start.bat for easy start on Windows
  * testing an idea about a global flash notice (doesn't work yet)


Version 0.09

  * some bugfixes
  * added the function google-results-domain just for fun (the first time I use some regular expressions)
  * added some features to the Dragonfly debug panel

  * 29 downloads so far ... :-)
   
Version 0.08

  * added a path to SQLite library for debian in module sqlite3.lsp ("/usr/lib/libsqlite3.so.0" ; Debian)

Version 0.07

  * added a twitter search with some nice css for speech bubbles
  * added a demo template for twitter

Version 0.06

  * testing out the idea of partials in templates
  * added a Dragonfly demo page

Version 0.05

  * added a sliding panel for debugging information
  * extended the debugging information
  * added functions for generating <script> and <rss> tags

Version 0.04

  * added a simple page-controller architecture in index.cgi
  * generated a newLISP-Doc for Dragonfly.lsp in the module directory

Version 0.03

  * finalized the table generator functions, especially (table_data)

Version 0.02

  * added the functions (form_hidden),(form_textfield)
  * playing around with nldb.lsp - but didn't really succeed

Version 0.01

  * opened a first template called "main.tpl"
  * added the first functions (css),(meta),(time-now),(form_open),(form_submit),(form_close)