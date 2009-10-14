;; @module nldb
;; @description a simple pure-newLISP database
;; @author cormullion (cormullion at mac.com), building on work by (kinghajj at gmail.com)
;; @location http://unbalanced-parentheses.nfshost.com/downloads
;; @version draft of 2009-09-11 20:42:48
;;<h4>About the nldb module</h4>
;;<p>This is a simple database module, for those occasions when it's not possible
;; to install or use something better, such as sqlite.</p>

(context 'nldb)

(set 'tables '()) ; this holds the list of table names

(define (create-table table-name (column-list 'column-1))
;; @syntax (create-table table-name <column-list>)
;; Create a table with columns in <column-list.>
  (push table-name tables -1)
  (set table-name (list column-list)))

(define (list-columns table-name)
;; @syntax (list-columns table-name)
;; List columns in table. table isn't quoted.
 (first table-name))

(define (count-columns table-name)
;; @syntax (count-columns table-name)
;; Count number of columns in table. table isn't quoted.
    (if table-name (length (list-columns table-name))))

(define (add-columns table-name new-column-list (value nil))
;; @syntax (add-columns table-name new-column-list value)
;; Add more columns to table (and every row in table).
;; You can provide a default value for them.
;; (add-columns 'elements '(Price Postage) 0)
;; (add-columns 'elements '(Uses) '(chemistry physics)) ; add a list field
    (letex ((table table-name))
        (let ((columns (first table)))
             ; mustn't duplicate columns
             (if (empty? (intersect new-column-list columns))
                 (setf table
                     (cons (append columns new-column-list)
                         (map  (fn (r) (append r (dup value (length new-column-list)))) 
                               (rest table))))))))

(define (truncate data-list size)
;; @syntax (truncate data-list size)
  (0 size (append data-list (dup nil size))))

(define (add-row table-name data)
;; @syntax (add-row table-name data)
;; Add a row to the table. Supply all the fields.
;; @example
;; (add-row 'elements1 '(1 1.0079 "Hydrogen" "H"))
    (if (list? data) 
        (letex ((table table-name))
               (push (truncate data (count-columns table)) table -1))))

(define (new-row table-name assoc-list)
;; @syntax (new-row table-name assoc-list)
;; Add a row to the table. Supply some values for some of the columns.
;; Any missing columns are set to nil.
;; @example
;; (new-row 'elements '((Name "Unobtainium") (Symbol "Ub")))
    (letex ((table table-name))
        (let ((columns (first table)))
            (push (map last 
              (map (fn (c) (or (assoc c assoc-list) 
                               (list c nil))) 
                    columns))
                    table -1))))

(define (select-rows the-table (select-fn true) (column-list true) (sort-column nil) (sort-function nil))
;; @syntax (select-rows the-table (select-fn true) (column-list true) (sort-column nil) (sort-function nil))
;; Select rows using select-function, returning columns in column-list, sorted by sort-column/sort-function.
;; Use true to select all rows/columns.
;; @example 
;; (select-rows 'elements)
;; (select-rows 'elements '(= Name "Helium")) ; the selection function looks for Name = "Helium"
;; (select-rows 'elements '(< DiscoveryYear 1600))
;; (select-rows 'elements true true 'Symbol >) ; all columns of all elements sorted by symbol inversely
;; (select-rows 'elements '(and (> EarthCrust 1) (< DiscoveryYear 1900)))
;; (select-rows 'elements true true 'Name <) ; all elements, sorted by name
;; (select-rows 'elements true true 'Name (fn (x y) (< (length x) (length y)))) ; all sorted by length of name
;; (select-rows 'elements '(> DiscoveryYear 1900) '(Name Symbol)) ; some columns of recent elements, unsorted
   (letex ((table the-table))
     (let ((columns (first table))
           (selection '())
           (cell nil)
           (selection-function nil))
     (dolist (row (rest table))
        ; first, 'evaluate' the selection function for this row
        ; by replacing any keys in select-fn with actual values from rows
        (set 'selection-function select-fn)
        (if (list? selection-function)
            (dolist (field columns)
                (set 'cell (row (find field columns)))
                ; lists have to be quoted, otherwise they're evaluated too soon
                (set-ref-all field selection-function (if (list? cell) 'cell cell))))
        ; now, does the selection function select this record?
        (if (eval selection-function)   
            (push row selection -1)))
        
      ; do we have to sort selection?
      (when sort-function
          (sort selection (fn (x y) (apply sort-function (list (x (find sort-column columns)) (y (find sort-column columns)))))))
      
      ; finally, do we have to filter columns of selection?
      ; if column-list is true, show all columns   
      (if (list? column-list)
          (set 'selection (map (fn (r) (select r (map (fn (c) (find c columns)) column-list))) selection)))
     selection)))
 
 (define (modify-row table the-row column-name modify-fn)
;; @syntax (modify-row table the-row column-name modify-fn)
;; modify-fn must be a function that evaluates to provide a new value, so, to set 
;; a numeric value you might use "(int 42)"
   (let ((row the-row)
         (cell nil)
         (columns (first table)))
        ; first, 'evaluate' selection function for this row
        ; by replacing any quoted keys in select-fn with actual values from rows
        (dolist (field columns)
            (set 'cell (row (find field columns)))
            (set-ref-all field modify-fn (if (list? cell) 'cell cell)))
        ; so, does the selection function select this record?
        (setf (nth (find column-name columns) row) (eval modify-fn))
        ; return the modified row
        row))

(define (delete-rows table-name select-fn)
;; @syntax (delete-rows table-name select-fn)
;; Delete rows found by select-fn.
;; returns number of rows selected (and presumably deleted)
;; obviously database has to be saved for permanent deletion
;; @example
;; (delete-rows 'elements '(> DiscoveryYear 1945))
    (let ((table table-name)
          (selection '()))
       (set 'selection (select-rows table-name select-fn))
       (map (fn (row)  (replace row (eval table))) selection)
      ; return length of selection
    (length selection)))

(define (change-rows table-name select-fn column-name modify-fn)
;; @syntax (change-rows table-name select-fn column-name modify-fn)
;; Change value of column-name in matching rows of table-name found by select-fn, using modify-fn
;; @example
;; (change-rows 'elements '(= Name "Helium") 'Symbol '(reverse Name))
;; changes symbol of all elements matching "Helium" to reverse of Name field
;; (change-rows 'elements '(= Name "Lead") 'BP '(add BP MP)) 
;; changes Boiling Point of all elements matching Lead to sum of BP and MP fields
;; modify-fn  must be a function that evaluates to the new value
(let ((table table-name)
       (selection '()))       
       (set 'selection (select-rows table-name select-fn))
       (map (fn (row) 
           ; replace old rows with new: find the original rows
           ; then strip the assoc keys that we used to find the rows
           (replace row 
                    (eval table-name)
                    (modify-row (eval table-name) row column-name modify-fn)))
           selection)
      ; return number of records selected
      (length selection)))

(define (find-text table-name str (regex-option 0))
;; @syntax (find-text <table-name> <str> [<regex-option>])
;; Find string in text fields of rows in table.
;; default is regular exp option 0 - ie case-sensitive!
;; @example
;; (find-text 'elements "e")
	(letex ((table table-name))
		(let ((results '()))
			(map (fn (row)
				(if (find str row regex-option)
					(push row results -1)))
	            table)
      results)))

(define (sort-table table-name column function)
;; @example
;; (sort-table 'elements 'Name (fn (x y) (< (length x) (length y))))
;; (sort-table 'elements 'BP >)
  (letex
    ((table table-name))
    (letn ((columns (first table))
           (col     (find column columns)))
    ; set the table to a new list
    ; keep the first row (column names) intact
    ; allow the caller to specify sort column and function
       (setf table 
          (cons columns 
            (sort (rest table) (fn (x y) (apply function (list (x col) (y col))))))))))

(define (save-db filename)
;; @syntax (save-db filename)
;; Save the database tables in the named file, and a list of tables too. 
(let ((save-list tables))
    (push 'tables save-list)
	    (apply save (cons filename save-list)) 
    	;;(println "saved the database in " filename)
	)
)

(define (load-db filename)
;; @syntax (load-db filename)
;; Replace the curent data tables with ones from <filename>
   (load filename))

(define (show)
;; @syntax (show)
;; Show the database
  (println 
    (dup "_" 60) "\n"
    "Contents of database "  (context)  "\n"
    " " (length tables) " table" (if (> (length tables) 1) "s" "") ": " tables "\n")
  (dolist (table tables)
     (println " Table:  " table)
     (println " Columns " (first (eval table)))
     (println " Rows:   " (length (rest (eval table))))
     (dolist (row (rest (eval table)))
        (println row))
     (println))
  (println (dup "_" 60))
  (sym (date)))

; eof