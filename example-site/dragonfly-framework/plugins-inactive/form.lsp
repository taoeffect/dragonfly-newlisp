;; @module form.lsp 
;; @description A library for creating HTML forms
;; @version 1.0 - Partial rewrite for Dragonfly. Addition attachments, custom port and proper utf8 encoding for subject/message/attachments
;; @author Marc Hildmann 2009-2010

(context 'FORM)

;===============================================================================
; !Form Functions
;===============================================================================

;; @syntax (FORM:open <form_action> <form_method)
;; @param <name> a string containing the form action
;; @param <name> a string containing the form submit method POST/GET
;; <p>Writes a standard HTML form open element</p>
;; 
(define (create form_action form_method)
  (print "<form action=\""form_action"\" method=\""form_method"\">")
)

;; @syntax (Dragonfly:form_textfield <form_name>)
;; @param <name> a string containing the text field name
;; <p>Writes a standard form field with size '30'</p>
;; 
(define (form-textfield form_name)
  (print "<tr class='form_row'><td class='form_label'><label for='"form_name"'>"(title-case form_name)"</label></td><td class='form_input'><input id='"form_name"' name='"form_name"' size='30' type='text' /></td></tr>")
)

;; @syntax (FORM:input <input_type> <input_value>)
;; @param <input_type> a string containing the input type
;; @param <input_value> a string containing the input value
;; <p>Writes a standard form input element.</p>
;; 
(define (input input_type input_value, input_name)
  (print "<input type=\""input_type"\" value=\""input_value"\" name=\""input_name"\" />")
)

;; @syntax (Dragonfly:form-submit-js <form_action> <form_value>)
;; @param <form_action> a string containing the URI after submitting the submit button
;; @param <form_value> a string containing the value for the submit button
;; <p>Writes a form submit button with some javascript
;; which modifies the action attribute.</p>
;; 
(define (form-submit-js form_action form_value)
  (print "<tr class='form_row'><td class='form_label'>&nbsp;</td><td class='form_input'><input id='commit' type='button' value='"form_value"' onClick='document.forms[0].action=\""form_action"\"+document.forms[0].Text.value;document.forms[0].submit();' /></td></tr>")
)

;; @syntax (FORM:close)
;; <p>Writes a form close tag</p>
;; 
(define (end)
  (print "</form>")
)

;; @syntax (Dragonfly:form_generate-from-columns <tablename>)
;; <p>Generates a form including input field from the given table.
;; The form uses POST Method and refers to itself. It includes one hidden
;; field called dragonfly_form, to detect form submit.
;; </p>
;; 
(define (form-generate-from-columns database tablename)
  (Dragonfly:load-database database) ; load the specified nldb
  (set 'columns (nldb:list-columns tablename))
  (print "<table><form id='form_"table"' action='?"viewname"/"{save}"' method='POST'>")
	(dolist (column_name columns)
		(print "<tr class='form_row'><td class='form_label'><label for='"column_name"'>"column_name"</label></td><td class='form_input'><input id='"column_name"' name='"column_name"' size='30' type='text' /></td></tr>")
	)
  (print "<tr class='form_row'><td class='form_label'>&nbsp;</td><td class='form_input'><input id='form_submit' type='submit' name='commit' value='Add' /></td></tr>")
  (print "<input type='hidden' name='databasetable' value='"table"' />")
  (print "</form></table>")
)



(context MAIN)