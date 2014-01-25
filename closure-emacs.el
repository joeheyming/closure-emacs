;; closure-emacs.el --- Google closure emacs helper functions.
;;
;; Author: Joe Heyming (joeheyming@gmail.com)
;; Keywords: javascript, google closure

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:
;;
;; - This module works in conjunction with js2mode.
;; - The module provides a js2mode hook where we apply key bindings if we detect we are in a closure file.
;; - The keybindings all begin with super-c (c for closure)
;; - The key bindings are as follows:
;;  - super c a-f -> closure-abstract-function: 
;;    - insert an abstract function at the cursor
;;  - super c a-r -> closure-add-require-line
;;    - Look at the current module under the cursor, and try to add it to the list of require lines at the top of the file.
;;  - super c c-c -> closure-insert-current-class
;;    - Insert the current class name at the cursor.
;;  - super c c-f -> closure-constructor-function
;;    - Insert the template for a constructor method based on the current module.
;;  - super c g-b -> closure-insert-goog-base
;;    - Insert goog.base. with the correct parameters at the top of the current function you are in.
;;  - super c n-a -> closure-attribute-prefix
;;    - Insert a new attribute at the cursor for the current module.
;;  - super c n-f -> closure-new-function
;;    - Insert a new function at the cursor for the current module.
;;  - super c s-c -> closure-super-jump
;;    - Go to the current super class. 
;;  - super c s-f -> closure-super-function
;;    - Insert a new function at the cursor, but add /** inheritDoc */ and goog.base
;;  - super c s-g -> closure-insert-singleton-getter
;;    - Insert the singleton getter function at the cursor.
;;  - super c s-r -> closure-sort-require-lines
;;   - Sort the require lines in the current module.
;;  - super c u-e -> closure-update-extends
;;   - If you change @extends, run this function and it will update goog.inherits as well as add the require line.
;;  - super c u-j -> closure-update-javadoc
;;   - Update the javadoc with the current function parameters (if changed) and add @private or @return if needed.
;;  - super c a-e -> closure-foreach-array
;;   - Insert a goog.array.forEach at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
;;  - super c a-t -> closure-foreach-array-this
;;   - Insert a goog.array.forEach (bound to this) at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
;;  - super c o-e -> closure-foreach-object
;;   - Insert a goog.object.forEach at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
;;  - super c o-t -> closure-foreach-object-this
;;   - Insert a goog.object.forEach (bound to this) at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
;;
;; Installation:
;;  - put closure-emacs.el somewhere in your emacs load path
;;  - add these lines to your .emacs file:
;;  (require 'closure-emacs)
;; - Create a file under ~/.emacs-closure-projects
;;  - Put your path to the google closure library:
;; E.g. ~/git/closure-library/closure
;; Put the path to your closure projects.

(provide 'closure-emacs)

; xxx utility functions xxx ;
(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))
; xxx utility functions xxx ;

(defun closure-search-function-declaration ()
  (search-backward-regexp "^\\(.*[ ]+=[ ]+\\)?function[^(]*(")
  )

(defun closure-class-comment ()
  "Insert a class comment for an attribute"
  (insert "/**\n * \n */\n")
  )

(defun closure-class-name ()
  "return the class name of the active buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "^goog.provide('")
    (let ((beg) (end) )
      (setq beg (point))
      (search-forward "'")
      (setq end (- (point) 1))
      (buffer-substring beg end)
      )
    )
  )

(defun closure-attribute-prefix ()
  "Insert a classname with fully qualified path"
  (interactive)
  (closure-class-comment)
  (insert (format "%s. = ;" (closure-class-name)))
  (backward-char 4)
)

(defun closure-insert-singleton-getter()
  (interactive)
  (insert (format "goog.addSingletonGetter(%s);\n" (closure-class-name))))

(defun closure-foreach (for-format)
  (setq variable (closure-current-expression))
  (end-of-line)
  (insert "\n")
  (indent-according-to-mode)
  (insert (format for-format variable))
  (previous-line)
  (previous-line)
  )

(defun closure-foreach-array ()
  "Create a closure array.forEach loop"
  (interactive)
  (closure-foreach "goog.array.forEach(%s, function() {\n\n});\n")
  )


(defun closure-foreach-array-this ()
  "Create a closure array.forEach loop with this binding"
  (interactive)
  (closure-foreach "goog.array.forEach(%s, function() {\n\n}, this);\n")
  )

(defun closure-foreach-object ()
  "Create a closure object.forEach loop"
  (interactive)
  (closure-foreach "goog.object.forEach(%s, function() {\n\n});\n")
  )


(defun closure-foreach-object-this ()
  "Create a closure object.forEach loop with this binding"
  (interactive)
  (closure-foreach "goog.object.forEach(%s, function(item) {\n\n}, this);\n")
  )


(defun closure-abstract-function ()
  "inserts boiler plate for new abstract closure function"
  (interactive)
  (closure-prototype-prefix)
  (save-excursion
    (forward-char 2)
    (insert " goog.abstractMethod")
    (delete-char 1)
    (end-of-line)
    (insert "\n")))

(defun closure-prototype-prefix ()
  "Insert the classname with a '.prototype = '"
  (interactive)
  (closure-attribute-prefix)
  (insert "prototype.")
)

(defun closure-constructor-function ()
  "Insert constructor code"
  (interactive)
  (closure-attribute-prefix)
  (backward-delete-char 1); remove the period
  (setq cname (closure-class-name))
  (save-excursion
    (progn
      ;; add comment params
      (beginning-of-line)
      (backward-char)
      (beginning-of-line)
      (backward-char)
      (end-of-line)
      (insert (format "%s\n * @constructor\n * @extends {}" cname))
      )
    )
  (forward-char 3)
  (insert "function() {\n  goog.base(this);\n}")
  (forward-char 1)
  (insert (format "\ngoog.inherits(%s, );" cname))
  (backward-char 2) ;; set cursor to let user input super class
)

(defun closure-insert-current-class ()
  (interactive)
  (insert (closure-class-name))
  )

(defun closure-sort-require-lines ()
  "Sorts goog.require lines in Closure"
  (interactive)
  (beginning-of-buffer)
  (search-forward-regexp "^goog.require")
  (let ((beg) (end))
    (beginning-of-line)
    (setq beg (point))
    (while (looking-at "goog.require(") (forward-line 1))
    (setq end (point))
    (sort-lines nil beg end)))

(defun closure-new-function ()
  "inserts boiler plate for new closure function"
  (interactive)
  (closure-prototype-prefix)
  (save-excursion
    (forward-char 2)
    (insert " function() {\n\n}")
    (delete-char 1)
    (end-of-line)
    (insert "\n")))

(defun closure-super-function ()
  "Insert new function with goog.base"
  (interactive)
  (save-excursion
    (insert "/** @inheritDoc */\n")
    (insert (format "%s.prototype. = ;" (closure-class-name)))
    (backward-char 4)
    (forward-char 2)
    (insert " function() {\n  goog.base(this, '');\n}")
    (delete-char 1)
    (end-of-line)
    (insert "\n")
    )
  (search-forward "=")
  (backward-char 2)
  )

(defun closure-get-javadoc-params ()
  "Fetch all the @param names in a comment block"
  (interactive)
  (setq params '())
  (save-excursion
    (closure-search-function-declaration)
    (search-backward "/**")
    (forward-line)
     (while (looking-at " \* ")
       (progn
         (setq line (current-line))
         (message line)
         (if (string-match ".*@param {.*} \\([^ ]+\\)" line)
             (progn
               (push (match-string 1 line) params)))
         (forward-line 1))))
  params) ; return params

(defun closure-get-function-params ()
  "Return a list of all params in a function definition"
  (interactive)
  (save-excursion
    (closure-search-function-declaration)
    (search-forward "(")
    (setq start (point))
    (search-forward ")")
    (setq params (buffer-substring start (- (point) 1)))
    (setq param_list (split-string params ","))
    (message (format "Params: %s length: %s" params (length param_list)))
    (if (and (= (length param_list) 1) (= (length (nth 0 param_list)) 0) )
        '()
      (mapcar (lambda (x) (s-trim x)) param_list)
      )
    )
  ) ; return the split string

(defun closure-insert-javadoc-param (param)
  "Find the above javadoc, put the param at the end"
  (save-excursion
    (search-backward "*/")
    (beginning-of-line)
    (insert " * @param {")
    (if (string-match "opt_" param)
        (insert "="))
    (insert (format "} %s .\n" param))))

(defun closure-insert-function-params (params)
  "Find the above javadoc, put the param at the end"
  (save-excursion
    (closure-search-function-declaration)
    (if (string-match "()" (current-line))
        (progn
          (search-forward "(")
          (insert (mapconcat 'identity params ", "))
          )
      (progn
        (search-forward ")")
        (backward-char)
        (insert ", ")
        (insert (mapconcat 'identity params ", "))
        )
      )))

(defun closure-get-javadoc ()
  "Fetch the above javadoc comment"
  (save-excursion
    (search-backward "/**")
    (setq start (point))
    (search-forward "*/")
    (buffer-substring start (point))
    )
  )

(defun closure-add-return ()
  "Add a return @param if your current function has a return statement (Not reliable with anonymous functions...)"
  (interactive)
  (save-excursion
    (closure-search-function-declaration)
    (setq start (point))
    (search-forward-regexp "^};")
    (setq function_def (buffer-substring start (point)))
    (message (format "Function: %s" function_def))
    (if (string-match "return" function_def)
        (progn
          (setq comment (closure-get-javadoc))
          (if (not(string-match "@return" comment))
            (progn
              (closure-search-function-declaration)
              (search-backward "*/")
              (beginning-of-line)
              (insert " * @return {} .\n")))))))

(defun closure-add-private ()
  "Find the above javadoc and add a @private at the beginning"
  (interactive)
  (save-excursion
    (setq package (js-function-name))
    (message (format "Function Name: %s" package))
    (if (string-match "_$" package)
        (progn
          (setq comment (closure-get-javadoc))
          (closure-search-function-declaration)
          (search-backward "/**")
          (end-of-line)
          (if (not(string-match "@private" comment))
              (insert "\n * @private")
            )
          ))
    ))

(defun js-function-name ()
  "Return the above function name"
  (interactive)
  (save-excursion
    (closure-search-function-declaration)
    (beginning-of-line)
    (let (package)
      (if (search-forward-regexp "\\(\\([^ ]+\\) = \\)?function\\([^(]+\\)?(" nil t)
          (progn
            (setq package (match-string 3))
            (if (not package) (setq package (match-string 2)))
            (message (format "Package: %s" package))
            )
        )
      package
      )
    )
  )

(defun closure-insert-goog-base ()
  (interactive)
  (save-excursion
    (setq package (js-function-name))
    (setq namespace (reverse (split-string package "\\.")))
    (setq func_name (pop namespace))
    (message (format "Function Name: %s" func_name))
    (closure-search-function-declaration)
    (end-of-line)
    (insert "\n")
    (indent-according-to-mode)
    (insert "goog.base(this")
    (indent-according-to-mode)
    (if (string-match "prototype" package)
        (insert (format ", '%s'" func_name)))
    (setq params (closure-get-function-params))
    (if (> (length params) 0)
        (progn
          (insert (format ", "))
          (insert (mapconcat 'identity params ", ") )))
    (insert ");")))


(defun closure-update-javadoc ()
  (interactive)
  (save-excursion
    (closure-add-private)
    (setq doc_params (closure-get-javadoc-params))
    (setq params (closure-get-function-params))

    (setq not_there '())
    (loop for param in params do
                (progn
                 (if (> (length param) 0)
                  (progn
                   (message (format "Param: '%s'" param))
                   (if (member param doc_params)
                    (progn
                     (message (format "Mark %s found in both lists" param))
                     (setq doc_params (remove param doc_params))
                   )
                    (progn
                     (message (format "Mark %s not found in javadoc" param))
                     (push param not_there)
                   ))))))
    (message (format "Doc Params not found in function: %s" (reverse doc_params)))
    (closure-insert-function-params (reverse doc_params))
   (loop for param in (reverse not_there) do
    (progn
     (message (format "Param not found in Javadoc: %s" param))
     (closure-insert-javadoc-param param)
   ))
   (closure-add-return)
 )
)

(defun closure-match (expression)
  "Return the line and filename that matches the javascript expression."
  (interactive)
  (setq paths (mapconcat 'identity closure-projects " "))
  (setq command (format "grep -RHn --include \"*.js\" -e \"%s\" %s 2> /dev/null | awk -F\: '{print $1,$2}'" expression paths))
  ;; (message (format "Command: %s" command))
  (split-string (first (split-string (shell-command-to-string command) "\n")) " ")
  )

(defun closure-provides-where (module)
  "Return the line and filename where a goog.provide() was found"
  (interactive)
  (closure-match (format "^goog.provide('%s');$" module))
  )

(defun closure-declaration-where (object value)
  "Return the line and filename where value is found on object"
  (interactive)
  (closure-match (format "^%s(.prototype)?.%s( =|;)" object value))
  )

(defun closure-super-class (module)
  "Get current super class, if any"
  (interactive)  
  (if (string-match module (closure-class-name))
      (save-excursion
	(beginning-of-buffer)
	(if (search-forward-regexp " * @extends {\\(.*\\)}" nil t)
	    (match-string 1)
	  (if (search-forward-regexp "^goog.inherits(.*, \\(.*\\))" nil t)
	      (match-string 1)
	    )
	  )
	)
    (progn
      (setq file_info (closure-provides-where module))
      (setq file_name (pop file_info))
      ;; (message (format "File Name: %s" file_name))
      (setq command (format "grep -e \"^goog.inherits\(%s, \" %s | sed 's/.*, \\\(.*\\\));/\\1/' | tr -d '\\n'" module file_name))
      ;; (message (format "Command: %s, Filename: %s" command file_name))
      (shell-command-to-string command)
      )
    )
  )

(defun closure-super-jump ()
  (interactive)
  (setq curr_class (closure-class-name))
  (setq super_class (closure-super-class curr_class))
  (message (format "Curr: %s, Super: %s" curr_class super_class))
  (setq file_info (closure-provides-where super_class))
  ;; (message (format "FileInfo: %s" file_info))
  (if (> (length file_info) 1)
      (progn
        (setq filename (pop file_info))
        ;; (message (format "Filename: %s" file_name))
        (setq line (pop file_info))
        (find-file filename)
        (goto-line (string-to-int line))
        )
    (error "No super class found %s" curr_class)
    )
  )


(defun closure-current-expression ()
  "Expression is: a word - Ex - foo, a function call - Ex foo(), a namespace - a.b.c"
  (save-excursion
    (let (beg variable)
      (skip-chars-backward "A-Za-z0-9_.[]()\"'")
      (setq beg (1- (point)))
      (skip-chars-forward "A-Za-z0-9_.()[]\"'")
      (s-trim (buffer-substring beg (point))))))

(defun message-current-expression () 
  (interactive)
  (message (format "Expression: %s" (closure-current-expression)))
  )

(defun closure-current-module ()
  "Get the object path up to the capitalized module/class name: foo.bar.Baz, or a.b.c, but not foo.bar.Baz.prototype"
  (save-excursion
    (skip-chars-backward "A-Za-z0-9_.")

    (let ((start (point)) module curclass)
      (skip-chars-forward "a-z0-9_.")
      (skip-chars-forward "A-Za-z0-9_")
      (buffer-substring-no-properties start (point)))))


(defun closure-add-require-line (&optional module)
  "Add a require line to the current module, if the module exists..."
  (interactive)
  (if (not module)
      (setq module (closure-current-module)))
  (if (string-match (format "goog.require('%s');" module)
                    (buffer-substring (point-min) (point-max)))
      (message "The module already is provided.")
    (progn
      (setq file_info (closure-provides-where module))
      (if (> (length file_info) 1)
          (progn
            (save-excursion
              (end-of-buffer)
              (search-backward "goog.provide(")
              (next-line)
              (next-line)
              (beginning-of-line)
              (insert (format "goog.require('%s');\n" module))
              (closure-sort-require-lines)
              (message "Now using %s goodness" module)
              )
            )
        (error "File not found for module: %s" module)))))

(defun closure-update-extends ()
  "Find the first extends , add the use line, then change the inherits line"
  (interactive)
  (save-excursion
    (if (string-match "@extends {" (current-line))
        (beginning-of-line)
      (beginning-of-buffer))
    (if (search-forward "@extends {")
        (progn
          (setq extends (closure-current-module))
          (message "Found extends: %s" extends)
          (closure-add-require-line)
          (if (search-forward "goog.inherits" 'nil 't)
              (progn
                (search-forward ", ")
                (zap-to-char 1 41) ; zap to right curly brace
                (insert (format "%s)" extends)))
            (progn
              (message "insert inherits")
              (search-forward " = function(")
              (beginning-of-line)
              (setq definition (closure-current-module))
              (search-forward-regexp "^};")
              (next-line)
              (beginning-of-line)
              (insert (format "goog.inherits(%s, %s);\n" definition extends))
              ))))))


(defvar closure-projects)
(defun reload-closure-projects ()
  "Load the current list of projects."
  (interactive)
  (setq closure-projects (split-string (shell-command-to-string "cat ~/.emacs-closure-projects | grep -e '^$' -v") "\n"))
  )
(reload-closure-projects)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun in-closure-file ()
  (string-match "goog.provide" (buffer-string)))

(define-prefix-command 'closure-map)
(global-set-key (kbd "s-c") 'closure-map)


(defun closure-set-keybindings ()
 (local-set-key [(super c) ?a ?f] 'closure-abstract-function)
 (local-set-key [(super c) ?a ?r] 'closure-add-require-line)
 (local-set-key [(super c) ?c ?c] 'closure-insert-current-class)
 (local-set-key [(super c) ?c ?f] 'closure-constructor-function)
 (local-set-key [(super c) ?g ?b] 'closure-insert-goog-base)
 (local-set-key [(super c) ?n ?a] 'closure-attribute-prefix)
 (local-set-key [(super c) ?n ?f] 'closure-new-function)
 (local-set-key [(super c) ?s ?c] 'closure-super-jump)
 (local-set-key [(super c) ?s ?f] 'closure-super-function)
 (local-set-key [(super c) ?s ?g] 'closure-insert-singleton-getter)
 (local-set-key [(super c) ?s ?r] 'closure-sort-require-lines)
 (local-set-key [(super c) ?u ?e] 'closure-update-extends)
 (local-set-key [(super c) ?u ?j] 'closure-update-javadoc)

 ; for-loop utilities
 (local-set-key [(super c) ?a ?e] 'closure-foreach-array)
 (local-set-key [(super c) ?a ?t] 'closure-foreach-array-this)
 (local-set-key [(super c) ?o ?e] 'closure-foreach-object)
 (local-set-key [(super c) ?o ?t] 'closure-foreach-object-this)
 )

;; javascript mode
(add-hook 'js2-mode-hook
          '(lambda ()
            (local-set-key [(return)] 'newline-and-indent)
            (when (in-closure-file)) (closure-set-keybindings)))

