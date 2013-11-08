(provide 'closure-emacs)

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
  (setq variable (current-variable))
  (end-of-line)
  (insert "\n")
  (insert (format for-format variable))
  (search-backward-regexp "forEach")
  (beginning-of-line)
  (indent-according-to-mode)
  (next-line)
  (indent-according-to-mode)
  (next-line)
  (indent-according-to-mode)
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


(defun closure-sort-require-lines ()
  "Sorts import lines for actionscript files"
  (interactive)
  (beginning-of-buffer)
  (search-forward-regexp "^goog.require")
  (let ((beg) (end))
    (beginning-of-line)
    (setq beg (point))
    (while (looking-at "goog.require") (forward-line 1))
    (setq end (point))
    (sort-lines nil beg end)))

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
      (backward-line 2)
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

(defun closure-sort-requires-lines ()
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
    (search-backward-regexp " \*/\n\\(.* =\\) function(")
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

(defun closure-get-function-params ()
  "Return a list of all params in a function definition"
  (interactive)
  (save-excursion
    (search-backward-regexp " \*/\n\\(.* =\\) function(")
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
    (search-backward " */")
    (beginning-of-line)
    (insert " * @param {")
    (if (string-match "opt_" param)
        (insert "="))
    (insert (format "} %s .\n" param))))

(defun closure-insert-function-param (param)
  "Find the above javadoc, put the param at the end"
  (save-excursion
    (search-backward-regexp " \*/\n\\(.* =\\) function(")
    (if (string-match "function()" (current-line))
        (progn
          (search-forward "function(")
          (insert param)
          )
      (progn
        (search-forward ")")
        (backward-char)
        (insert (format ", %s" param))
        )
      )))

(defun closure-get-javadoc ()
  "Fetch the above javadoc comment"
  (save-excursion
    (search-backward "/**")
    (setq start (point))
    (search-forward " */")
    (buffer-substring start (point))
    )
  )

(defun closure-add-return ()
  "Add a return @param if your current function has a return statement (Not reliable with anonymous functions...)"
  (save-excursion
    (search-backward-regexp " \*/\n\\(.* =\\) function(")
    (setq start (point))
    (search-forward-regexp "^};")
    (setq function_def (buffer-substring start (point)))
    (message (format "Function: %s" function_def))
    (if (string-match "return" function_def)
        (progn
          (setq comment (closure-get-javadoc))
          (if (not(string-match "@return" comment))
              (progn
                (search-backward " */")
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
          (search-backward-regexp " \*/\n\\(.* =\\) function(")
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
    (search-backward-regexp " \*/\n.* = function")
    (next-line)
    (beginning-of-line)
    (search-forward-regexp "\\(.*\\) = function")
    (setq package (match-string 1))
    (message (format "Package: %s" package))
    package
    )
  )

(defun closure-insert-goog-base ()
  (interactive)
  (save-excursion
    (setq package (js-function-name))
    (setq namespace (reverse (split-string package "\\.")))
    (setq func_name (pop namespace))
    (message (format "Function Name: %s" func_name))
    (search-backward-regexp "\\(.*\\) = function")
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
          (insert (list-join params ", ") )))
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
   (loop for param in (reverse doc_params) do
    (progn
     (message (format "Doc Param not found in function: %s" param))
     (closure-insert-function-param param)
   ))
   (loop for param in (reverse not_there) do
    (progn
     (message (format "Param not found in Javadoc: %s" param))
     (closure-insert-javadoc-param param)
   ))
   (closure-add-return)
 )
)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun in-closure-file ()
  (string-match "goog.provide" (buffer-string)))

(define-prefix-command 'closure-map)
(global-set-key (kbd "s-c") 'closure-map)


(defun closure-set-keybindings ()
 (local-set-key [(super c) ?a ?f] 'closure-abstract-function)
 (local-set-key [(super c) ?c ?c] 'closure-insert-current-class)
 (local-set-key [(super c) ?g ?b] 'closure-insert-goog-base)
 (local-set-key [(super c) ?n ?a] 'closure-attribute-prefix)
 (local-set-key [(super c) ?n ?f] 'closure-new-function)
 (local-set-key [(super c) ?s ?f] 'closure-super-function)
 (local-set-key [(super c) ?s ?g] 'closure-insert-singleton-getter)
 (local-set-key [(super c) ?s ?r] 'closure-sort-requires-lines)
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

