(provide 'closure-emacs)

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
      (buffer-substring beg end))))

(defun closure-class-comment ()
  "Insert a class comment for an attribute"
  (insert "/**\n * \n */\n")
  )

(defun closure-attribute-prefix ()
  "Insert a classname with fully qualified path"
  (interactive)
  (closure-class-comment)
  (insert (format "%s. = ;" (closure-class-name)))
  (backward-char 4)
)

(defun closure-prototype-prefix ()
  "Insert the classname with a '.prototype = '"
  (interactive)
  (closure-attribute-prefix)
  (insert "prototype.")
)

(defun closure-new-method ()
  "inserts boiler plate for new closure method"
  (interactive)
  (closure-prototype-prefix)
  (save-excursion
    (forward-char 2)
    (insert " function() {\n\n}")
    (delete-char 1)
    (end-of-line)
    (insert "\n")))

(defun closure-abstract-function ()
  "inserts boiler plate for new abstract closure method"
  (interactive)
  (closure-prototype-prefix)
  (save-excursion
    (forward-char 2)
    (insert " goog.abstractMethod")
    (delete-char 1)
    (end-of-line)
    (insert "\n")))


(defun closure-super-method ()
  "Insert new method with goog.base"
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

(defun closure-constructor-method ()
  "Insert constructor code"
  (interactive)
  (closure-attribute-prefix)
  (backward-delete-char 1); remove the period
  (setq cname (closure-class-name))
  (save-excursion
    (progn
      ;; add comment params
      (search-backward "\n")
      (search-backward "\n")
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


