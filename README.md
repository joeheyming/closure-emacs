closure-emacs
=============

Emacs keybindings for programming google closure.


 - The key bindings are as follows:
  - super c a-f -> closure-abstract-function: 
    - insert an abstract function at the cursor
  - super c a-r -> closure-add-require-line
    - Look at the current module under the cursor, and try to add it to the list of require lines at the top of the file.
  - super c c-c -> closure-insert-current-class
    - Insert the current class name at the cursor.
  - super c c-f -> closure-constructor-function
    - Insert the template for a constructor method based on the current module.
  - super c g-b -> closure-insert-goog-base
    - Insert goog.base. with the correct parameters at the top of the current function you are in.
  - super c n-a -> closure-attribute-prefix
    - Insert a new attribute at the cursor for the current module.
  - super c n-f -> closure-new-function
    - Insert a new function at the cursor for the current module.
  - super c s-c -> closure-super-jump
    - Go to the current super class. 
  - super c s-f -> closure-super-function
    - Insert a new function at the cursor, but add /** inheritDoc */ and goog.base
  - super c s-g -> closure-insert-singleton-getter
    - Insert the singleton getter function at the cursor.
  - super c s-r -> closure-sort-require-lines
   - Sort the require lines in the current module.
  - super c u-e -> closure-update-extends
   - If you change @extends, run this function and it will update goog.inherits as well as add the require line.
  - super c u-j -> closure-update-javadoc
   - Update the javadoc with the current function parameters (if changed) and add @private or @return if needed.
  - super c a-e -> closure-foreach-array
   - Insert a goog.array.forEach at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
  - super c a-t -> closure-foreach-array-this
   - Insert a goog.array.forEach (bound to this) at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
  - super c o-e -> closure-foreach-object
   - Insert a goog.object.forEach at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
  - super c o-t -> closure-foreach-object-this
   - Insert a goog.object.forEach (bound to this) at the cursor.  If an appropriate expression is found under the cursor, we try to use it as the first argument to the forEach function.
