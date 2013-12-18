;;; multi.el --- Clojure-style multimethods -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/multi
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.3") (emacs "24.1"))

;;; Commentary:

;; This library provides Clojure-style multimethods for Emacs Lisp.
;; http://clojure.org/multimethods

;; First declare a multimethod dispatching with `identity'.

;;    (multi-defmulti foo #'identity)

;; Define two methods for `foo'. A value of :default is special.

;;    (multi-defmethod foo :default (a)
;;      :discard)

;;    (multi-defmethod foo :food (a)
;;      :bite)

;; Declare that apples are a kind of food, establishing the beginnings
;; of an ad hoc type hierarchy.

;;    (multi-derive :apple :food)

;; The results:

;;    (foo :food)   ; => :bite
;;    (foo :apple)  ; => :bite
;;    (foo :pencil) ; => :discard

;; Here's another example, showing that dispatch values can be
;; arbitrarily complex structures. First define some animal interactions:

;;    (multi-defmulti show #'list "Show an animal to another animal.")

;;    (multi-defmethod show '(:cat :mouse) (a b)
;;      :chase)

;;    (multi-defmethod show '(:cat :dog) (a b)
;;      :flee)

;;    (multi-defmethod show '(:dog :cat) (a b)
;;      :bark)

;; The results are as expected:

;;    (show :cat :dog)    ; => :flee
;;    (show :cat :mouse)  ; => :chase

;; Now teach the system that a tabby is a cat.

;;    (multi-derive :tabby :cat)
;;    (show :dog :tabby)  ; => :bark

;;; Code:

(require 'cl-lib)

;; Hierarchy functions

(cl-defstruct (multi-hierarchy (:constructor multi-make-hierarchy))
  (parents     (make-hash-table :test 'eq) :read-only t)
  (defaults    (make-hash-table :test 'eq) :read-only t)
  (classifiers (make-hash-table :test 'eq) :read-only t)
  (methods     (make-hash-table :test 'eq) :read-only t)
  (-cache      (make-hash-table :test 'equal :weakness 'value)))

(defvar multi-hierarchy (multi-make-hierarchy)
  "Global multimethods hierarchy.")

(defun multi--clear-dispatch-cache ()
  "Reset the dispatch cache in the global hierarchy."
  (clrhash (multi-hierarchy--cache multi-hierarchy)))

(defun multi-derive (symbol parent)
  "Derive a parent-child relationship from PARENT to SYMBOL."
  (multi--clear-dispatch-cache)
  (let ((table (multi-hierarchy-parents multi-hierarchy)))
    (cl-pushnew parent (gethash symbol table))))

(defun multi-parents (symbol)
  "Return a list of parents of SYMBOL."
  (gethash symbol (multi-hierarchy-parents multi-hierarchy)))

(gv-define-setter multi-parents (parents symbol)
  `(let ((table (multi-hierarchy-parents multi-hierarchy)))
     (multi--clear-dispatch-cache)
     (setf (gethash ,symbol table) ,parents)))

(defun multi-ancestors (symbol)
  "Return a list of ancestors of SYMBOL."
  (let ((parents (multi-parents symbol)))
    (cl-remove-duplicates
     (apply #'append parents (mapcar #'multi-ancestors parents)))))

(cl-defun multi-distance (child parent &optional (base-distance 0) stack)
  "Return distance if CHILD is derived, directly or indirectly, from PARENT.
Otherwise return nil."
  (cond ((eq child parent)  base-distance)
        ((memq child stack) nil)
        ((let ((parents (multi-parents child))
               (next-distance (1+ base-distance)))
           (if (memq child parents)
               next-distance
             (cl-loop with next-stack = (cons child stack)
                      for next in parents
                      for distance =
                      (multi-distance next parent next-distance next-stack)
                      when distance minimize it))))))

(defun multi--list-every (p a b)
  "Like `cl-every' but handle improper lists and mismatched lengths."
  (or (and (and (null a) (null b)) 0)
      (if (not (listp a))
          (funcall p a b)
        (unless (or (null a) (null b))
          (let ((result (funcall p (car a) (car b))))
            (and result
                 (+ result (multi--list-every p (cdr a) (cdr b)))))))))

(defun multi--seq-every (p a b)
  "Like `cl-every' but sum the results."
  (unless (not (= (length a) (length b)))
    (cl-loop for a-element across a
             for b-element across b
             when (funcall p a-element b-element) sum it
             else return nil)))

(defun multi-isa-p (a b)
  "Return non-nil if value A is dervied, directly or indirectly, from value B.
The return value is a distance metric from A to B."
  (when (eq (type-of a) (type-of b))
    (cl-typecase a
      (symbol (multi-distance a b))
      (string (and (string= a b) 0))
      (list (multi--list-every #'multi-isa-p a b))
      (sequence (multi--seq-every #'multi-isa-p a b))
      (t (equal a b)))))

;; Multimethods

(defun multi-classifier (multimethod)
  "Get the classifier function for MULTIMETHOD."
  (gethash multimethod (multi-hierarchy-classifiers multi-hierarchy)))

(gv-define-setter multi-classifier (classifier-function multimethod)
  `(let ((table (multi-hierarchy-classifiers multi-hierarchy)))
     (setf (gethash ,multimethod table) ,classifier-function)))

(defun multi-methods (multimethod)
  "Return the methods for MULTIMETHOD."
  (gethash multimethod (multi-hierarchy-methods multi-hierarchy)))

(gv-define-setter multi-methods (methods multimethod)
  `(let ((table (multi-hierarchy-methods multi-hierarchy)))
     (multi--clear-dispatch-cache)
     (setf (gethash ,multimethod table) ,methods)))

(defun multi-default (multimethod)
  "Get the default method for MULTIMETHOD."
  (gethash multimethod (multi-hierarchy-defaults multi-hierarchy)))

(gv-define-setter multi-default (method multimethod)
  `(let ((table (multi-hierarchy-defaults multi-hierarchy)))
     (multi--clear-dispatch-cache)
     (setf (gethash ,multimethod table) ,method)))

(defun multi-lookup (multimethod value)
  "Return an alist of equally-preferred methods for VALUE in MULTIMETHOD."
  (let* ((key (cons multimethod value))
         (cache (multi-hierarchy--cache multi-hierarchy))
         (cached (gethash key cache)))
    (if cached
        cached
      (cl-loop with methods = (multi-methods multimethod)
               with default = (multi-default multimethod)
               with best = nil
               with best-methods = (if default (cl-acons nil default ()) ())
               for (dispatch-value . method) in methods
               for score = (multi-isa-p value dispatch-value)
               when (and score (or (null best) (> score best)))
               do (setf best score
                        best-methods (cl-acons dispatch-value method ()))
               else when (and score (= best score))
               do (push (cons dispatch-value method) best-methods)
               finally (return (setf (gethash key cache) best-methods))))))

(defun multi--funcall (multimethod args)
  "Run the method for MULTIMETHOD with ARGS."
  (let* ((classifier (multi-classifier multimethod))
         (value (apply classifier args))
         (methods (multi-lookup multimethod value)))
    (cl-case (length methods)
      (0 (error "No method found in %S for %S" multimethod value))
      (1 (apply (cdr (car methods)) args))
      (otherwise (error "no preferred dispatch in %S: %S -> %S"
                        multimethod value (mapcar #'car methods))))))

(defmacro multi-defmulti (name classifier-function &optional docstring)
  "Define a new multimethod as NAME dispatching on CLASSIFIER-FUNCTION."
  (declare (indent 2))
  `(progn
     (multi--clear-dispatch-cache)
     (setf (multi-classifier ',name) ,classifier-function
           (multi-methods    ',name) ()
           (multi-default    ',name) nil)
     (defun ,name (&rest args)
       ,(format "%s\n\nThis function is a multimethod."
                (or docstring "Not documented."))
       (multi--funcall ',name args))))

(defun multi-add-method (multimethod value function)
  "Declare FUNCTION as a method in MULTIMETHOD for VALUE."
  (prog1 (list multimethod value)
    (multi--clear-dispatch-cache)
    (if (eq value :default)
        (setf (multi-default multimethod) function)
      (push (cons value function) (multi-methods multimethod)))))

(defmacro multi-defmethod (name dispatch-value args &rest body)
  "Define a new method in multimethod NAME for DISPATCH-VALUE."
  (declare (indent 3))
  `(multi-add-method ',name ,dispatch-value
                     (cl-function (lambda ,args ,@body))))

(let ((macros (regexp-opt '("multi-defmulti" "multi-defmethod") t)))
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(" macros "\\_>" "[ \t]*" "\\(\\sw+\\)?")
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(provide 'multi)

;;; multi.el ends here
