;;; predd.el --- Clojure-style multimethods (predicate dispatch) -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/predd
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.3") (emacs "24.1"))

;;; Commentary:

;; This library provides Clojure-style multimethods for Emacs Lisp.
;; http://clojure.org/multimethods

;; First declare a multimethod dispatching with `identity'.

;;    (predd-defmulti foo #'identity)

;; Define two methods for `foo'. A value of :default is special.

;;    (predd-defmethod foo :default (a)
;;      :discard)

;;    (predd-defmethod foo :food (a)
;;      :bite)

;; Declare that apples are a kind of food, establishing the beginnings
;; of an ad hoc type hierarchy.

;;    (predd-derive :apple :food)

;; The results:

;;    (foo :food)   ; => :bite
;;    (foo :apple)  ; => :bite
;;    (foo :pencil) ; => :discard

;; Here's another example, showing that dispatch values can be
;; arbitrarily complex structures. First define some animal interactions:

;;    (predd-defmulti show #'list "Show an animal to another animal.")

;;    (predd-defmethod show '(:cat :mouse) (a b)
;;      :chase)

;;    (predd-defmethod show '(:cat :dog) (a b)
;;      :flee)

;;    (predd-defmethod show '(:dog :cat) (a b)
;;      :bark)

;; The results are as expected:

;;    (show :cat :dog)    ; => :flee
;;    (show :cat :mouse)  ; => :chase

;; Now teach the system that a tabby is a cat.

;;    (predd-derive :tabby :cat)
;;    (show :dog :tabby)  ; => :bark

;;; Code:

(require 'cl-lib)

;; Hierarchy functions

(cl-defstruct (predd-hierarchy (:constructor predd-make-hierarchy))
  (parents     (make-hash-table :test 'eq) :read-only t)
  (defaults    (make-hash-table :test 'eq) :read-only t)
  (classifiers (make-hash-table :test 'eq) :read-only t)
  (methods     (make-hash-table :test 'eq) :read-only t)
  (-cache      (make-hash-table :test 'equal :weakness 'value)))

(defvar predd-hierarchy (predd-make-hierarchy)
  "Global multimethods hierarchy.")

(defun predd--clear-dispatch-cache ()
  "Reset the dispatch cache in the global hierarchy."
  (clrhash (predd-hierarchy--cache predd-hierarchy)))

(defun predd-derive (symbol parent)
  "Derive a parent-child relationship from PARENT to SYMBOL."
  (predd--clear-dispatch-cache)
  (let ((table (predd-hierarchy-parents predd-hierarchy)))
    (cl-pushnew parent (gethash symbol table))))

(defun predd-parents (symbol)
  "Return a list of parents of SYMBOL."
  (gethash symbol (predd-hierarchy-parents predd-hierarchy)))

(gv-define-setter predd-parents (parents symbol)
  `(let ((table (predd-hierarchy-parents predd-hierarchy)))
     (predd--clear-dispatch-cache)
     (setf (gethash ,symbol table) ,parents)))

(defun predd-ancestors (symbol)
  "Return a list of ancestors of SYMBOL."
  (let ((parents (predd-parents symbol)))
    (cl-remove-duplicates
     (apply #'append parents (mapcar #'predd-ancestors parents)))))

(cl-defun predd-distance (child parent &optional (base-distance 0) stack)
  "Return distance if CHILD is derived, directly or indirectly, from PARENT.
Otherwise return nil."
  (cond ((eq child parent)  base-distance)
        ((memq child stack) nil)
        ((let ((parents (predd-parents child))
               (next-distance (1+ base-distance)))
           (if (memq child parents)
               next-distance
             (cl-loop with next-stack = (cons child stack)
                      for next in parents
                      for distance =
                      (predd-distance next parent next-distance next-stack)
                      when distance minimize it))))))

(defun predd--list-every (p a b)
  "Like `cl-every' but handle improper lists and mismatched lengths."
  (or (and (and (null a) (null b)) 0)
      (if (not (listp a))
          (funcall p a b)
        (unless (or (null a) (null b))
          (let* ((result (funcall p (car a) (car b)))
                 (next-result (when result
                                (predd--list-every p (cdr a) (cdr b)))))
            (if (and result next-result)
                (+ result next-result)))))))

(defun predd--seq-every (p a b)
  "Like `cl-every' but sum the results."
  (unless (not (= (length a) (length b)))
    (cl-loop for a-element across a
             for b-element across b
             when (funcall p a-element b-element) sum it
             else return nil)))

(defun predd-isa-p (a b)
  "Return non-nil if value A is dervied, directly or indirectly, from value B.
The return value is a distance metric from A to B."
  (when (eq (type-of a) (type-of b))
    (cl-typecase a
      (symbol (predd-distance a b))
      (string (and (string= a b) 0))
      (list (predd--list-every #'predd-isa-p a b))
      (sequence (predd--seq-every #'predd-isa-p a b))
      (t (equal a b)))))

;; Multimethods

(defun predd-classifier (multimethod)
  "Get the classifier function for MULTIMETHOD."
  (gethash multimethod (predd-hierarchy-classifiers predd-hierarchy)))

(gv-define-setter predd-classifier (classifier-function multimethod)
  `(let ((table (predd-hierarchy-classifiers predd-hierarchy)))
     (setf (gethash ,multimethod table) ,classifier-function)))

(defun predd-methods (multimethod)
  "Return the methods for MULTIMETHOD."
  (gethash multimethod (predd-hierarchy-methods predd-hierarchy)))

(gv-define-setter predd-methods (methods multimethod)
  `(let ((table (predd-hierarchy-methods predd-hierarchy)))
     (predd--clear-dispatch-cache)
     (setf (gethash ,multimethod table) ,methods)))

(defun predd-default (multimethod)
  "Get the default method for MULTIMETHOD."
  (gethash multimethod (predd-hierarchy-defaults predd-hierarchy)))

(gv-define-setter predd-default (method multimethod)
  `(let ((table (predd-hierarchy-defaults predd-hierarchy)))
     (predd--clear-dispatch-cache)
     (setf (gethash ,multimethod table) ,method)))

(defun predd-lookup (multimethod value)
  "Return an alist of equally-preferred methods for VALUE in MULTIMETHOD."
  (let* ((key (cons multimethod value))
         (cache (predd-hierarchy--cache predd-hierarchy))
         (cached (gethash key cache)))
    (if cached
        cached
      (cl-loop with methods = (predd-methods multimethod)
               with default = (predd-default multimethod)
               with best = nil
               with best-methods = (if default (cl-acons nil default ()) ())
               for (dispatch-value . method) in methods
               for score = (predd-isa-p value dispatch-value)
               when (and score (or (null best) (< score best)))
               do (setf best score
                        best-methods (cl-acons dispatch-value method ()))
               else when (and score (= best score))
               do (push (cons dispatch-value method) best-methods)
               finally (return (setf (gethash key cache) best-methods))))))

(defun predd--funcall (multimethod args)
  "Run the method for MULTIMETHOD with ARGS."
  (let* ((classifier (predd-classifier multimethod))
         (value (apply classifier args))
         (methods (predd-lookup multimethod value)))
    (cl-case (length methods)
      (0 (error "No method found in %S for %S" multimethod value))
      (1 (apply (cdr (car methods)) args))
      (otherwise (error "no preferred dispatch in %S: %S -> %S"
                        multimethod value (mapcar #'car methods))))))

(defmacro predd-defmulti (name classifier-function &optional docstring)
  "Define a new multimethod as NAME dispatching on CLASSIFIER-FUNCTION."
  (declare (indent 2))
  `(progn
     (predd--clear-dispatch-cache)
     (setf (predd-classifier ',name) ,classifier-function
           (predd-methods    ',name) ()
           (predd-default    ',name) nil)
     (defun ,name (&rest args)
       ,(format "%s\n\nThis function is a multimethod."
                (or docstring "Not documented."))
       (predd--funcall ',name args))))

(defun predd-add-method (multimethod value function)
  "Declare FUNCTION as a method in MULTIMETHOD for VALUE."
  (prog1 (list multimethod value)
    (predd--clear-dispatch-cache)
    (if (eq value :default)
        (setf (predd-default multimethod) function)
      (let ((previous (cl-assoc value (predd-methods multimethod)
                                :test #'equal)))
        (if previous
            (setf (cdr previous) function)
          (push (cons value function) (predd-methods multimethod)))))))

(defmacro predd-defmethod (name dispatch-value args &rest body)
  "Define a new method in multimethod NAME for DISPATCH-VALUE."
  (declare (indent 3))
  `(predd-add-method ',name ,dispatch-value
                     (cl-function (lambda ,args ,@body))))

(let ((macros (regexp-opt '("predd-defmulti" "predd-defmethod") t)))
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(" macros "\\_>" "[ \t]*" "\\(\\sw+\\)?")
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(provide 'predd)

;;; multi.el ends here
