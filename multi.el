;;; multi.el --- multimethods for Emacs Lisp

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

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

;; Inheritance functions

(defun multi-derive (symbol parent)
  "Derive a parent-child relationship from PARENT to SYMBOL."
  (cl-pushnew parent (get symbol :multi-parents)))

(defun multi-parents (symbol)
  "Return a list of parents of SYMBOL."
  (get symbol :multi-parents))

(defun multi-ancestors (symbol)
  "Return a list of ancestors of SYMBOL."
  (let ((parents (multi-parents symbol)))
    (cl-remove-duplicates
     (apply #'append parents (mapcar #'multi-ancestors parents)))))

(defun multi-isa-p (child parent)
  "Return non-nil if CHILD is a descendant of PARENT."
  (or (eq child parent)
      (let ((parents (multi-parents child)))
        (or (memq child parents)
            (cl-some (lambda (s) (multi-isa-p s parent)) parents)))))

(defun multi--every (p a b)
  "Like `cl-every' but handle improper lists and mismatched lengths."
  (or (and (null a) (null b))
      (if (not (listp a))
          (funcall p a b)
        (unless (or (null a) (null b))
          (and (funcall p (car a) (car b))
               (multi--every p (cdr a) (cdr b)))))))

(defun multi-equal (a b)
  "Compare A and B like `equal' but accounting for inheritance."
  (when (eq (type-of a) (type-of b))
    (cl-typecase a
      (symbol (multi-isa-p a b))
      (string (string= a b))
      (list (multi--every #'multi-equal a b))
      (sequence (and (= (length a) (length b))
                     (cl-every #'multi-equal a b)))
      (t (equal a b)))))

;; Multimethods

(defun multi-dispatch (multimethod)
  "Get the dispatch function for MULTIMETHOD."
  (get multimethod :multi-dispatch))

(gv-define-setter multi-dispatch (dispatch-function multimethod)
  (setf (get multimethod :multi-dispatch) dispatch-function))

(defun multi-methods (multimethod)
  "Return the methods for MULTIMETHOD."
  (get multimethod :multi-methods))

(defun multi-lookup (multimethod value)
  "Return the method to use for VALUE in MULTIMETHOD."
  (let ((methods (multi-methods multimethod)))
    (or (cdr (cl-assoc value methods :test #'multi-equal))
        (get multimethod :multi-default))))

(defun multi--funcall (multimethod args)
  "Run the method for MULTIMETHOD with ARGS."
  (let* ((dispatch (multi-dispatch multimethod))
         (value (apply dispatch args))
         (method (multi-lookup multimethod value)))
    (if method
        (apply method args)
      (error "No method found in %S for %S" multimethod value))))

(defmacro multi-defmulti (name dispatch-fn &optional docstring)
  "Define a new multimethod as NAME dispatching on DISPATCH-FN."
  (declare (indent 2))
  `(progn
     (setf (get ',name :multi-dispatch) ,dispatch-fn
           (get ',name :multi-methods) ()
           (get ',name :multi-default) nil)
     (defun ,name (&rest args)
       ,docstring
       (multi--funcall ',name args))))

(defun multi-add-method (multimethod value function)
  "Declare FUNCTION as a method in MULTIMETHOD for VALUE."
  (prog1 (list multimethod value)
    (if (eq value :default)
        (setf (get multimethod :multi-default) function)
      (push (cons value function) (get multimethod :multi-methods)))))

(defmacro multi-defmethod (name dispatch-value args &rest body)
  "Define a new method in multimethod NAME for DISPATCH-VALUE."
  (declare (indent 3))
  `(multi-add-method ',name ,dispatch-value (lambda ,args ,@body)))

(let ((macros (regexp-opt '("multi-defmulti" "multi-defmethod") t)))
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(" macros "\\_>" "[ \t]*" "\\(\\sw+\\)?")
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(provide 'multi)

;;; multi.el ends here
