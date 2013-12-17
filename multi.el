;;; multi.el --- Clojure-style multimethods, multiple dispatch

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

(defvar multi--cache (make-hash-table :test 'equal :weakness 'value)
  "Table used for faster multimethod dispatching.")

(defvar multi--cache-counter 0
  "Increments any time inheritance changes, wiping the cache.")

;; Inheritance functions

(defun multi-derive (symbol parent)
  "Derive a parent-child relationship from PARENT to SYMBOL."
  (cl-incf multi--cache-counter)
  (cl-pushnew parent (get symbol :multi-parents)))

(defun multi-parents (symbol)
  "Return a list of parents of SYMBOL."
  (get symbol :multi-parents))

(gv-define-setter multi-parents (parents symbol)
  `(progn
     (cl-incf multi--cache-counter)
     (setf (get ,symbol :multi-parents) ,parents)))

(defun multi-ancestors (symbol)
  "Return a list of ancestors of SYMBOL."
  (let ((parents (multi-parents symbol)))
    (cl-remove-duplicates
     (apply #'append parents (mapcar #'multi-ancestors parents)))))

(cl-defun multi-isa-p (child parent &optional (base-distance 0))
  "Return non-nil if CHILD is a descendant of PARENT.
The return value is an integer distance metric."
  (if (eq child parent)
      base-distance
    (let ((parents (multi-parents child))
          (next-distance (1+ base-distance)))
      (if (memq child parents)
          next-distance
        (cl-some (lambda (s) (multi-isa-p s parent next-distance)) parents)))))

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

(defun multi-equal (a b)
  "Compare A and B like `equal' but accounting for inheritance.
Returns nil for no match, otherwise an integer distance metric."
  (when (eq (type-of a) (type-of b))
    (cl-typecase a
      (symbol (multi-isa-p a b))
      (string (or (string= a b) 0))
      (list (multi--list-every #'multi-equal a b))
      (sequence (multi--seq-every #'multi-equal a b))
      (t (equal a b)))))

;; Multimethods

(defun multi-dispatch (multimethod)
  "Get the dispatch function for MULTIMETHOD."
  (get multimethod :multi-dispatch))

(gv-define-setter multi-dispatch (dispatch-function multimethod)
  `(setf (get ,multimethod :multi-dispatch) ,dispatch-function))

(defun multi-methods (multimethod)
  "Return the methods for MULTIMETHOD."
  (get multimethod :multi-methods))

(defun multi-lookup (multimethod value)
  "Return an alist of equally-preferred methods for VALUE in MULTIMETHOD."
  (let* ((key (list multi--cache-counter multimethod value))
         (cached (gethash key multi--cache)))
    (if cached
        cached
      (cl-loop with methods = (multi-methods multimethod)
               with default = (get multimethod :multi-default)
               with best = nil
               with best-methods = (if default (cl-acons nil default ()) ())
               for (dispatch-value . method) in methods
               for score = (multi-equal value dispatch-value)
               when (and score (or (null best) (> score best)))
               do (setf best score
                        best-methods (cl-acons dispatch-value method ()))
               else when (and score (= best score))
               do (push (cons dispatch-value method) best-methods)
               finally (return
                        (setf (gethash key multi--cache) best-methods))))))

(defun multi--funcall (multimethod args)
  "Run the method for MULTIMETHOD with ARGS."
  (let* ((dispatch (multi-dispatch multimethod))
         (value (apply dispatch args))
         (methods (multi-lookup multimethod value)))
    (cl-case (length methods)
      (0 (error "No method found in %S for %S" multimethod value))
      (1 (apply (cdr (car methods)) args))
      (otherwise (error "no preferred dispatch in %S: %S -> %S"
                        multimethod value (mapcar #'car methods))))))

(defmacro multi-defmulti (name dispatch-fn &optional docstring)
  "Define a new multimethod as NAME dispatching on DISPATCH-FN."
  (declare (indent 2))
  `(progn
     (cl-incf multi--cache-counter)
     (setf (get ',name :multi-dispatch) ,dispatch-fn
           (get ',name :multi-methods) ()
           (get ',name :multi-default) nil)
     (defun ,name (&rest args)
       ,(format "%s\n\nThis function is a multimethod."
                (or docstring "Not documented."))
       (multi--funcall ',name args))))

(defun multi-add-method (multimethod value function)
  "Declare FUNCTION as a method in MULTIMETHOD for VALUE."
  (prog1 (list multimethod value)
    (cl-incf multi--cache-counter)
    (if (eq value :default)
        (setf (get multimethod :multi-default) function)
      (push (cons value function) (get multimethod :multi-methods)))))

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
