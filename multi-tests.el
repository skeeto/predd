;;; multi-tests.el -- tests for multimethods library

(require 'ert)
(require 'cl-lib)
(require 'multi)

(defmacro multi--symbol-function (symbol)
  "Like `symbol-function' but return nil if unbound."
  `(if (fboundp ,symbol) (symbol-function ,symbol) nil))

(gv-define-setter multi--symbol-function (store symbol)
  `(if ,store (fmakunbound ,symbol) (fset ,symbol ,store)))

(defmacro multi-save-symbols (symbols &rest body)
  "Run BODY, preserving SYMBOLS plists, value bindings, and function bindings."
  (declare (indent 1))
  `(cl-letf ,(cl-loop for symbol in symbols
                      unless (keywordp symbol)
                      collect `(,symbol nil)
                      collect `((multi--symbol-function ',symbol) nil)
                      collect `((symbol-plist ',symbol) ()))
     ,@(cl-loop for symbol in symbols
                collect `(declare-function ,symbol nil))
     ,@body))

(ert-deftest multi-inheritance ()
  (multi-save-symbols (:fruit :apple :gala :carrot)
    (multi-derive :apple :fruit)
    (multi-derive :gala :apple)
    (should (equal '(:fruit) (multi-parents :apple)))
    (should (equal '(:apple :fruit) (multi-ancestors :gala)))
    (should (= 0 (multi-isa-p :fruit :fruit)))
    (should (= 1 (multi-isa-p :apple :fruit)))
    (should (= 2 (multi-isa-p :gala :fruit)))
    (should-not (multi-isa-p :fruit :apple))
    (should-not (multi-isa-p :carrot :fruit))
    (should (= 1 (multi-equal :apple :fruit)))
    (should (= 2 (multi-equal '(:gala) '(:fruit))))
    (should (= 3 (multi-equal '(:gala . :apple) '(:fruit . :fruit))))
    (should (= 1 (multi-equal '[:carrot [t :apple]] '[:carrot [t :fruit]])))))

(ert-deftest multi-no-preferred ()
  (multi-save-symbols (:cat :orange :tabby show)
    (multi-defmulti show #'vector)
    (multi-defmethod show [:dog :cat] (a b) :chase)
    (multi-defmethod show [:dog :orange] (a b) :bark)
    (multi-defmethod show :default (a b) :unknown)
    (should (eq :chase (show :dog :cat)))
    (should (eq :bark (show :dog :orange)))
    (multi-derive :tabby :cat)
    (multi-derive :tabby :orange)
    (should-error (show :dog :tabby))))

(ert-deftest multi-default ()
  (multi-save-symbols (show)
    (multi-defmulti show #'vector)
    (should-error (show))
    (multi-defmethod show :default () :foo)
    (should (eq :foo (show)))))

(defmacro multi-test-measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun multi-test-benchmark ()
  "Return the time of a benchmark of multimethod dispatching."
  (multi-test-measure-time
    (multi-save-symbols
        (meal-result :sweet :fruit :apple :gala :orange :candy :cake)
      (multi-defmulti meal-result #'vector)
      (multi-defmethod meal-result [:sweet :sweet] (a b) :fat)
      (multi-defmethod meal-result [:fruit :fruit] (a b) :skinny)
      (multi-defmethod meal-result [:fruit :sweet] (a b) :average)
      (multi-defmethod meal-result [:sweet :fruit] (a b) :average)
      (multi-derive :apple :fruit)
      (multi-derive :orange :fruit)
      (multi-derive :gala :apple)
      (multi-derive :cake :sweet)
      (multi-derive :candy :sweet)
      (cl-loop repeat 50000
            do (meal-result :cake :candy)
            do (meal-result :gala :orange)
            do (meal-result :apple :fruit)))))

(defun multi-test-benchmark-print ()
  "Print the benchmark results."
  (interactive)
  (princ (format "Benchmark: %f seconds\n" (multi-test-benchmark))))

;;; multi-tests.el ends here
