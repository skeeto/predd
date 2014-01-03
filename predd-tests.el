;;; predd-tests.el -- tests for multimethods library

(require 'ert)
(require 'cl-lib)
(require 'predd)

(defmacro predd--symbol-function (symbol)
  "Like `symbol-function' but return nil if unbound."
  `(if (fboundp ,symbol) (symbol-function ,symbol) nil))

(gv-define-setter predd--symbol-function (store symbol)
  `(if ,store (fmakunbound ,symbol) (fset ,symbol ,store)))

(defmacro predd-test-harness (funcs &rest body)
  "Run BODY with a temporary hierarchy, preserving FUNCS function bindings."
  (declare (indent 1))
  `(let ((predd-hierarchy (predd-make-hierarchy)))
     (cl-letf ,(cl-loop for func in funcs
                        collect `((predd--symbol-function ',func) nil))
       ,@(cl-loop for func in funcs
                  collect `(declare-function ,func nil))
       ,@body)))

(ert-deftest predd-inheritance ()
  (predd-test-harness ()
    (predd-derive :apple :fruit)
    (predd-derive :gala :apple)
    (should (equal '(:fruit) (predd-parents :apple)))
    (should (equal '(:apple :fruit) (predd-ancestors :gala)))
    (should (= 0 (predd-isa-p :fruit :fruit)))
    (should (= 1 (predd-isa-p :apple :fruit)))
    (should (= 2 (predd-isa-p :gala :fruit)))
    (should-not (predd-isa-p :fruit :apple))
    (should-not (predd-isa-p :carrot :fruit))
    (should (= 1 (predd-isa-p :apple :fruit)))
    (should (= 2 (predd-isa-p '(:gala) '(:fruit))))
    (should (= 3 (predd-isa-p '(:gala . :apple) '(:fruit . :fruit))))
    (should (= 1 (predd-isa-p '[:carrot [t :apple]] '[:carrot [t :fruit]])))
    (should (= 0 (predd-isa-p "Foo" "Foo")))))

(ert-deftest predd-min-inheriance ()
  "Equality should find the smallest path."
  (predd-test-harness ()
    (predd-derive :ab :a)
    (predd-derive :ab :b)
    (predd-derive :aab :a)
    (predd-derive :aab :ab)
    (should (= 1 (predd-isa-p :aab :a)))))

(ert-deftest predd-no-preferred ()
  "Throw an error when no method is preferred."
  (predd-test-harness (show)
    (predd-defmulti show #'vector)
    (predd-defmethod show [:dog :cat] (a b) :chase)
    (predd-defmethod show [:dog :orange] (a b) :bark)
    (predd-defmethod show :default (a b) :unknown)
    (should (eq :chase (show :dog :cat)))
    (should (eq :bark (show :dog :orange)))
    (predd-derive :tabby :cat)
    (predd-derive :tabby :orange)
    (should-error (show :dog :tabby))))

(ert-deftest predd-default ()
  (predd-test-harness (show)
    (predd-defmulti show #'vector)
    (should-error (show))
    (predd-defmethod show :default () :foo)
    (should (eq :foo (show)))))

(defmacro predd-test-measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun predd-test-benchmark ()
  "Return the time of a benchmark of multimethod dispatching."
  (predd-test-measure-time
    (predd-test-harness (meal-result)
      (predd-defmulti meal-result #'vector)
      (predd-defmethod meal-result [:sweet :sweet] (a b) :fat)
      (predd-defmethod meal-result [:fruit :fruit] (a b) :skinny)
      (predd-defmethod meal-result [:fruit :sweet] (a b) :average)
      (predd-defmethod meal-result [:sweet :fruit] (a b) :average)
      (predd-derive :apple :fruit)
      (predd-derive :orange :fruit)
      (predd-derive :gala :apple)
      (predd-derive :cake :sweet)
      (predd-derive :candy :sweet)
      (cl-loop repeat 50000
            do (meal-result :cake :candy)
            do (meal-result :gala :orange)
            do (meal-result :apple :fruit)))))

(defun predd-test-benchmark-print ()
  "Print the benchmark results."
  (interactive)
  (princ (format "Benchmark: %f seconds\n" (predd-test-benchmark))))

;;; predd-tests.el ends here
