;;; multi-tests.el -- tests for multimethods library

(require 'ert)
(require 'cl-lib)
(require 'multi)

;; emacs -batch -Q -L .. -L . -l multi-tests.el -f ert-run-tests-batch

(defmacro multi-save-plists (symbols &rest body)
  "Run BODY, restoring plists for SYMBOLS afterwards."
  (declare (indent 1))
  `(let ((multi--plists (mapcar #'symbol-plist ',symbols)))
     (unwind-protect
         (progn
           (dolist (symbol ',symbols)
             (setf (symbol-plist symbol) nil))
           ,@body)
       (cl-loop for symbol in ',symbols
                for plist in multi--plists
                do (setf (symbol-plist symbol) plist)))))

(ert-deftest multi-inheritance ()
  (multi-save-plists (:fruit :apple :gala :carrot)
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
  (multi-save-plists (:cat :orange :tabby multi-test--action)
    (multi-defmulti multi-test--action #'vector)
    (multi-defmethod multi-test--action [:dog :cat] (a b) :chase)
    (multi-defmethod multi-test--action [:dog :orange] (a b) :bark)
    (multi-defmethod multi-test--action :default (a b) :unknown)
    (should (eq :chase (multi-test--action :dog :cat)))
    (should (eq :bark (multi-test--action :dog :orange)))
    (multi-derive :tabby :cat)
    (multi-derive :tabby :orange)
    (should-error (multi-test--action :dog :tabby))))

(ert-deftest multi-default ()
  (multi-defmulti multi-test--action #'vector)
  (should-error (multi-test--action))
  (multi-defmethod multi-test--action :default () :foo)
  (should (eq :foo (multi-test--action))))

;;; multi-tests.el ends here
