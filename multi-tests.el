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

;;; multi-tests.el ends here
