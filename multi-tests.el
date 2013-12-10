;;; multi-tests.el -- tests for multimethods library

(require 'ert)
(require 'cl-lib)
(require 'multi)

;; emacs -batch -Q -L .. -L . -l multi-tests.el -f ert-run-tests-batch

(defmacro multi--save-plists (symbols &rest body)
  "Run BODY, restoring plists for SYMBOLS afterwards."
  (declare (indent 1))
  `(let ((multi--plists (mapcar #'symbol-plist ',symbols))
         (result (progn
                   (dolist (symbol ',symbols)
                     (setf (symbol-plist symbol) nil))
                   ,@body)))
     (prog1 result
       (cl-loop for symbol in ',symbols
                for plist in multi--plists
                do (setf (symbol-plist symbol) plist)))))

(ert-deftest multi-inheritance ()
  (multi--save-plists (:fruit :apple :gala :carrot)
    (multi-derive :apple :fruit)
    (multi-derive :gala :apple)
    (should (equal '(:fruit) (multi-parents :apple)))
    (should (equal '(:apple :fruit) (multi-ancestors :gala)))
    (should (multi-isa-p :fruit :fruit))
    (should (multi-isa-p :apple :fruit))
    (should (multi-isa-p :gala :fruit))
    (should-not (multi-isa-p :fruit :apple))
    (should-not (multi-isa-p :carrot :fruit))
    (should (multi-equal :apple :fruit))
    (should (multi-equal '(:gala) '(:fruit)))
    (should (multi-equal '(:gala . :apple) '(:fruit . :fruit)))
    (should (multi-equal '[:carrot [t :apple]] '[:carrot [t :fruit]]))))

;;; multi-tests.el ends here
