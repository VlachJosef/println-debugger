;;; println-debugger-tests.el --- tests for println-debugger functions -*- lexical-binding: t -*-
;;
;;; Code:

(require 'ert)
(require 'ert-x)
(require 'println-debugger)

(defun println-preferences-default ()
  (setf (println-preferences->counter println-global-preferences) 0)
  (setf (println-preferences->mode println-global-preferences) :item)
  (setf (println-preferences->flags println-global-preferences) (println-flags-create
                                                                 :multiline t
                                                                 :align nil
                                                                 :show-identifier nil)))

(ert-deftest println-test-scala ()
  (ert-test-erts-file (ert-resource-file "scala.erts")))

(ert-deftest println-test-rust ()
  (ert-test-erts-file (ert-resource-file "rust.erts")))

(ert-deftest println-test-typescript ()
  (ert-test-erts-file (ert-resource-file "typescript.erts")))

(ert-deftest println-test-elisp ()
  (ert-test-erts-file (ert-resource-file "elisp.erts")))
