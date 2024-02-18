;;; println-debugger-tests.el --- tests for println-debugger functions -*- lexical-binding: t -*-
;;
;;; Code:

(require 'ert)
(require 'ert-x)
(require 'println-debugger)

(defun println-set-preferences (counter mode flags)
  (setf (println-preferences->counter println-global-preferences) counter)
  (setf (println-preferences->mode println-global-preferences) mode)
  (setf (println-preferences->flags println-global-preferences) flags))

(defun println-preferences-default ()
  (println-set-preferences 0 :killed-text
                           (println-flags-create
                            :multiline t
                            :align nil
                            :show-identifier nil)))

(defun println-preferences-stamp-cluster ()
  (println-set-preferences 2 :stamp
                           (println-flags-create
                            :multiline t
                            :align nil
                            :show-identifier nil)))

(defun println-preferences-stamp-single-line-cluster ()
  (println-set-preferences 4 :stamp
                           (println-flags-create
                            :multiline nil
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
