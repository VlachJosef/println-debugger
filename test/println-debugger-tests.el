;;; println-debugger-tests.el --- Tests for println-debugger functions -*- lexical-binding: t -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;;  Tests for println-debugger
;;
;;; Code:

(require 'ert)
(require 'ert-x)
(require 'println-debugger)

(defun println-set-preferences (counter mode flags)
  (setf (println-gen-preferences->counter println-gen-global-preferences) counter)
  (setf (println-gen-preferences->mode println-gen-global-preferences) mode)
  (setf (println-gen-preferences->flags println-gen-global-preferences) flags))

(defun println-preferences-default ()
  (println-set-preferences 0 :killed-text
                           (println-gen-flags-create
                            :multiline t
                            :align nil
                            :show-identifier nil)))

(defun println-preferences-stamp-cluster ()
  (println-set-preferences 2 :stamp
                           (println-gen-flags-create
                            :multiline t
                            :align nil
                            :show-identifier nil)))

(defun println-preferences-stamp-single-line-cluster ()
  (println-set-preferences 4 :stamp
                           (println-gen-flags-create
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

;;; println-debugger-tests.el ends here
