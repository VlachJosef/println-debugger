;;; println-debugger.el --- Convenient function for generating println statements -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-debugger-common)

(if (featurep 'elisp-mode)
    (require 'println-debugger-emacs-lisp)
  (with-eval-after-load 'elisp-mode
    (require 'println-debugger-emacs-lisp)))

(if (featurep 'scala-mode)
    (require 'println-debugger-scala)
  (with-eval-after-load 'scala-mode
    (require 'println-debugger-scala)))

(if (featurep 'scala-ts-mode)
    (require 'println-debugger-scala)
  (with-eval-after-load 'scala-ts-mode
    (require 'println-debugger-scala)))

(if (featurep 'js)
    (require 'println-debugger-javascript)
  (with-eval-after-load 'js
    (require 'println-debugger-javascript)))

(if (featurep 'typescript-ts-mode)
    (require 'println-debugger-javascript)
  (with-eval-after-load 'typescript-ts-mode
    (require 'println-debugger-javascript)))

(if (featurep 'rust-ts-mode)
    (require 'println-debugger-rust)
  (with-eval-after-load 'rust-ts-mode
    (require 'println-debugger-rust)))

(provide 'println-debugger)
