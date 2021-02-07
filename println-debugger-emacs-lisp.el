;;; println-debugger-emacs-lisp.el --- message statements generation for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-debugger-common)

(defun println-emacs-lisp-to-single-line-string (item)
  (concat (format ", %s:" item) " %s"))

(defun println-emacs-lisp-to-string (item identifier)
  (format "(message \"%s%s: %%s\" %s)" (println-identifier identifier) item (println-editable item)))

(defun println-emacs-lisp-literal-string (item)
  (format "(message \"%s\")" (println-editable item)))

(defun println-emacs-lisp-value (item)
  (format "(message \"%%s\" %s)" item (println-editable item)))

(defun println-emacs-lisp-to-string-aligned (item longest identifier)
  (format "(message \"%s%s: %%s\" %s)" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s") (println-safe-string item)) (println-editable item)))

(defun println-emacs-lisp-render-single-line (items identifier)
  (concat "(message \""
          (println-identifier identifier)
          (s-chop-prefix ", " (mapconcat #'println-emacs-lisp-to-single-line-string items ""))
          "\" "
          (mapconcat #'println-editable items " ")
          ")"))

(defun println-emacs-lisp-search-defun ()
  (save-excursion
    (re-search-backward "^(defun \\([^( ]*\\)")
    (match-string 1)))

(defun println-emacs-lisp-stamp (order)
  (format "(message \"HerE %s%s%s\")" order order order))

(println-register-major-mode 'emacs-lisp-mode
                             #'println-emacs-lisp-to-string
                             #'println-emacs-lisp-literal-string
                             #'println-emacs-lisp-value
                             #'println-emacs-lisp-to-string-aligned
                             #'println-emacs-lisp-render-single-line
                             #'println-emacs-lisp-search-defun
                             #'println-emacs-lisp-stamp)

(provide 'println-debugger-emacs-lisp)
