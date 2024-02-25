;;; println-debugger-emacs-lisp.el --- Message statements generation for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'elisp-mode)
(require 'println-debugger)

(defun println-debugger-emacs-lisp-to-single-line-string (item)
  (format "%s: %%s" (println-debugger-safe-string item)))

(defun println-debugger-emacs-lisp-to-string (item identifier)
  (format "(message \"%s%s: %%s\" %s)" (println-debugger-identifier identifier) (println-debugger-safe-string item) item))

(defun println-debugger-emacs-lisp-literal-string (item)
  (format "(message \"%s\")" (println-debugger-safe-string item)))

(defun println-debugger-emacs-lisp-value (item)
  (format "(message \"%%s\" %s)" item))

(defun println-debugger-emacs-lisp-to-string-aligned (item longest identifier)
  (format "(message \"%s%s: %%s\" %s)" (println-debugger-identifier identifier) (format (concat "%-" (number-to-string longest) "s") (println-debugger-safe-string item)) item))

(defun println-debugger-emacs-lisp-render-single-line (items identifier)
  (concat "(message \""
          (println-debugger-identifier identifier)
          (mapconcat #'println-debugger-emacs-lisp-to-single-line-string items " ")
          "\" "
          (mapconcat #'identity items " ")
          ")"))

(defun println-debugger-emacs-lisp-search-defun ()
  (save-excursion
    (re-search-backward "^(defun \\([^( ]*\\)" nil t)
    (match-string 1)))

(defun println-debugger-emacs-lisp-stamp (order)
  (format "(message \"HeRe %s%s%s\")" order order order))

(defun println-debugger-emacs-lisp-foreach (item type)
  (pcase type
    (:foreach
     (format "(dolist (element %s) (message \"\%%s\" element))" item))
    (:foreach-delimited
     (format "(message \"%s START\")\n(dolist (element %s) (message \"\%%s\" element))\n(message \"%s END\")" item item item))))

(println-debugger-register-major-mode
 'emacs-lisp-mode
 #'println-debugger-emacs-lisp-to-string
 #'println-debugger-emacs-lisp-literal-string
 #'println-debugger-emacs-lisp-value
 #'println-debugger-emacs-lisp-foreach
 #'println-debugger-emacs-lisp-to-string-aligned
 #'println-debugger-emacs-lisp-render-single-line
 #'println-debugger-emacs-lisp-search-defun
 #'println-debugger-emacs-lisp-stamp)

(provide 'println-debugger-emacs-lisp)

;;; println-debugger-emacs-lisp.el ends here
