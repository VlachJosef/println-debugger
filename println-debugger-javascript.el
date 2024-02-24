;;; println-debugger-javascript.el --- Console.log statements generation for Javascript -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-gen)

(defun println-debugger-javascript-to-string (item identifier)
  (concat "console.log(\"" (println-gen-identifier identifier) (println-gen-safe-string item) ": \", "
          item ");"))

(defun println-debugger-javascript-literal-string (item)
  (format "console.log(\"%s\");" item))

(defun println-debugger-javascript-value (item)
  (format "console.log(%s);" item))

(defun println-debugger-javascript-to-string-aligned (item longest identifier)
  (concat "console.log(\"" (println-gen-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-gen-safe-string item)) "\", " item ");"))

(defun println-debugger-javascript-to-single-line-string (item)
  (concat "\", " (println-gen-safe-string item) ": \", " item))

(defun println-debugger-javascript-render-single-line (items identifier)
  (concat "console.log(\"" (println-gen-identifier identifier) (string-trim-left (mapconcat #'println-debugger-javascript-to-single-line-string items ", ") "\", ") ");"))

(defun println-debugger-javascript-identifier ()
  (format "%s" (treesit-defun-name (treesit-defun-at-point))))

(defun println-debugger-javascript-stamp (order)
  (format "console.log(\"HeRe %s%s%s\");" order order order))

(defun println-debugger-javascript-foreach (item type)
  (pcase type
    (:foreach
     (format "%s.forEach((item) => console.log(item));" item))
    (:foreach-delimited
     (format "console.log(\"%s START\");\n%s.forEach((item) => console.log(item));\nconsole.log(\"%s END\");" item item item))))

(dolist (mode '(js-mode typescript-ts-mode tsx-ts-mode))
  (println-gen-register-major-mode
   mode
   #'println-debugger-javascript-to-string
   #'println-debugger-javascript-literal-string
   #'println-debugger-javascript-value
   #'println-debugger-javascript-foreach
   #'println-debugger-javascript-to-string-aligned
   #'println-debugger-javascript-render-single-line
   #'println-debugger-javascript-identifier
   #'println-debugger-javascript-stamp))

(provide 'println-debugger-javascript)

;;; println-debugger-javascript.el ends here
