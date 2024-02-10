;;; println-debugger-javascript.el --- console.log statements generation for Javascript -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-debugger-common)

(defun println-search-javascript-func ()
   (js--treesit-defun-name (treesit-defun-at-point)))

(defun println-javascript-to-string (item identifier)
  (concat "console.log(\"" (println-identifier identifier) (println-safe-string item) ": \", "
          item ");"))

(defun println-javascript-literal-string (item)
  (format "console.log(\"%s\");" item))

(defun println-javascript-value (item)
  (format "console.log(%s);" item))

(defun println-javascript-to-string-aligned (item longest identifier)
  (concat "console.log(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-safe-string item)) "\", " item ");"))

(defun println-javascript-to-single-line-string (item)
  (concat "\", " (println-safe-string item) ": \", " item))

(defun println-javascript-render-single-line (items identifier)
  (concat "console.log(\"" (println-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-javascript-to-single-line-string items ", ")) ");"))

(defun println-javascript-identifier ()
  (format "%s/%s" (buffer-name) (println-search-javascript-func)))

(defun println-javascript-stamp (order)
  (format "console.log(\"HeRe %s%s%s\");" order order order))

(defun println-javascript-foreach (item type)
  (pcase type
    (:foreach
     (format "%s.forEach((item) => console.log(item));" item))
    (:foreach-delimited
     (format "console.log(\"%s START\");\n%s.forEach((item) => console.log(item));\nconsole.log(\"%s END\");" item item item))))

(println-register-major-mode 'js-mode
                             #'println-javascript-to-string
                             #'println-javascript-literal-string
                             #'println-javascript-value
                             #'println-javascript-to-string-aligned
                             #'println-javascript-render-single-line
                             #'println-javascript-identifier
                             #'println-javascript-stamp
                             #'println-javascript-foreach)

(println-register-major-mode 'typescript-ts-mode
                             #'println-javascript-to-string
                             #'println-javascript-literal-string
                             #'println-javascript-value
                             #'println-javascript-to-string-aligned
                             #'println-javascript-render-single-line
                             #'println-javascript-identifier
                             #'println-javascript-stamp
                             #'println-javascript-foreach)

(println-register-major-mode 'tsx-ts-mode
                             #'println-javascript-to-string
                             #'println-javascript-literal-string
                             #'println-javascript-value
                             #'println-javascript-to-string-aligned
                             #'println-javascript-render-single-line
                             #'println-javascript-identifier
                             #'println-javascript-stamp
                             #'println-javascript-foreach)

(provide 'println-debugger-javascript)
