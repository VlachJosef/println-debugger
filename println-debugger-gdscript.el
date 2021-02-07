;;; println-debugger-gdscript.el --- printt statements generation for GDScript -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-debugger-common)

(defun println-search-gdscript-func ()
  (println-search-regex "func" 1))

(defun println-gdscript-to-string (item identifier)
  (concat "printt(\"" (println-identifier identifier) (println-safe-string item) ": \", "
          (println-editable item) ")"))

(defun println-javascript-literal-string (item)
  (format "printt(\"%s\")" (println-editable item)))

(defun println-javascript-value (item)
  (format "printt(%s)" (println-editable item)))

(defun println-gdscript-to-string-aligned (item longest identifier)
  (concat "printt(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-safe-string item)) "\", " (println-editable item) ")"))

(defun println-gdscript-to-single-line-string (item)
  (concat "\", " (println-safe-string item) ": \", " item))

(defun println-gdscript-render-single-line (items identifier)
  (concat "printt(\"" (println-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-gdscript-to-single-line-string items ", ")) ")"))

(defun println-gdscript-identifier ()
  (format "%s.%s" (buffer-name) (println-search-gdscript-func)))

(defun println-gdscript-stamp (order)
  (format "printt(\"HeRe %s%s%s\")" order order order))

(defun println-gdscript-foreach (item type)
  (format "printt(\"FOREACH\")"))

(println-register-major-mode 'gdscript-mode
                             #'println-gdscript-to-string
                             #'println-gdscript-literal-string
                             #'println-gdscript-value
                             #'println-gdscript-to-string-aligned
                             #'println-gdscript-render-single-line
                             #'println-gdscript-identifier
                             #'println-gdscript-stamp
                             #'println-gdscript-foreach)

(provide 'println-debugger-gdscript)
