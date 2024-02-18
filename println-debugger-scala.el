;;; println-debugger-scala.el --- Println statements generation for Scala -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-gen)

(defun println-debugger-scala-to-single-line-string (item)
  (concat "\", " (println-gen-safe-string item) ": \" + " item))

(defun println-debugger-scala-search-def ()
  ;; (println-search-regex "def" 1)
  "Not found def")

(defun println-debugger-scala-search-class ()
  ;; (println-search-regex "\\(class\\|object\\|trait\\)" 2)
  "Not found class")

(defun println-debugger-scala-to-string (item identifier)
  (concat "println(\"" (println-gen-identifier identifier) (println-gen-safe-string item) ": \" + " item ")"))

(defun println-debugger-scala-literal-string (item)
  (format "println(\"%s\")" item))

(defun println-debugger-scala-value (item)
  (format "println(%s)" item))

(defun println-debugger-scala-to-string-aligned (item longest identifier)
  (concat "println(\"" (println-gen-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-gen-safe-string item)) "\" + " item ")"))

(defun println-debugger-scala-render-single-line (items identifier)
  (concat "println(\"" (println-gen-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-debugger-scala-to-single-line-string items " + ")) ")"))

(defun println-debugger-scala-identifier ()
  (format "%s.%s" (println-debugger-scala-search-class) (println-debugger-scala-search-def)))

(defun println-debugger-scala-stamp (order)
  (format "println(\"HeRe %s%s%s\")" order order order))

(defun println-debugger-scala-foreach (item type)
  (pcase type
    (:foreach
     (format "%s.foreach(println)" item))
    (:foreach-delimited
     (format "println(\"%s START\")\n%s.foreach(println)\nprintln(\"%s END\")" item item item))))

(println-gen-register-major-mode
 'scala-ts-mode
 #'println-debugger-scala-to-string
 #'println-debugger-scala-literal-string
 #'println-debugger-scala-value
 #'println-debugger-scala-foreach
 #'println-debugger-scala-to-string-aligned
 #'println-debugger-scala-render-single-line
 #'println-debugger-scala-identifier
 #'println-debugger-scala-stamp)

(provide 'println-debugger-scala)

;;; println-debugger-scala.el ends here
