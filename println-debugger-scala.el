;;; println-debugger-scala.el --- println statements generation for Scala -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-debugger-common)
;;(require 'scala-mode-syntax)

(defun println-scala-to-single-line-string (item)
  (concat "\", " (println-safe-string item) ": \" + " item))

;; (defun println-search-regex (regex num)
;;   (save-excursion
;;     (let* ((scala-syntax:definition-words-re regex)
;;            (scala-syntax:all-definition-re (scala-syntax:build-definition-re (concat "\\(?1:" scala-syntax:definition-words-re "\\)\\b"))))
;;       (scala-syntax:beginning-of-definition)
;;       (re-search-forward (concat regex " " scala-syntax:plainid-re) nil t)
;;       (match-string num))))

(defun println-search-def ()
  ;; (println-search-regex "def" 1)
  "Not found def"
  )

(defun println-search-class ()
  ;; (println-search-regex "\\(class\\|object\\|trait\\)" 2)
  "Not found class"
  )

(defun println-scala-to-string (item identifier)
  (concat "println(\"" (println-identifier identifier) (println-safe-string item) ": \" + "
          (println-editable item) ")"))

(defun println-scala-literal-string (item)
  (format "println(\"%s\")" (println-editable item)))

(defun println-scala-value (item)
  (format "println(%s)" (println-editable item)))

(defun println-scala-to-string-aligned (item longest identifier)
  (concat "println(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-safe-string item)) "\" + " (println-editable item) ")"))

(defun println-scala-render-single-line (items identifier)
  (concat "println(\"" (println-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-scala-to-single-line-string items " + ")) ")"))

(defun println-scala-identifier ()
  (format "%s.%s" (println-search-class) (println-search-def)))

(defun println-scala-stamp (order)
  (format "println(\"HeRe %s%s%s\")" order order order))

(defun println-scala-foreach (item type)
  (pcase type
    (:foreach
     (format "%s.foreach(println)" item))
    (:foreach-delimited
     (format "println(\"%s START\")\n%s.foreach(println)\nprintln(\"%s END\")" item item item))))

(println-register-major-mode 'scala-ts-mode
                             #'println-scala-to-string
                             #'println-scala-literal-string
                             #'println-scala-value
                             #'println-scala-to-string-aligned
                             #'println-scala-render-single-line
                             #'println-scala-identifier
                             #'println-scala-stamp
                             #'println-scala-foreach)

(provide 'println-debugger-scala)
