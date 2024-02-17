;;; println-debugger-rust.el --- println! statements generation for Rust -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-debugger-common)

(defun println-rust-search-fn ()
  (rust-ts-mode--defun-name (treesit-defun-at-point)))

(defun println-rust-to-string (item identifier)
  (concat "println!(\"" (println-identifier identifier) (println-safe-string item) ": {:?}\", "
          item ");"))

(defun println-rust-literal-string (item)
  (format "println!(\"%s\");" item))

(defun println-rust-value (item)
  (format "println!(\"{:?}\", %s);" item))

(defun println-rust-to-string-aligned (item longest identifier)
  (concat "println!(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: {:?}") (println-safe-string item)) "\", " item ");"))

(defun println-rust-render-single-line (items identifier)
  (concat "println!(\""
          (println-identifier identifier)
          (s-chop-prefix " " (apply 'concat (make-list (length items) " {:?}")))
          "\", "
          (mapconcat #'identity items ", ")
          ");"))

(defun println-rust-identifier ()
  (format "%s" (println-rust-search-fn)))

(defun println-rust-stamp (order)
  (format "println!(\"HeRe %s%s%s\");" order order order))

(defun println-rust-foreach (item type)
  (pcase type
    (:foreach
     (format "for item in %s.iter() {\n    println!(\"{:?}\", item);\n}" item))
    (:foreach-delimited
     (format "println!(\"%s START\");\nfor item in %s.iter() {\n    println!(\"{:?}\", item);\n}\nprintln!(\"%s END\");" item item item))))


(println-register-major-mode 'rust-ts-mode
                             #'println-rust-to-string
                             #'println-rust-literal-string
                             #'println-rust-value
                             #'println-rust-foreach
                             #'println-rust-to-string-aligned
                             #'println-rust-render-single-line
                             #'println-rust-identifier
                             #'println-rust-stamp)

(provide 'println-debugger-rust)
