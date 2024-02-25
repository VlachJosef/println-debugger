;;; println-debugger-rust.el --- Println! statements generation for Rust -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'println-gen)

(defun println-debugger-rust-to-single-line-string (item)
  (format "%s: {:?}" (println-gen-safe-string item)))

(defun println-debugger-rust-to-string (item identifier)
  (concat "println!(\"" (println-gen-identifier identifier) (println-gen-safe-string item) ": {:?}\", "
          item ");"))

(defun println-debugger-rust-literal-string (item)
  (format "println!(\"%s\");" (println-gen-safe-string item)))

(defun println-debugger-rust-value (item)
  (format "println!(\"{:?}\", %s);" item))

(defun println-debugger-rust-to-string-aligned (item longest identifier)
  (concat "println!(\"" (println-gen-identifier identifier) (format (concat "%-" (number-to-string longest) "s: {:?}") (println-gen-safe-string item)) "\", " item ");"))

(defun println-debugger-rust-render-single-line (items identifier)
  (concat "println!(\""
          (println-gen-identifier identifier)
          (mapconcat #'println-debugger-rust-to-single-line-string items " ")
          "\", "
          (mapconcat #'identity items ", ")
          ");"))

(defun println-debugger-rust-identifier ()
  (format "%s" (treesit-defun-name (treesit-defun-at-point))))

(defun println-debugger-rust-stamp (order)
  (format "println!(\"HeRe %s%s%s\");" order order order))

(defun println-debugger-rust-foreach (item type)
  (pcase type
    (:foreach
     (format "for item in %s.iter() {\n    println!(\"{:?}\", item);\n}" item))
    (:foreach-delimited
     (format "println!(\"%s START\");\nfor item in %s.iter() {\n    println!(\"{:?}\", item);\n}\nprintln!(\"%s END\");" item item item))))

(println-gen-register-major-mode
 'rust-ts-mode
 #'println-debugger-rust-to-string
 #'println-debugger-rust-literal-string
 #'println-debugger-rust-value
 #'println-debugger-rust-foreach
 #'println-debugger-rust-to-string-aligned
 #'println-debugger-rust-render-single-line
 #'println-debugger-rust-identifier
 #'println-debugger-rust-stamp)

(provide 'println-debugger-rust)

;;; println-debugger-rust.el ends here
