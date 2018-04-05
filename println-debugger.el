;;; println-debugger.el --- Convenient function for generating println statements

;; Copyright (c) 2018 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 's)
(require 'imenu)
(require 'subr-x)
(require 'ensime)

(defvar println-scala "println(\"%s: \" + (%s))")
(defvar println-emacs-lisp "(message \"%s: %%s\" %s)")
(defvar println-javascript "console.log('%s', %s);")

(defun print-ln-print (prompt)
  (let ((num (prefix-numeric-value arg))
        (on-empty-line (string-blank-p (thing-at-point 'line t))))
    (when (> num 0)
      (when (equal num 1)
        (unless on-empty-line
          (beginning-of-line)
          (ensime-company-complete-or-indent)
          (kill-line)))

      (when (and (> num 1) (not on-empty-line))
        (move-beginning-of-line nil)
        (insert "\n")
        (previous-line))

      (dotimes (number num)
        (let* ((index (- num number 1))
               (latest-kill (current-kill index t))
               (latest-kill-unquoted (s-replace "\"" " " latest-kill)))
          (ensime-company-complete-or-indent)
          (insert (format prompt latest-kill-unquoted latest-kill))
          (move-end-of-line nil)
          (unless (equal index 0)
            (insert "\n")))))))

(defun print-ln (&optional arg)
  (interactive "P")
  (pcase major-mode
    (`emacs-lisp-mode (print-ln-print println-emacs-lisp))
    (`js-mode (print-ln-print println-javascript))
    (`scala-mode (print-ln-print println-scala))))

(provide 'println-debugger)
;;; println-debugger.el ends here
