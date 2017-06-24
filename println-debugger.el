;;; println-debugger.el --- Convenient function for generating println statements

;; Copyright (c) 2017 Josef Vlach

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

(defun print-ln (&optional arg)
  (interactive "P")
  (pcase major-mode
    ('scala-mode
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
             (insert (format "println(\"%s: \" + (%s))" latest-kill-unquoted latest-kill))
             (move-end-of-line nil)
             (unless (equal index 0)
               (insert "\n")))))))))

(provide 'println-debugger)
;;; println-debugger.el ends here
