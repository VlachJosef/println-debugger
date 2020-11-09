;;; println-debugger.el --- Convenient function for generating println statements -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 's)
(eval-when-compile (require 'subr-x))

(defvar println-scala "println(\"%s: \" + (%s))")
(defvar println-emacs-lisp "(message \"%s: %%s\" %s)")
(defvar println-javascript "console.log('%s', %s);")
(defvar println-haskell "putTextLn $ \"%s \" <> show %s")
(defvar println-gdscript "print(\"%s \", str(%s))")
;;(defvar println-gdscript "gui.print_scalar(\"%s \", str(%s))")

;;(current-kill 3)

(defvar lorem-scala "package uk.gov.hmrc.gform.eval

import cats.Monoid
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, Sum }

case class SumInfo(lookup: Map[Sum, Set[FormComponentId]]) extends AnyVal {
  // uk
  // gov
  // hmrc
  // gform
  // sharedmodel
  // formtemplate
  // \"A\"
  def keys: Set[Sum] = lookup.keySet

  def ++(sumInfo: SumInfo): SumInfo = {
    SumInfo(lookup ++ sumInfo.lookup)
  }

  def sums: Set[Sum] = lookup.keys.toSet

  def dependees(formComponentId: FormComponentId): Option[Set[FormComponentId]] = {

    val res: Set[FormComponentId] = lookup.toList
      .collect {
        case (Sum(FormCtx(fcId)), dependeesFcId) if (fcId.baseComponentId === formComponentId.baseComponentId) =>
          dependeesFcId
      }
      .toSet
      .flatten

    if (res.isEmpty) None else Some(res)
  }
}
")

;;((read-char-choice "HI:" '(?a ?b ?c)))

;;(insert (propertize "Red Text" 'font-lock-face '(:foreground "red")))


(defface print-ln-diff-hunk-heading
  '((((class color) (background dark))
     :extend t
     :background "DarkSlateBlue"))
  "Face for print-ln generated lines."
  :group 'print-ln-faces)

(defvar println-basic-renderer (make-hash-table))
(defvar println-aligned-renderer (make-hash-table))
(defvar println-single-line-renderer (make-hash-table))
(defvar println-identifier-founder (make-hash-table))

(defvar println-counter 0)

(define-minor-mode print-ln-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  nil
  " println"
  nil
  ;;(font-lock-remove-keywords nil print-ln--font-lock-keywords)
  (when print-ln-mode
    ))

;; (defun print-ln-overlay-modified (overlay before-after beginning end &optional length)
;;   (message "overlay: %s" overlay)
;;   (message "before-after: %s" before-after)
;;   (message "beginning: %s" beginning)
;;   (message "end: %s" end)
;;   (message "length: %s" length)
;;   )
;;;###autoload
(add-hook 'scala-mode-hook 'print-ln-mode)

(add-hook 'before-revert-hook 'println-save-print-data)
(add-hook 'after-revert-hook 'println-recover-print-data)

(cl-defstruct (println-revert-info (:constructor println-revert-info-create)
                                   (:copier nil)
                                   (:conc-name println-revert-info->))
  data)


;;(eobp)

(defvar println-revert-data)

(defun println-demo-data () (let* ((kill-ring '("hi" "there"))
                                   (identifier "Demo.init")
                                   (flags (print-ln-flags-create :multiline t :align t :show-identifier t))
                                   (data (print-ln-data-create :items nil :identifier identifier :flags flags)))
                              (dotimes (item (length kill-ring))
                                (let ((current (nth item kill-ring)))
                                  (println-data-add-item data current)))
                              data))

(defun println-demo ()
  (forward-line)
  (print-ln-render (println-demo-data)))

;; (defun println-current-overlay (pos)
;;   (setq current-overlay nil)
;;   (let ((overlays (overlays-in pos pos)))
;;     (dolist (overlay overlays)
;;       (when (overlay-get overlay 'print-ln-p)
;;         (let ((data (overlay-get overlay 'print-ln)))
;;           (push (println-revert-info-create :data data) println-revert-data))))
;;     (remove-overlays (point-min) (point-max) 'print-ln-p t)))

(defun println-save-print-data ()
  (setq println-revert-data nil)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (dolist (overlay overlays)
      (when (overlay-get overlay 'print-ln-p)
        (let ((data (overlay-get overlay 'print-ln)))
          (push (println-revert-info-create :data data) println-revert-data))))
    (remove-overlays (point-min) (point-max) 'print-ln-p t)))

(defun print-ln-to-regex (str)
  (s-replace-all '(("println(" . "[[:space:]]*println(\n?[[:space:]]*")) str))

(defun println-recover-print-data ()
  (dolist (revert-info println-revert-data)
    (let* ((data (println-revert-info->data revert-info))
           (quoted-string (print-ln-to-regex (regexp-quote (println-render data)))))
      (save-excursion
        (goto-char (point-min))
        (when-let ((res (re-search-forward quoted-string nil t)))
          (let ((overlay (make-overlay (match-beginning 0) (1+ (match-end 0)) (current-buffer) t nil)))
            (overlay-put overlay 'face 'print-ln-diff-hunk-heading)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'print-ln-p t)
            (overlay-put overlay 'print-ln data)
            (overlay-put overlay 'keymap print-ln-keymap)))))))

(cl-defstruct (print-ln-data (:constructor print-ln-data-create)
                             (:copier nil)
                             (:conc-name print-ln-data->))
  items identifier flags)

(cl-defstruct (print-ln-flags (:constructor print-ln-flags-create)
                              (:copier nil)
                              (:conc-name print-ln-flags->))
  multiline align show-identifier)

(defun println-table-update-identifier (data identifier)
  (setf (print-ln-data->identifier data)
        identifier))

(defun println-data-add-item (data item)
  (setf (print-ln-data->items data)
        (append (print-ln-data->items data) (list item))))

(defun println-data-update-item (overlay current modified)
  (let ((data (overlay-get overlay 'print-ln)))
    (setf (print-ln-data->items data)
        (seq-map (lambda (item) (if (eq item current) modified item)) (print-ln-data->items data)))))

(defun println-table-remove-row (data)
  (let* ((items (print-ln-data->items data)))
    (if (equal 1 (length items))
        (print-ln-delete-current)
      (setf (print-ln-data->items data)
            (reverse (cdr (reverse items))))
      (print-ln-delete-current)
      (print-ln-render data))))

(defun print-ln-foreach()
  (interactive)

  (message "HELLO FROM print-ln-foreach"))

(defun println-search-regex (regex num)
  (save-excursion
    (let* ((scala-syntax:definition-words-re regex)
           (scala-syntax:all-definition-re (scala-syntax:build-definition-re (concat "\\(?1:" scala-syntax:definition-words-re "\\)\\b"))))
      (scala-syntax:beginning-of-definition)
      (re-search-forward (concat regex " " scala-syntax:plainid-re))
      (match-string num))))

(defun println-search-def ()
  (println-search-regex "def" 1))

(defun println-search-class ()
  (println-search-regex "\\(class\\|object\\)" 2))

(defun print-ln-toggle-identifier (data)
  (let ((flags (print-ln-data->flags data)))
    (setf (print-ln-flags->show-identifier flags)
          (not (print-ln-flags->show-identifier flags)))))

(defun print-ln-toggle-multiline (data)
  (let ((flags (print-ln-data->flags data)))
    (setf (print-ln-flags->multiline flags)
          (not (print-ln-flags->multiline flags)))))

(defun print-ln-toggle-align (data)
  (let ((flags (print-ln-data->flags data)))
    (setf (print-ln-flags->align flags)
          (not (print-ln-flags->align flags)))))

(defun println-get-data ()
  (get-char-property (point) 'print-ln))

(defun print-ln-align()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (print-ln-toggle-align data)
    (print-ln-delete-current)
    (print-ln-render data)))

(defun print-ln-multiline()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (print-ln-toggle-multiline data)
    (print-ln-delete-current)
    (print-ln-render data)))

(defun print-ln-identifier()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (print-ln-toggle-identifier data)
    (print-ln-delete-current)
    (print-ln-render data)))

(defun print-ln-increase()
  (interactive)
  (let* ((println-inhibit-modification-hooks t)
         (data (println-get-data))
         (items (print-ln-data->items data))
         (kill-items (println-preprocess-kill-ring))
         (next-kill (seq-find (lambda (kill-item)
                                (not (member kill-item items)))
                              kill-items)))
    (if (not next-kill)
        (message "No more elements in kill-ring.")
      (println-data-add-item data next-kill)
      (print-ln-delete-current)
      (print-ln-render data))))

(defun println-pre-process-kill-ring-element (element)
  (s-collapse-whitespace
   (s-trim (substring-no-properties element))))

(defun println-preprocess-kill-ring ()
  (let ((result))
    (dolist (element kill-ring)
      (let ((element (println-pre-process-kill-ring-element element)))
        (unless (or
                 (string-blank-p element)
                 (string-match-p "[\r\n]+" element))
          (setq result (cons element result)))))
    (reverse result)))

(defun print-ln-decrease ()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (println-table-remove-row data)))

(defun print-ln-stamp()
  (interactive)
  (let ((data (println-get-data)))
    (print-ln-delete-current)
    (setq println-counter (1+ println-counter))
    (print-ln-render-2 data println-counter)))

(defun print-ln-reset ()
  (interactive)
  (setq println-counter 0)
  (message "Println counter reset."))

(defun print-ln-reverse()
  (interactive)
  (let* ((data (println-get-data))
         (items (print-ln-data->items data)))
    (setf (print-ln-data->items data)
          (reverse items))
    (print-ln-delete-current)
    (print-ln-render data)))

(defvar print-ln-keymap
  (let ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "C-i") 'print-ln-foreach)
    (define-key map (kbd "C-M-r") 'print-ln-reverse)
    (define-key map (kbd "C-M-i") 'print-ln-reset)
    (define-key map (kbd "C-M-o") 'print-ln-stamp)
    (define-key map (kbd "C-M-s") 'print-ln-identifier)
    (define-key map (kbd "C-M-m") 'print-ln-multiline)
    (define-key map (kbd "C-M-a") 'print-ln-align)
    (define-key map (kbd "C-M-p") 'print-ln-decrease)
    (define-key map (kbd "C-M-n") 'print-ln-increase)
    (define-key map (kbd "C-M-d") 'print-ln-delete-at-point)
    map)
  "Keymap for print-ln managed region.")

(defun print-ln-delete-at-point ()
  (interactive)
  (print-ln-delete-current))

(defun print-ln-find-overlay-specifying (prop)
  (let ((overlays (overlays-at (point))))
    (seq-find (lambda (overlay) (overlay-get overlay prop)) overlays)))

(defun print-ln-delete-current ()
  (when-let ((overlay (print-ln-find-overlay-specifying 'print-ln)))
    (delete-region (overlay-start overlay) (overlay-end overlay))))

(defun println-safe-string (str) (s-replace "\"" "" str))

(defun println-identifier (identifier)
  (if identifier
      (format "[%s] " identifier)
    ""))

(defun println-to-string (item identifier)
  (when-let ((basic-renderer (gethash major-mode println-basic-renderer)))
    (funcall basic-renderer item identifier)))

(defun println-to-string-aligned (item longest identifier)
  (when-let ((aligned-renderer (gethash major-mode println-aligned-renderer)))
    (funcall aligned-renderer item longest identifier)))

(defun println-scala-to-single-line-string (item)
  (concat "\", " (println-safe-string item) ": \" + " item))

(defun println-emacs-lisp-to-single-line-string (item)
  (concat (format ", %s:" item) " %s"))

(defun println-set-major-mode-rules (buffer-type &rest rules)
  (if-let* ((binding (assoc buffer-type gdscript-debug--buffer-rules)))
      (setcdr binding rules)
    (push (cons buffer-type rules)
	  gdscript-debug--buffer-rules)))

(defun println-render-single-line (items identifier)
  (when-let ((single-line-renderer (gethash major-mode println-single-line-renderer)))
    (funcall single-line-renderer items identifier)))

(defun println-multiline-p (data)
  (print-ln-flags->multiline (print-ln-data->flags data)))

(defun println-align-p (data)
  (print-ln-flags->align (print-ln-data->flags data)))

(defun println-show-identifier-p (data)
  (print-ln-flags->show-identifier (print-ln-data->flags data)))

(defun println-render (data)
  (let ((items (print-ln-data->items data))
        (identifier (when (println-show-identifier-p data) (print-ln-data->identifier data)))
        (indentation (s-pad-left (current-indentation) " " " ")))
    (if (println-multiline-p data)
        (if (println-align-p data)
            (let ((longest (seq-max (seq-map (lambda (it) (string-width (or it ""))) items))))
              (mapconcat (lambda (item)
                           (println-to-string-aligned item longest identifier)) items (concat "\n" indentation)))
          (mapconcat (lambda (item)
                       (println-to-string item identifier)) items (concat "\n" indentation)))
      (println-render-single-line items identifier))))

;; (let ((buffer (generate-new-buffer "limbo")))
;;   (with-current-buffer buffer
;;     (scala-mode)
;;     (insert lorem-scala)
;;     (display-buffer (current-buffer))))

(defun println-search-identifier ()
  (when-let ((identifier-founder (gethash major-mode println-identifier-founder)))
    (funcall identifier-founder)))

(defun println (prefix)
  (interactive "p")
  (when (println-get-data)
    (print-ln-delete-current))

  (let* ((kill-ring (println-preprocess-kill-ring))
         (identifier (println-search-identifier))
         (flags (print-ln-flags-create :multiline t :align nil :show-identifier t))
         (data (print-ln-data-create :items nil :identifier identifier :flags flags)))
    (dotimes (item (min prefix (length kill-ring)))
      (let ((current (nth item kill-ring)))
        (println-data-add-item data current)))

    (forward-line)
    (print-ln-render data)))

(defun print-ln-render (data)
  (let ((str (println-render data)))
    (print-ln-render-content data str)))

(defun print-ln-render-2 (data counter)
  (print-ln-render-content data (format "println(\"HERE %s%s%s\")" counter counter counter)))

(defun println-modification (overlay after beginning end &optional pre-length)
  (message "overlay %s after %s start %s-%s pre-length %s" overlay after beginning end pre-length))

(defun print-ln-render-content (data content)
  (indent-according-to-mode)
  (let* ((indentation (s-pad-left (current-indentation) " " " "))
         (content (concat indentation content "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'print-ln-diff-hunk-heading)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'print-ln-p t)
      (overlay-put overlay 'keymap print-ln-keymap)
      ;;(overlay-put overlay 'modification-hooks '(println-modification))

      (overlay-put overlay 'print-ln data)
      ;;(put-text-property start (point) 'print-ln data)
      ;;(put-text-property start (point) 'keymap print-ln-keymap)

      (backward-char))))

(defun println-insert-in-front (start end)

  (message "[println-insert-in-front] %s %s" start end))

(defvar println-inhibit-modification-hooks nil)

(defun println-modify (start end)
  (message "[MODIFY] star t %s end %s undo-in-progress %s" start end undo-in-progress)
  (when (and
         (not undo-in-progress)
         (not println-inhibit-modification-hooks)
         (print-ln-find-overlay-specifying 'print-ln-p))
    (let* ((inhibit-modification-hooks t)
           (inserted (buffer-substring-no-properties start end))
           (overlay (print-ln-find-overlay-specifying 'print-ln))
           (point (point))
           (current-item (get-text-property (1- point) :print-ln-current))
           ;;(data (overlay-get overlay 'print-ln))
           (start2 (previous-single-property-change start :print-ln-current nil (line-beginning-position)))
           (end2 (next-single-property-change end :print-ln-current nil (line-end-position)))
           (left (buffer-substring-no-properties start2 start))
           (right (buffer-substring-no-properties end (if (eq end2 (line-end-position)) (1- end2) end2)))
           (modified-item (concat left right))
           (search-for (concat current-item "[[:space:]]*:")))

      (put-text-property start2 end2 :print-ln-current modified-item)
      (message "[MODIFY] star t %s end  %s :%s:  mi: %s search-for %s" start end inserted modified-item search-for)

      (when (re-search-backward search-for (line-beginning-position) t)
        (replace-match (concat modified-item ":"))
        (println-data-update-item overlay current-item modified-item)
        (goto-char  (1- point)))

      (message "[MODIFY] start2 %s end2 %s eol %s current %s" start2 end2 (line-end-position) current-item))))

(defun println-insert-behind (start end)
  (message "[BEHIND] star t %s end %s undo-in-progress %s" start end undo-in-progress)
  (when (not undo-in-progress)
    (let* ((inhibit-modification-hooks t)
           (inserted (buffer-substring-no-properties start end))
           (overlay (print-ln-find-overlay-specifying 'print-ln))
           (point (point))
           (current-item (get-text-property (1- point) :print-ln-current))
           (data (overlay-get overlay 'print-ln))
           (start2 (previous-single-property-change point :print-ln-current nil (line-beginning-position)))
           (end2 (next-single-property-change point :print-ln-current nil (line-end-position)))
           (modified-item (buffer-substring-no-properties start2 (if (eq end2 (line-end-position)) (1- end2) end2)))
           (search-for (concat current-item "[[:space:]]*:")))

      ;; (concat (car (print-ln-data->items data)) inserted)
      (println-data-update-item overlay current-item modified-item)
      ;; (print-ln-delete-current)
      ;; (print-ln-render data)
      ;; (goto-char (1+ point))
      (when (re-search-backward search-for (line-beginning-position) t)
        (replace-match (concat modified-item ":"))
        (put-text-property (1+ start2) (1+ end2) :print-ln-current modified-item)
        (goto-char  (1+ point)))

      (message "[BEHIND] star t %s end  %s :%s:" start end inserted)
      (message "[BEHIND] start2 %s end2 %s eol %s current '%s' modified-item '%s'" start2 end2 (line-end-position) current-item modified-item))))

;; Scala support
(defun println-scala-to-string (item identifier)
  (message "[println-scala-to-string] identifier: %s" identifier)
  (message "[println-scala-to-string] itAem     : %s" itAem)

  (concat "println(\"" (println-identifier identifier) (println-safe-string item) ": \" + "
          (propertize item 'modification-hooks '(println-modify)) ")"))

(defun println-scala-to-string-aligned (item longest identifier)
  (concat "println(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-safe-string item)) "\" + " item ")"))

(defun println-scala-render-single-line (items identifier)
  (concat "println(\"" (println-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-scala-to-single-line-string items " + ")) ")"))

(defun println-scala-identifier ()
  (format "%s.%s" (println-search-class) (println-search-def)))

(defun println-editable (item)
  (propertize item
              :print-ln-current item
              'modification-hooks '(println-modify)
              'insert-in-front-hooks '(println-insert-in-front)
              'insert-behind-hooks '(println-insert-behind)))

;; Emacs Lisp support
(defun println-emacs-lisp-to-string (item identifier)
  (format "(message \"%s%s: %%s\" %s)" (println-identifier identifier) item (println-editable item)))

(defun println-emacs-lisp-to-string-aligned (item longest identifier)
  (format "(message \"%s%s: %%s\" %s)" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s") (println-safe-string item)) (println-editable item)))

(defun println-emacs-lisp-render-single-line (items identifier)
  (concat "(message \""
          (println-identifier identifier)
          (s-chop-prefix ", " (mapconcat #'println-emacs-lisp-to-single-line-string items ""))
          "\" "
          (mapconcat #'identity items " ")
          ")"))

(defun println-emacs-lisp-search-defun ()
  (save-excursion
    (re-search-backward "^(defun \\([^( ]*\\)")
    (match-string 1)))

(defun println-register-major-mode (major-mode basic aligned single identifier)
  (puthash major-mode basic println-basic-renderer)
  (puthash major-mode aligned println-aligned-renderer)
  (puthash major-mode single println-single-line-renderer)
  (puthash major-mode identifier println-identifier-founder))

(println-register-major-mode 'scala-mode
                             #'println-scala-to-string
                             #'println-scala-to-string-aligned
                             #'println-scala-render-single-line
                             #'println-scala-identifier)

(println-register-major-mode 'emacs-lisp-mode
                             #'println-emacs-lisp-to-string
                             #'println-emacs-lisp-to-string-aligned
                             #'println-emacs-lisp-render-single-line
                             #'println-emacs-lisp-search-defun)

(defun print-ln-print (num prompt)
  (let ((on-empty-line (string-blank-p (thing-at-point 'line t))))
    (when (> num 0)
      (when (equal num 1)
        (unless on-empty-line
          (beginning-of-line)
          (indent-according-to-mode)
          (kill-line)))

      (when (and (> num 1) (not on-empty-line))
        (move-beginning-of-line nil)
        (insert "\n")
        (previous-line))

      (dotimes (number num)
        (let* ((index (- num number 1))
               (latest-kill (current-kill index t))
               (latest-kill-unquoted (s-replace "\"" " " latest-kill)))
          (indent-according-to-mode)
          (insert (format prompt latest-kill-unquoted latest-kill))
          (move-end-of-line nil)
          (unless (equal index 0)
            (insert "\n")))))))

(defun print-ln (arg)
  (interactive "p")
  (pcase major-mode
    (`emacs-lisp-mode (println arg))
    (`js-mode (print-ln-print arg println-javascript))
    (`scala-mode (println arg))
    (`haskell-mode (print-ln-print arg println-haskell))
    (`gdscript-mode (print-ln-print arg println-gdscript))))

(provide 'println-debugger)
;;; println-debugger.el ends here
