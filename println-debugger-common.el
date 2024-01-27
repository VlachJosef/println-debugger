;;; println-debugger-common.el --- Convenient function for generating println statements -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 's)
(eval-when-compile (require 'subr-x))

;;(insert (propertize "Red Text" 'font-lock-face '(:foreground "red")))

(defface print-ln-diff-hunk-heading
  '((((class color) (background dark))
     :extend t
     :background "DarkSlateBlue"))
  "Face for lines controlled by print-ln."
  :group 'print-ln-faces)

;; (defface print-ln-diff-hunk-heading-2
;;   '((((class color) (background dark))
;;      :extend t
;;      :background "black"))
;;   "Face for print-ln generated lines."
;;   :group 'print-ln-faces)

(defvar println-basic-renderer (make-hash-table))
(defvar println-basic-value-renderer (make-hash-table))
(defvar println-basic-literal-string-renderer (make-hash-table))
(defvar println-aligned-renderer (make-hash-table))
(defvar println-single-line-renderer (make-hash-table))
(defvar println-identifier-founder (make-hash-table))
(defvar println-stamp-renderer (make-hash-table))
(defvar println-foreach-renderer (make-hash-table))

(defvar println-counter 0)

;; (define-minor-mode print-ln-mode
;;   "Highlight nested parentheses, brackets, and braces according to their depth."
;;   nil
;;   " println"
;;   nil
;;   ;;(font-lock-remove-keywords nil print-ln--font-lock-keywords)
;;   (when print-ln-mode
;;     ))

;; (defun print-ln-overlay-modified (overlay before-after beginning end &optional length)
;;   (message "overlay: %s" overlay)
;;   (message "before-after: %s" before-after)
;;   (message "beginning: %s" beginning)
;;   (message "end: %s" end)
;;   (message "length: %s" length)
;;   )

(add-hook 'before-revert-hook 'println-save-print-data)
(add-hook 'after-revert-hook 'println-recover-print-data)

(cl-defstruct (println-revert-info
               (:constructor println-revert-info-create)
               (:copier nil)
               (:conc-name println-revert-info->))
  data)

(defvar println-revert-data)

(defun println-demo-data ()
  (let* ((kill-ring '("hi" "there"))
         (identifier "Demo.init")
         (flags (print-ln-flags-create :multiline t :align t :show-identifier nil))
         (data (println-cluster-data-create :items nil :identifier identifier :flags flags :indentation nil)))
    (dotimes (item (length kill-ring))
      (let ((current (nth item kill-ring)))
        (println-data-add-item data current)))
    data))

(defun println-demo ()
  (interactive)
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

(cl-defstruct (println-preferences
               (:constructor println-preferences-create)
               (:copier nil)
               (:conc-name println-preferences->))
  ;; can be either :item or :stamp
  mode
  ;; local preferenes for cluster of println statements
  flags)

(cl-defstruct (println-stamp-data
               (:constructor println-stamp-data-create)
               (:copier nil)
               (:conc-name println-stamp-data->))
  order)

(cl-defstruct (println-item-type
               (:copier nil)
               (:constructor nil)
               (:constructor println-item-type-rich)
               (:constructor println-item-type-plain (&key (type :plain)))
               (:constructor println-item-type-string (&key (type :string-literal)))
               (:conc-name println-item-type->))
  (type :rich :read-only nil :documentation "One of :rich :plain :string-literal"))

;; (println-item-type-rich)
;; (println-item-type-plain)
;; (println-item-type-string)
;; (cl-struct-slot-info 'println-item-type)

(cl-defstruct (println-item-data
               (:constructor println-item-data-create)
               (:copier nil)
               (:conc-name println-item-data->))
  item
  (type (println-item-type-rich)))

;; (println-item-data-create :item "abc")
;;
;; (let ((stamp (println-stamp-data-create :order 1))
;;       (item (println-item-data-create :item "POKLOP")))
;;   (cond ((println-item-data-p item)
;;          (message "HERE"))
;;         (t (message "NOPE %s" item)))
;;  )

(cl-defstruct (println-cluster-data
               (:constructor println-cluster-data-create)
               (:copier nil)
               (:conc-name println-cluster-data->))
  items identifier flags indentation)

(cl-defstruct (print-ln-flags
               (:constructor print-ln-flags-create)
               (:copier nil)
               (:conc-name print-ln-flags->))
  multiline align show-identifier)

(defconst println-default-flags
  (print-ln-flags-create
   :multiline t
   :align nil
   :show-identifier nil))

(defvar println-global-preferences
  (println-preferences-create
   :mode :item
   :flags println-default-flags)
  "Preferences to use when new cluster of print statements is created.")

;; (defun println-table-update-identifier (data identifier)
;;   (setf (println-cluster-data->identifier data)
;;         identifier))

(defun println-data-add-item (data item)
  (setf (println-cluster-data->items data)
        (append (println-cluster-data->items data) (list (println-item-data-create :item item)))))

(defun println-data-update-item (overlay current modified)
  (let ((data (overlay-get overlay 'print-ln)))
    (setf (println-cluster-data->items data)
          (seq-map (lambda (item) (if (eq item current) modified item)) (println-cluster-data->items data)))))

(defun println-table-remove-row (data)
  (let* ((items (println-cluster-data->items data)))
    (if (equal 1 (length items))
        (print-ln-delete-current)
      (setf (println-cluster-data->items data)
            (reverse (cdr (reverse items))))
      (println-refresh data))))

(defun print-ln-toggle-identifier (data)
  (let* ((flags (println-cluster-data->flags data))
         (global (println-preferences->flags println-global-preferences))
         (show-identifier (not (print-ln-flags->show-identifier flags))))
    (setf (print-ln-flags->show-identifier flags) show-identifier
          (print-ln-flags->show-identifier global) show-identifier)))

(defun print-ln-toggle-multiline (data)
  (let* ((flags (println-cluster-data->flags data))
         (global (println-preferences->flags println-global-preferences))
         (multiline (not (print-ln-flags->multiline flags))))
    (setf (print-ln-flags->multiline flags) multiline
          (print-ln-flags->multiline global) multiline)))

(defun print-ln-toggle-align (data)
  (let* ((flags (println-cluster-data->flags data))
         (global (println-preferences->flags println-global-preferences))
         (align (not (print-ln-flags->align flags))))
    (setf (print-ln-flags->align flags) align
          (print-ln-flags->align global) align)))

(defun println-get-data ()
  (get-char-property (point) 'print-ln))

(defmacro println-with-cluster-data (cluster-data body)
  (declare (indent 1) (debug t))
  `(let* ((,cluster-data (println-get-data)))
     (if (println-cluster-data-p ,cluster-data)
         ,body
       (user-error "No cluster data found at the point."))))

(defmacro println-modify-and-refresh-cluster (cluster-data body)
  (declare (indent 1) (debug t))
  `(println-with-cluster-data ,cluster-data
     (progn ,body
            (let ((old-line-num (line-number-at-pos))
                  (old-column (current-column))
                  (old-eol (eolp)))
              (println-refresh ,cluster-data)
              (goto-char (point-min))
              (forward-line (1- old-line-num))
              (goto-char (if old-eol
                             (line-end-position)
                           (min (line-end-position) (+ (line-beginning-position) old-column))))))))

(defun println-get-line-data ()
  (if-let ((item-data (get-text-property (if (eolp) (1- (point)) (point)) 'item-data)))
      item-data
    (let ((line (thing-at-point 'line)))
      (get-text-property (next-single-property-change 0 'item-data line) 'item-data line))))

(defun print-ln-align ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (print-ln-toggle-align data)))

(defun print-ln-multiline ()
  (interactive)
  (let ((data (println-get-data)))
    (print-ln-toggle-multiline data)
    (println-refresh data)))

(defun print-ln-identifier ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (print-ln-toggle-identifier data)))

(defun println-next-kill (data)
  (let ((items (seq-map #'println-item-data->item
                        (seq-filter #'println-item-data-p
                                    (println-cluster-data->items data))))
        (kill-items (println-preprocess-kill-ring)))
    (seq-find (lambda (kill-item)
                (not (member kill-item items)))
              kill-items)))

(defun print-ln-increase ()
  (interactive)
  (let* ((data (println-get-data))
         (next-kill (println-next-kill data)))
    (if (not next-kill)
        (message "No more elements in kill-ring.")
      (println-data-add-item data next-kill)
      (println-refresh data))))

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
  (let ((data (println-get-data)))
    (println-table-remove-row data)))

(defun print-ln-stamp (prefix)
  (interactive "p")

  (println-modify-and-refresh-cluster data
    (if (println-singleline-p data)
        (print-ln-toggle-multiline data)
      (let* ((item-old (println-get-line-data))
             (items (println-cluster-data->items data))
             (index (seq-position items item-old))
             (item-new (cond ((println-item-data-p item-old)
                              (setf (println-preferences->mode println-global-preferences) :stamp)
                              (println-stamp-data-create :order (setq println-counter (1+ println-counter))))
                             ((println-stamp-data-p item-old)
                              (setf (println-preferences->mode println-global-preferences) :item)
                              (println-item-data-create :item (println-next-kill data))))))
        (if (numberp index)
            (setf (nth index items) item-new)
          (error "Index of element '%s' not found" item-old))))))

(defun print-ln-reset ()
  (interactive)
  (setq println-counter 0)
  (message "Println counter reset."))

(defun print-ln-reverse ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (setf (println-cluster-data->items data)
          (reverse (println-cluster-data->items data)))))

(defun println-exclude-current ()
  (interactive)
  (let* ((data (println-get-data))
         (items (println-cluster-data->items data)))
    (message "[println-exclude-current] items: %s" items)))

(defun print-ln-literal/identifier ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (if (println-singleline-p data)
        (print-ln-toggle-multiline data)
      (if (println-align-p data)
          (print-ln-toggle-align data)
        (let* ((line-data (println-get-line-data))
               (type (println-item-data->type line-data)))
          (let ((next-type
                 (pcase (println-item-type->type type)
                   (:rich :string-literal)
                   (:string-literal :plain)
                   (:plain :rich)
                   (_ :rich))))
            (setf (println-item-type->type type) next-type)))))))

(defun print-ln-foreach ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (if (println-stamp-data-p (println-get-line-data))
        (message "Can't apply foreach on stamp data")
      (if (println-singleline-p data)
          (print-ln-toggle-multiline data)
        (when (println-align-p data)
          (print-ln-toggle-align data))
        (let* ((line-data (println-get-line-data))
               (type (println-item-data->type line-data)))
          (let ((next-type
                 (pcase (println-item-type->type type)
                   (:foreach :foreach-delimited)
                   (:foreach-delimited :foreach)
                   (_ :foreach))))
            (setf (println-item-type->type type) next-type)))))))

(defvar print-ln-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'print-ln-commit)
    (define-key map (kbd "RET") 'print-ln-newline)
    (define-key map (kbd "C-c _") 'print-ln-scala-for-comprehension)
    (define-key map (kbd "C-M-r") 'print-ln-reverse)
    ;;(define-key map (kbd "C-M-k") 'println-exclude-current)
    (define-key map (kbd "C-M-i") 'print-ln-reset)
    (define-key map (kbd "C-M-o") 'print-ln-stamp)
    (define-key map (kbd "C-M-s") 'print-ln-identifier)
    (define-key map (kbd "C-M-m") 'print-ln-multiline)
    (define-key map (kbd "C-M-a") 'print-ln-align)
    (define-key map (kbd "C-M-p") 'print-ln-decrease)
    (define-key map (kbd "C-M-n") 'print-ln-increase)
    (define-key map (kbd "C-M-d") 'print-ln-delete-at-point)
    (define-key map (kbd "C-M-x") 'print-ln-literal/identifier)
    (define-key map (kbd "C-M-c") 'print-ln-foreach)
    map)
  "Keymap for print-ln managed region.")

(defun print-ln-newline ()
  (interactive)
  (when-let ((overlay (print-ln-find-overlay-specifying 'print-ln)))
    (if (not (= 1 (- (overlay-end overlay) (point))))
        (let ((original-binding (key-binding (kbd "RET") nil nil (point-min))))
          ;; Call whatever was originally bind by "RET" key.
          (unless (eq original-binding 'print-ln-newline)
            (funcall original-binding)))
      (goto-char (overlay-end overlay))
      (insert "\n")
      (forward-line -1)
      (indent-according-to-mode))))

(defun print-ln-scala-for-comprehension ()
  (interactive)
  (message "[print-ln-scala-for-comprehension] TODO"))

(defun print-ln-commit ()
  (interactive)
  (let* ((data (println-get-data))
         (str (println-render data))
         (indentation (println-cluster-data->indentation data))
         (content (concat indentation str "\n"))
         (point (point)))
    (print-ln-delete-current)
    (insert content)
    (goto-char point)))

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

(defun println-apply-renderer-by-item-type (item identifier indentation)
  (let ((type (println-item-type->type (println-item-data->type item))))
    (pcase type
      (:rich
       (if-let ((basic-renderer (gethash major-mode println-basic-renderer)))
           (funcall basic-renderer (println-item-data->item item) identifier)
         (user-error "No basic renderer found for %s." major-mode)))
      (:string-literal
       (if-let ((string-literal-renderer (gethash major-mode println-basic-literal-string-renderer)))
           (funcall string-literal-renderer (println-item-data->item item))
         (user-error "No string literal renderer found for %s." major-mode)))
      (:plain
       (if-let ((value-renderer (gethash major-mode println-basic-value-renderer)))
           (funcall value-renderer (println-item-data->item item))
         (user-error "No value renderer found for %s." major-mode)))
      ((or :foreach :foreach-delimited)
       (if-let ((foreach-renderer (gethash major-mode println-foreach-renderer)))
           (s-replace "\n"  (format "\n%s" indentation) (funcall foreach-renderer (println-item-data->item item) type))
         (user-error "No foreach renderer found for %s." major-mode))))))

(defun println-to-string (item identifier indentation)
  (cond ((println-item-data-p item)
         (propertize (println-apply-renderer-by-item-type item identifier indentation) 'item-data item))
        ((println-stamp-data-p item)
         (if-let ((stamp-renderer (gethash major-mode println-stamp-renderer)))
             (propertize (funcall stamp-renderer (println-stamp-data->order item)) 'item-data item)
           (user-error "No stamp renderer found for %s." major-mode)))))

(defun println-to-string-aligned (item longest identifier)
  (cond ((println-item-data-p item)
         (when-let ((aligned-renderer (gethash major-mode println-aligned-renderer)))
           (propertize (funcall aligned-renderer (println-item-data->item item) longest identifier) 'item-data item)))
        ((println-stamp-data-p item)
         (when-let ((stamp-renderer (gethash major-mode println-stamp-renderer)))
           (propertize (funcall stamp-renderer (println-stamp-data->order item)) 'item-data item)))))

(defun println-render-single-line (items identifier)
  (when-let ((single-line-renderer (gethash major-mode println-single-line-renderer)))
    ;; TODO
    (funcall single-line-renderer (mapcar #'println-item-data->item (seq-filter #'println-item-data-p items)) identifier)))

(defun println-multiline-p (data)
  (print-ln-flags->multiline (println-cluster-data->flags data)))

(defun println-singleline-p (data)
  (not (println-multiline-p data)))

(defun println-align-p (data)
  (print-ln-flags->align (println-cluster-data->flags data)))

(defun println-show-identifier-p (data)
  (print-ln-flags->show-identifier (println-cluster-data->flags data)))

(defun println-render (data)
  (let ((items (println-cluster-data->items data))
        (identifier (when (println-show-identifier-p data) (println-cluster-data->identifier data)))
        (indentation (println-cluster-data->indentation data)))
    (if (println-multiline-p data)
        (if (println-align-p data)
            (let* ((abc (seq-map (lambda (item) (string-width (or (println-item-data->item item) ""))) (seq-filter #'println-item-data-p items)))
                   (longest (if abc (seq-max abc) "")))
              (mapconcat (lambda (item)
                           (println-to-string-aligned item longest identifier)) items (concat "\n" indentation)))
          (mapconcat (lambda (item)
                       (println-to-string item identifier indentation)) items (concat "\n" indentation)))
      (println-render-single-line items identifier))))

(defun println-search-identifier ()
  (when-let ((identifier-founder (gethash major-mode println-identifier-founder)))
    (funcall identifier-founder)))

(defun println-indentation ()
  (save-excursion
    (forward-line)
    (indent-according-to-mode)
    (if indent-tabs-mode
        (s-pad-left (/ (current-indentation) tab-width) "\t" "\t")
      (s-pad-left (current-indentation) " " " "))))

(defun println-standard (prefix)
  (when (println-get-data)
    (print-ln-delete-current))
  (let* ((kill-ring (println-preprocess-kill-ring))
         (identifier (println-search-identifier))
         (flags (println-preferences->flags println-global-preferences))
         (indentation (println-indentation))
         (data (println-cluster-data-create :items nil :identifier identifier :flags flags :indentation indentation)))
    (dotimes (item (min prefix (length kill-ring)))
      (let ((current (nth item kill-ring)))
        (println-data-add-item data current)))
    (forward-line)
    (print-ln-render data)))

(defun print-ln-add-stamp ()
  (when (println-get-data)
    (print-ln-delete-current))
  (let* ((identifier (println-search-identifier))
         (flags (println-preferences->flags println-global-preferences))
         (indentation (println-indentation))
         (data (println-cluster-data-create
                :items (list (println-stamp-data-create :order (setq println-counter (1+ println-counter))))
                :identifier identifier
                :flags flags
                :indentation indentation)))
    (forward-line)
    (print-ln-render data)))

(defun println-insert-after (prefix)
  (interactive "p")
  (pcase (println-preferences->mode println-global-preferences)
    (:item (println-standard prefix))
    (:stamp (print-ln-add-stamp))
    (_ (error "Unknown mode %s" (println-preferences->mode println-global-preferences)))))

(defun println-refresh (data)
  (print-ln-delete-current)
  (print-ln-render data))

(defun print-ln-render (data)
  (let ((str (println-render data)))
    (print-ln-render-content data str)))

(defun print-ln-render-content (data content)
                                        ;(indent-according-to-mode)
  (let* ((indentation (println-cluster-data->indentation data))
         (content (concat indentation content "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'print-ln-diff-hunk-heading)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'print-ln-p t)
      (overlay-put overlay 'keymap print-ln-keymap)
      (overlay-put overlay 'print-ln data)
      (backward-char))))

(defun println-editable (item)
  (propertize item :print-ln-current item))

(defun println-register-major-mode (major-mode basic literal-string value aligned single identifier stamp foreach)
  (puthash major-mode basic println-basic-renderer)
  (puthash major-mode literal-string println-basic-literal-string-renderer)
  (puthash major-mode value println-basic-value-renderer)
  (puthash major-mode aligned println-aligned-renderer)
  (puthash major-mode single println-single-line-renderer)
  (puthash major-mode identifier println-identifier-founder)
  (puthash major-mode stamp println-stamp-renderer)
  (puthash major-mode foreach println-foreach-renderer))

(provide 'println-debugger-common)
