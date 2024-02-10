;;; println-debugger-common.el --- Quick generating of println expressions -*- lexical-binding: t; -*-

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

(defface println-diff-hunk-heading
  '((((class color) (background dark))
     :extend t
     :background "DarkSlateBlue"))
  "Face for lines controlled by println."
  :group 'println-faces)

(defvar println-basic-renderer (make-hash-table))
(defvar println-basic-value-renderer (make-hash-table))
(defvar println-basic-literal-string-renderer (make-hash-table))
(defvar println-aligned-renderer (make-hash-table))
(defvar println-single-line-renderer (make-hash-table))
(defvar println-identifier-founder (make-hash-table))
(defvar println-stamp-renderer (make-hash-table))
(defvar println-foreach-renderer (make-hash-table))

(defvar println-counter 0)

(cl-defstruct (println-revert-info
               (:constructor println-revert-info-create)
               (:copier nil)
               (:conc-name println-revert-info->))
  data)

(defvar println-revert-data)

(defun println-demo-data ()
  (let* ((kill-ring '("hi" "there"))
         (identifier "Demo.init")
         (flags (println-flags-create :multiline t :align t :show-identifier nil))
         (data (println-cluster-data-create :items nil :identifier identifier :flags flags :indentation nil)))
    (dotimes (item (length kill-ring))
      (let ((current (nth item kill-ring)))
        (println-data-add-item data current)))
    data))

(defun println-demo ()
  (interactive)
  (forward-line)
  (println-write (println-demo-data)))

(cl-defstruct (println-preferences
               (:constructor println-preferences-create)
               (:copier nil)
               (:conc-name println-preferences->))
  ;; can be either :item or :stamp
  mode
  ;; local preferences for cluster of println statements
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

(cl-defstruct (println-flags
               (:constructor println-flags-create)
               (:copier nil)
               (:conc-name println-flags->))
  multiline align show-identifier)

(defconst println-default-flags
  (println-flags-create
   :multiline t
   :align nil
   :show-identifier nil))

(defvar println-global-preferences
  (println-preferences-create
   :mode :item
   :flags println-default-flags)
  "Preferences to use when new cluster of print statements is created.")

(defun println-data-add-item (data item)
  (setf (println-cluster-data->items data)
        (append (println-cluster-data->items data) (list (println-item-data-create :item item)))))

(defun println-data-update-item (overlay current modified)
  (let ((data (overlay-get overlay 'print-ln)))
    (setf (println-cluster-data->items data)
          (seq-map (lambda (item) (if (eq item current) modified item)) (println-cluster-data->items data)))))

(defun println-table-remove-row (data)
  (let* ((items (println-cluster-data->items data)))
    (if (= 1 (length items))
        (progn (println-delete-current)
               (forward-line -1))
      (setf (println-cluster-data->items data)
            (reverse (cdr (reverse items))))
      (println-refresh data))))

(defun println-toggle-identifier (data)
  (let* ((flags (println-cluster-data->flags data))
         (global (println-preferences->flags println-global-preferences))
         (show-identifier (not (println-flags->show-identifier flags))))
    (setf (println-flags->show-identifier flags) show-identifier
          (println-flags->show-identifier global) show-identifier)))

(defun println-toggle-multiline (data)
  (let* ((flags (println-cluster-data->flags data))
         (global (println-preferences->flags println-global-preferences))
         (multiline (not (println-flags->multiline flags))))
    (setf (println-flags->multiline flags) multiline
          (println-flags->multiline global) multiline)))

(defun println-toggle-align (data)
  (let* ((flags (println-cluster-data->flags data))
         (global (println-preferences->flags println-global-preferences))
         (align (not (println-flags->align flags))))
    (setf (println-flags->align flags) align
          (println-flags->align global) align)))

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

(defun println-align ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (println-toggle-align data)))

(defun println-multiline ()
  (interactive)
  (let ((data (println-get-data)))
    (println-toggle-multiline data)
    (println-refresh data)))

(defun println-show-identifier ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (println-toggle-identifier data)))

(defun println-next-kill (data)
  (let ((items (seq-map #'println-item-data->item
                        (seq-filter #'println-item-data-p
                                    (println-cluster-data->items data))))
        (kill-items (println-preprocess-kill-ring)))
    (seq-find (lambda (kill-item)
                (not (member kill-item items)))
              kill-items)))

(defun println-increase ()
  (interactive)
  (let* ((data (println-get-data))
         (next-kill (println-next-kill data)))
    (if (not next-kill)
        (message "No more elements in kill-ring")
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

(defun println-decrease ()
  (interactive)
  (let ((data (println-get-data)))
    (println-table-remove-row data)))

(defun println-stamp (prefix)
  (interactive "p")

  (println-modify-and-refresh-cluster data
    (if (println-singleline-p data)
        (println-toggle-multiline data)
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

(defun println-reset ()
  (interactive)
  (setq println-counter 0)
  (message "Println counter reset"))

(defun println-reverse ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (progn
      (let* ((items (println-cluster-data->items data))
             (items-with-index (seq-map-indexed (lambda (elt idx)
                                 (list idx elt))
                               items))
             (indexed-stamps-only (seq-filter  (lambda (s) (println-stamp-data-p (cadr s))) items-with-index))
             (items-only (seq-filter  #'println-item-data-p items))
             (reversed (reverse items-only)))
        (dolist (element indexed-stamps-only)
          (let ((stamp-index (car element))
                (stamp (cadr element)))
             (push stamp (nthcdr stamp-index reversed))))
        (setf (println-cluster-data->items data) reversed)))))

(defun println-exclude-current ()
  (interactive)
  (let* ((data (println-get-data))
         (items (println-cluster-data->items data)))
    (message "[println-exclude-current] items: %s" items)))

(defun println-literal/identifier ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (if (println-singleline-p data)
        (println-toggle-multiline data)
      (if (println-align-p data)
          (println-toggle-align data)
        (let* ((line-data (println-get-line-data))
               (type (println-item-data->type line-data)))
          (let ((next-type
                 (pcase (println-item-type->type type)
                   (:rich :string-literal)
                   (:string-literal :plain)
                   (:plain :rich)
                   (_ :rich))))
            (setf (println-item-type->type type) next-type)))))))

(defun println-foreach ()
  (interactive)
  (println-modify-and-refresh-cluster data
    (if (println-singleline-p data)
        (println-toggle-multiline data)
      (if (println-stamp-data-p (println-get-line-data))
          (message "Can't apply foreach on stamp data")
        (when (println-align-p data)
          (println-toggle-align data))
        (let* ((line-data (println-get-line-data))
               (type (println-item-data->type line-data)))
          (let ((next-type
                 (pcase (println-item-type->type type)
                   (:foreach :foreach-delimited)
                   (:foreach-delimited :foreach)
                   (_ :foreach))))
            (setf (println-item-type->type type) next-type)))))))

(defvar println-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'println-commit)
    (define-key map (kbd "RET") 'println-newline)
    (define-key map (kbd "C-c _") 'println-scala-for-comprehension)
    (define-key map (kbd "C-M-r") 'println-reverse)
    ;;(define-key map (kbd "C-M-k") 'println-exclude-current)
    (define-key map (kbd "C-M-i") 'println-reset)
    (define-key map (kbd "C-M-o") 'println-stamp)
    (define-key map (kbd "C-M-s") 'println-show-identifier)
    (define-key map (kbd "C-M-m") 'println-multiline)
    (define-key map (kbd "C-M-a") 'println-align)
    (define-key map (kbd "C-M-p") 'println-decrease)
    (define-key map (kbd "C-M-n") 'println-increase)
    (define-key map (kbd "C-M-d") 'println-delete-at-point)
    (define-key map (kbd "C-M-x") 'println-literal/identifier)
    (define-key map (kbd "C-M-c") 'println-foreach)
    map)
  "Keymap for println managed region.")

(defun println-newline ()
  (interactive)
  (when-let ((overlay (println-find-overlay-specifying 'print-ln)))
    (if (not (= 1 (- (overlay-end overlay) (point))))
        (let ((original-binding (key-binding (kbd "RET") nil nil (point-min))))
          ;; Call whatever was originally bind by "RET" key.
          (unless (eq original-binding 'println-newline)
            (funcall original-binding)))
      (goto-char (overlay-end overlay))
      (insert "\n")
      (forward-line -1)
      (indent-according-to-mode))))

(defun println-scala-for-comprehension ()
  (interactive)
  (message "[println-scala-for-comprehension] TODO"))

(defun println-commit ()
  (interactive)
  (let* ((data (println-get-data))
         (str (println-render data))
         (indentation (println-cluster-data->indentation data))
         (content (concat indentation str "\n"))
         (point (point)))
    (println-delete-current)
    (insert content)
    (goto-char point)))

(defun println-delete-at-point ()
  (interactive)
  (println-delete-current))

(defun println-find-overlay-specifying (prop)
  (let ((overlays (overlays-at (point))))
    (seq-find (lambda (overlay) (overlay-get overlay prop)) overlays)))

(defun println-delete-current ()
  (when-let ((overlay (println-find-overlay-specifying 'print-ln)))
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
    (funcall single-line-renderer (mapcar #'println-item-data->item (seq-filter #'println-item-data-p items)) identifier)))

(defun println-multiline-p (data)
  (println-flags->multiline (println-cluster-data->flags data)))

(defun println-singleline-p (data)
  (not (println-multiline-p data)))

(defun println-align-p (data)
  (println-flags->align (println-cluster-data->flags data)))

(defun println-show-identifier-p (data)
  (println-flags->show-identifier (println-cluster-data->flags data)))

(defun println-render (data)
  (let ((items (println-cluster-data->items data))
        (identifier (when (println-show-identifier-p data) (println-cluster-data->identifier data)))
        (indentation (println-cluster-data->indentation data)))
    (if (println-multiline-p data)
        (if (println-align-p data)
            (let* ((abc (seq-map (lambda (item) (string-width (or (println-item-data->item item) ""))) (seq-filter #'println-item-data-p items)))
                   (longest (if abc (seq-max abc) "")))
              (mapconcat (lambda (item)
                           (println-to-string-aligned item longest identifier))
                         items (concat "\n" indentation)))
          (mapconcat (lambda (item)
                       (println-to-string item identifier indentation))
                     items (concat "\n" indentation)))
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
  (if (null kill-ring)
      (message "Nothing to print, kill-ring is empty")
    (when (println-get-data)
      (println-delete-current))
    (let* ((kill-ring (println-preprocess-kill-ring))
           (identifier (println-search-identifier))
           (flags (println-preferences->flags println-global-preferences))
           (indentation (println-indentation))
           (data (println-cluster-data-create :items nil :identifier identifier :flags flags :indentation indentation)))
      (dotimes (item (min prefix (length kill-ring)))
        (let ((current (nth item kill-ring)))
          (println-data-add-item data current)))
      (forward-line)
      (println-write data))))

(defun println-add-stamp ()
  (when (println-get-data)
    (println-delete-current))
  (let* ((identifier (println-search-identifier))
         (flags (println-preferences->flags println-global-preferences))
         (indentation (println-indentation))
         (data (println-cluster-data-create
                :items (list (println-stamp-data-create :order (setq println-counter (1+ println-counter))))
                :identifier identifier
                :flags flags
                :indentation indentation)))
    (forward-line)
    (println-write data)))

(defun println-insert-after (prefix)
  (interactive "p")
  (pcase (println-preferences->mode println-global-preferences)
    (:item (println-standard prefix))
    (:stamp (println-add-stamp))
    (_ (error "Unknown mode %s" (println-preferences->mode println-global-preferences)))))

(defun println-refresh (data)
  (println-delete-current)
  (println-write data))

(defun println-write (data)
  (let ((str (println-render data)))
    (println-render-content data str)))

(defun println-render-content (data content)
                                        ;(indent-according-to-mode)
  (let* ((indentation (println-cluster-data->indentation data))
         (content (concat indentation content "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'println-diff-hunk-heading)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap println-keymap)
      (overlay-put overlay 'print-ln data)
      (backward-char))))

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
