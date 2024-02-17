;;; println-debugger-common.el --- Quick generating of println expressions -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(eval-when-compile (require 'subr-x))

(defface println-cluster-overlay
  '((((class color) (background dark))
     :extend t
     :background "DarkSlateBlue"))
  "Face for lines controlled by println."
  :group 'println-faces)

(defvar println-basic-renderer (make-hash-table))
(defvar println-basic-value-renderer (make-hash-table))
(defvar println-basic-literal-string-renderer (make-hash-table))
(defvar println-foreach-renderer (make-hash-table))
(defvar println-aligned-renderer (make-hash-table))
(defvar println-single-line-renderer (make-hash-table))
(defvar println-identifier-founder (make-hash-table))
(defvar println-stamp-renderer (make-hash-table))

(cl-defstruct (println-preferences
               (:constructor println-preferences-create)
               (:copier nil)
               (:conc-name println-preferences->))
  ;; number used by the :stamp mode
  counter
  ;; can be either :killed-text or :stamp
  mode
  ;; local preferences for cluster of println statements
  flags)

(cl-defstruct (println-stamp
               (:constructor println-stamp-create)
               (:copier nil)
               (:conc-name println-stamp->))
  order)

(cl-defstruct (println-killed-text
               (:constructor println-killed-text-create)
               (:copier nil)
               (:conc-name println-killed-text->))
  killed-text
  (type :rich :read-only nil :documentation "One of :rich :plain :string-literal :foreach or :foreach-delimited"))

(cl-defstruct (println-cluster
               (:constructor println-cluster-create)
               (:copier nil)
               (:conc-name println-cluster->))
  ;; list of println-killed-texts and/or println-stamps
  items
  ;; name of function/defun wrapping current println cluster. It is determined by mode specific function.
  identifier
  ;; cluster local flags
  flags
  ;; indentation of the cluster
  indentation)

(cl-defstruct (println-flags
               (:constructor println-flags-create)
               (:conc-name println-flags->))
  multiline align show-identifier)

(defvar println-global-preferences
  (println-preferences-create
   ;; Counter for :stamp mode.
   :counter 0
   ;; Either :killed-text or :stamp. Determines mode to start new println cluster in.
   :mode :killed-text
   :flags (println-flags-create
           :multiline t
           :align nil
           :show-identifier nil))
  "Preferences to use when creating new cluster of print statements.")

(defun println-data-add-item (cluster-data item)
  (setf (println-cluster->items cluster-data)
        (append (println-cluster->items cluster-data) (list (println-killed-text-create :killed-text item)))))

(defun println-toggle-multiline (data)
  (let* ((flags (println-cluster->flags data))
         (global (println-preferences->flags println-global-preferences))
         (multiline (not (println-flags->multiline flags))))
    (setf (println-flags->multiline flags) multiline
          (println-flags->multiline global) multiline)))

(defun println-toggle-align (data)
  (let* ((flags (println-cluster->flags data))
         (global (println-preferences->flags println-global-preferences))
         (align (not (println-flags->align flags))))
    (setf (println-flags->align flags) align
          (println-flags->align global) align)))

(defun println-get-line-data ()
  "Returns item data for current line. Returns nil when line has no
item data, for example when rendering println cluster as single line."
  (if-let ((println-line-data (get-text-property (if (eolp) (1- (point)) (point)) 'println-line-data)))
      println-line-data
    (if-let ((line (thing-at-point 'line))
             (prop-position (next-single-property-change 0 'println-line-data line)))
        (get-text-property prop-position 'println-line-data line))))

(defun println-get-cluster-data ()
  (get-char-property (point) 'println-cluster))

(defmacro println-with-cluster-data (cluster-data line-data body)
  (declare (indent 2) (debug t))
  `(let* ((,cluster-data (println-get-cluster-data))
          (,line-data (println-get-line-data)))
     (if (println-cluster-p ,cluster-data)
         ,body
       (user-error "No cluster data found at the point."))))

(defmacro println-modify-and-refresh-cluster (cluster-data line-data body)
  (declare (indent 2) (debug t))
  `(println-with-cluster-data ,cluster-data ,line-data
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

(defmacro println-modify-and-refresh-cluster-point-at-end (cluster-data body)
  (declare (indent 1) (debug t))
  `(println-with-cluster-data ,cluster-data ignore-line-data
     (progn ,body
            (println-refresh ,cluster-data))))

(defun println-align ()
  (interactive)
  (println-modify-and-refresh-cluster cluster-data line-data
    (println-toggle-align cluster-data)))

(defun println-multiline ()
  (interactive)
  (println-modify-and-refresh-cluster-point-at-end cluster-data
    (println-toggle-multiline cluster-data)))

(defun println-show-identifier ()
  (interactive)
  (println-modify-and-refresh-cluster cluster-data line-data
    (let* ((flags (println-cluster->flags cluster-data))
           (global (println-preferences->flags println-global-preferences))
           (show-identifier (not (println-flags->show-identifier flags))))
      (setf (println-flags->show-identifier flags) show-identifier
            (println-flags->show-identifier global) show-identifier))))

(defun println-next-kill (cluster-data)
  (let ((items (seq-map #'println-killed-text->killed-text
                        (seq-filter #'println-killed-text-p
                                    (println-cluster->items cluster-data))))
        (kill-items (println-preprocess-kill-ring)))
    (seq-find (lambda (kill-item)
                (not (member kill-item items)))
              kill-items)))

(defun println-preprocess-kill-ring ()
  (let ((result))
    (dolist (element kill-ring)
      (let ((element (string-clean-whitespace element)))
        (unless (or
                 (string-blank-p element)
                 (string-match-p "[\r\n]+" element))
          (setq result (cons element result)))))
    (reverse result)))

(defun println-increase ()
  (interactive)
  (println-modify-and-refresh-cluster-point-at-end cluster-data
    (let ((next-kill (println-next-kill cluster-data)))
      (if (not next-kill)
          (message "No more elements in kill-ring")
        (println-data-add-item cluster-data next-kill)))))

(defun println-decrease ()
  (interactive)
  (println-modify-and-refresh-cluster-point-at-end cluster-data
    (setf (println-cluster->items cluster-data)
          (reverse (cdr (reverse (println-cluster->items cluster-data)))))))

(defun println-stamp ()
  (interactive)
  (println-modify-and-refresh-cluster cluster-data line-data
    (if (println-singleline-p cluster-data)
        (println-toggle-multiline cluster-data)
      (let* ((items (println-cluster->items cluster-data))
             (index (seq-position items line-data))
             (item-new (cond ((println-killed-text-p line-data)
                              (setf (println-preferences->mode println-global-preferences) :stamp)
                              (println-stamp-create :order
                                                    (setf (println-preferences->counter println-global-preferences)
                                                          (1+ (println-preferences->counter println-global-preferences)))))
                             ((println-stamp-p line-data)
                              (setf (println-preferences->mode println-global-preferences) :killed-text)
                              (println-killed-text-create :killed-text (println-next-kill cluster-data))))))
        (if (numberp index)
            (setf (nth index items) item-new)
          (error "Index of element '%s' not found" line-data))))))

(defun println-reset ()
  (interactive)
  (setf (println-preferences->counter println-global-preferences) 0)
  (message "println counter reset to 1"))

(defun println-reverse ()
  (interactive)
  (println-modify-and-refresh-cluster data line-data
    (progn
      (let* ((items (println-cluster->items data))
             (items-with-index (seq-map-indexed (lambda (elt idx)
                                                  (list idx elt))
                                                items))
             (indexed-stamps-only (seq-filter  (lambda (s) (println-stamp-p (cadr s))) items-with-index))
             (items-only (seq-filter  #'println-killed-text-p items))
             (reversed (reverse items-only)))
        (dolist (element indexed-stamps-only)
          (let ((stamp-index (car element))
                (stamp (cadr element)))
            (push stamp (nthcdr stamp-index reversed))))
        (setf (println-cluster->items data) reversed)))))

(defun println-literal/identifier ()
  (interactive)
  (println-modify-and-refresh-cluster data line-data
    (if (println-singleline-p data)
        (println-toggle-multiline data)
      (if (println-stamp-p line-data)
          (message "Can't apply literal/identifier on stamp data")
        (when (println-align-p data)
          (println-toggle-align data))
        (setf (println-killed-text->type line-data)
              (pcase (println-killed-text->type line-data)
                (:rich :string-literal)
                (:string-literal :plain)
                (:plain :rich)
                (_ :rich)))))))

(defun println-foreach ()
  (interactive)
  (println-modify-and-refresh-cluster data line-data
    (if (println-singleline-p data)
        (println-toggle-multiline data)
      (if (println-stamp-p line-data)
          (message "Can't apply foreach on stamp data")
        (when (println-align-p data)
          (println-toggle-align data))
        (setf (println-killed-text->type line-data)
              (pcase (println-killed-text->type line-data)
                (:foreach :foreach-delimited)
                (:foreach-delimited :foreach)
                (_ :foreach)))))))

(defvar println-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'println-commit)
    (define-key map (kbd "RET") 'println-newline)
    (define-key map (kbd "C-M-r") 'println-reverse)
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
  (when-let ((overlay (println-find-overlay-specifying 'println-cluster)))
    (if (not (= 1 (- (overlay-end overlay) (point))))
        (let ((original-binding (key-binding (kbd "RET") nil nil (point-min))))
          ;; Call whatever was originally bind by "RET" key.
          (unless (eq original-binding 'println-newline)
            (funcall original-binding)))
      (goto-char (overlay-end overlay))
      (insert "\n")
      (forward-line -1)
      (indent-according-to-mode))))

(defun println-commit ()
  (interactive)
  (let* ((data (println-get-cluster-data))
         (str (println-render data))
         (indentation (println-cluster->indentation data))
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
  (when-let ((overlay (println-find-overlay-specifying 'println-cluster)))
    (delete-region (overlay-start overlay) (overlay-end overlay))))

(defun println-safe-string (str)
  (replace-regexp-in-string "\"" "" str))

(defun println-identifier (identifier)
  (if identifier
      (format "[%s] " identifier)
    ""))

(defun println-renderer (type)
  (let ((renderer
         (gethash major-mode (pcase type
                               (:rich println-basic-renderer)
                               (:plain println-basic-value-renderer)
                               (:string-literal println-basic-literal-string-renderer)
                               ((or :foreach :foreach-delimited) println-foreach-renderer)
                               (:align println-aligned-renderer)
                               (:single println-single-line-renderer)
                               (:stamp println-stamp-renderer)))))
    (unless renderer
      (user-error "No %s renderer found for %s." type major-mode))
    renderer))

(defun println-apply-renderer-by-item-type (item identifier indentation)
  (let* ((type (println-killed-text->type item))
         (killed-text (println-killed-text->killed-text item))
         (renderer (println-renderer type)))
    (pcase type
      (:rich
       (funcall renderer killed-text identifier))
      (:plain
       (funcall renderer killed-text))
      (:string-literal
       (funcall renderer killed-text))
      ((or :foreach :foreach-delimited)
       (replace-regexp-in-string "\n" (format "\n%s" indentation) (funcall renderer killed-text type))))))

(defun println-to-string (item identifier indentation)
  (cond ((println-killed-text-p item)
         (propertize (println-apply-renderer-by-item-type item identifier indentation) 'println-line-data item))
        ((println-stamp-p item)
         (propertize (funcall (println-renderer :stamp) (println-stamp->order item)) 'println-line-data item))))

(defun println-to-string-aligned (item longest identifier)
  (cond ((println-killed-text-p item)
         (propertize (funcall (println-renderer :align) (println-killed-text->killed-text item) longest identifier) 'println-line-data item))
        ((println-stamp-p item)
         (propertize (funcall (println-renderer :stamp) (println-stamp->order item)) 'println-line-data item))))

(defun println-render-single-line (items identifier)
  (funcall (println-renderer :single) (mapcar #'println-killed-text->killed-text (seq-filter #'println-killed-text-p items)) identifier))

(defun println-multiline-p (data)
  (println-flags->multiline (println-cluster->flags data)))

(defun println-singleline-p (data)
  "Returns t if items from DATA are printed on a single line, otherwise nil"
  (not (println-multiline-p data)))

(defun println-align-p (data)
  (println-flags->align (println-cluster->flags data)))

(defun println-show-identifier-p (data)
  (println-flags->show-identifier (println-cluster->flags data)))

(defun println-render (data)
  (let ((items (println-cluster->items data))
        (identifier (when (println-show-identifier-p data) (println-cluster->identifier data)))
        (indentation (println-cluster->indentation data)))
    (if (println-multiline-p data)
        (if (println-align-p data)
            (let* ((widths (seq-map (lambda (item) (string-width (or (println-killed-text->killed-text item) ""))) (seq-filter #'println-killed-text-p items)))
                   (longest (if widths (seq-max widths) 0)))
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

;; Get indentations of a new line below current line
(defun println-indentation ()
  (save-excursion
    (goto-char (line-end-position))
    (newline 1)
    (indent-according-to-mode)
    (prog1 (if indent-tabs-mode
               (make-string (/ (current-indentation) tab-width) ?\t)
             (make-string (current-indentation) ?\ ))
      (delete-region (1- (line-beginning-position)) (point)))))

(defun println-standard (prefix)
  (if (not kill-ring)
      (message "Nothing to print, kill-ring is empty")
    (when (println-get-cluster-data)
      (println-delete-current))
    (let* ((kill-ring (println-preprocess-kill-ring))
           (identifier (println-search-identifier))
           (global-flags (println-preferences->flags println-global-preferences))
           (flags (copy-println-flags global-flags))
           (indentation (println-indentation))
           (cluster-data (println-cluster-create :items nil :identifier identifier :flags flags :indentation indentation)))
      (dotimes (item (min prefix (length kill-ring)))
        (let ((current (nth item kill-ring)))
          (println-data-add-item cluster-data current)))
      (forward-line)
      (println-write cluster-data))))

(defun println-add-stamp ()
  (when (println-get-cluster-data)
    (println-delete-current))
  (let* ((identifier (println-search-identifier))
         (global-flags (println-preferences->flags println-global-preferences))
         (flags (copy-println-flags global-flags))
         (indentation (println-indentation))
         (cluster-data (println-cluster-create
                        :items (list (println-stamp-create :order
                                                           (setf (println-preferences->counter println-global-preferences)
                                                                 (1+ (println-preferences->counter println-global-preferences)))))
                        :identifier identifier
                        :flags flags
                        :indentation indentation)))
    (forward-line)
    (println-write cluster-data)))

;;;###autoload
(defun println-insert (prefix)
  (interactive "p")
  (pcase (println-preferences->mode println-global-preferences)
    (:killed-text (println-standard prefix))
    (:stamp (println-add-stamp))
    (_ (error "Unknown mode %s" (println-preferences->mode println-global-preferences)))))

(defun println-refresh (data)
  (println-delete-current)
  (if (println-cluster->items data)
      (println-write data)
    ;; No items to render, go to previous line
    (forward-line -1)
    (indent-according-to-mode)))

(defun println-write (data)
  (let ((str (println-render data)))
    (println-render-content data str)))

(defun println-render-content (data content)
  (let* ((indentation (println-cluster->indentation data))
         (content (concat indentation content "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'println-cluster-overlay)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap println-keymap)
      (overlay-put overlay 'println-cluster data)
      (backward-char))))

(defun println-register-major-mode (major-mode basic literal-string value foreach aligned single identifier stamp)
  (puthash major-mode basic println-basic-renderer)
  (puthash major-mode literal-string println-basic-literal-string-renderer)
  (puthash major-mode value println-basic-value-renderer)
  (puthash major-mode foreach println-foreach-renderer)
  (puthash major-mode aligned println-aligned-renderer)
  (puthash major-mode single println-single-line-renderer)
  (puthash major-mode identifier println-identifier-founder)
  (puthash major-mode stamp println-stamp-renderer))

(provide 'println-debugger-common)
