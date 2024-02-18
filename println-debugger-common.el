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
  indentation
  ;; list of kill-texts to ignore
  ignore-list)

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

(defun println-data-add-item (cdata item)
  (setf (println-cluster->items cdata)
        (append (println-cluster->items cdata) (list (println-killed-text-create :killed-text item)))))

(defun println-toggle-multiline (cdata)
  (let* ((flags (println-cluster->flags cdata))
         (global (println-preferences->flags println-global-preferences))
         (multiline (not (println-flags->multiline flags))))
    (setf (println-flags->multiline flags) multiline
          (println-flags->multiline global) multiline)))

(defun println-toggle-align (cdata)
  (let* ((flags (println-cluster->flags cdata))
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

(defmacro println-with-cluster-data (cdata line-data body)
  (declare (indent 2) (debug t))
  `(let* ((,cdata (println-get-cluster-data))
          (,line-data (println-get-line-data)))
     (if (println-cluster-p ,cdata)
         ,body
       (user-error "No cluster data found at the point."))))

(defmacro println-modify-and-refresh-cluster (cdata line-data body)
  (declare (indent 2) (debug t))
  `(println-with-cluster-data ,cdata ,line-data
     (progn ,body
            (let ((old-line-num (line-number-at-pos))
                  (old-column (current-column))
                  (old-eol (eolp)))
              (println-refresh ,cdata)
              (goto-char (point-min))
              (forward-line (1- old-line-num))
              (goto-char (if old-eol
                             (line-end-position)
                           (min (line-end-position) (+ (line-beginning-position) old-column))))))))

(defmacro println-modify-and-refresh-cluster-point-at-end (cdata body &rest rest)
  (declare (indent 1) (debug t))
  `(println-with-cluster-data ,cdata ignore-line-data
     (progn ,body
            ,@rest
            (println-refresh ,cdata))))

(defun println-align ()
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (println-toggle-align cdata)))

(defun println-multiline ()
  (interactive)
  (println-modify-and-refresh-cluster-point-at-end cdata
    (unless (println-no-kill-text cdata)
      (println-toggle-multiline cdata))))

(defun println-show-identifier ()
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (let* ((flags (println-cluster->flags cdata))
           (global (println-preferences->flags println-global-preferences))
           (show-identifier (not (println-flags->show-identifier flags))))
      (setf (println-flags->show-identifier flags) show-identifier
            (println-flags->show-identifier global) show-identifier))))

(defun println-next-kill (cdata)
  (let ((items (seq-map #'println-killed-text->killed-text
                        (seq-filter #'println-killed-text-p
                                    (println-cluster->items cdata))))
        (ignore-list (println-cluster->ignore-list cdata))
        (kill-items (println-preprocess-kill-ring)))
    (seq-find (lambda (kill-item)
                (and
                 (not (member kill-item items))
                 (not (member kill-item ignore-list))))
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

(defun println-add-next-kill (cdata)
  (let ((next-kill (println-next-kill cdata)))
    (if (not next-kill)
        (message "No more elements in kill-ring")
      (println-data-add-item cdata next-kill))))

(defun println-increase ()
  (interactive)
  (println-modify-and-refresh-cluster-point-at-end cdata
    (println-add-next-kill cdata)))

(defun println-force-multiline (cdata)
  "Set multiline flag of `cdata' to t"
  (setf (println-flags->multiline (println-cluster->flags cdata)) t))

(defun println-decrease ()
  (interactive)
  (println-modify-and-refresh-cluster-point-at-end cdata
    (setf (println-cluster->items cdata)
          (reverse (cdr (reverse (println-cluster->items cdata)))))
    (when (println-no-kill-text cdata)
      (println-force-multiline cdata))))

(defun println-stamp ()
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (if (println-singleline-p cdata)
        (println-toggle-multiline cdata)
      (let* ((items (println-cluster->items cdata))
             (index (seq-position items line-data))
             (item-new (cond ((println-killed-text-p line-data)
                              (setf (println-preferences->mode println-global-preferences) :stamp)
                              (println-stamp-create :order
                                                    (setf (println-preferences->counter println-global-preferences)
                                                          (1+ (println-preferences->counter println-global-preferences)))))
                             ((println-stamp-p line-data)
                              (let ((next-kill (println-next-kill cdata)))
                                (if (not next-kill)
                                    (message "Nothing to replace stamp with, kill-ring is empty")
                                  (setf (println-preferences->mode println-global-preferences) :killed-text)
                                  (println-killed-text-create :killed-text (println-next-kill cdata))))))))
        (when (and (numberp index)
                   (or (println-stamp-p item-new)
                       (println-killed-text-p item-new)))
          (setf (nth index items) item-new))))))

(defun println-reset ()
  (interactive)
  (setf (println-preferences->counter println-global-preferences) 0)
  (message "println counter reset to 1"))

(defun println-reverse ()
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (let* ((items (println-cluster->items cdata))
           (items-with-index (seq-map-indexed (lambda (elt idx) (list idx elt)) items))
           (indexed-stamps-only (seq-filter  (lambda (s) (println-stamp-p (cadr s))) items-with-index))
           (items-only (seq-filter  #'println-killed-text-p items))
           (reversed (reverse items-only)))
      (dolist (element indexed-stamps-only)
        (let ((stamp-index (car element))
              (stamp (cadr element)))
          (push stamp (nthcdr stamp-index reversed))))
      (setf (println-cluster->items cdata) reversed))))

(defun println-literal/identifier ()
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (if (println-singleline-p cdata)
        (println-toggle-multiline cdata)
      (if (println-stamp-p line-data)
          (message "Can't apply literal/identifier on stamp sata")
        (when (println-align-p cdata)
          (println-toggle-align cdata))
        (setf (println-killed-text->type line-data)
              (pcase (println-killed-text->type line-data)
                (:rich :string-literal)
                (:string-literal :plain)
                (:plain :rich)
                (_ :rich)))))))

(defun println-foreach ()
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (if (println-singleline-p cdata)
        (println-toggle-multiline cdata)
      (if (println-stamp-p line-data)
          (message "Can't apply foreach on stamp data")
        (when (println-align-p cdata)
          (println-toggle-align cdata))
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
    (define-key map (kbd "C-M-k") 'println-ignore)
    (define-key map (kbd "C-M-d") 'println-delete-at-point)
    (define-key map (kbd "C-M-x") 'println-literal/identifier)
    (define-key map (kbd "C-M-c") 'println-foreach)
    map)
  "Keymap for println cluster text overlay.")

(defun println-newline ()
  "When at the end of managed println cluster, pressing RET opens
new line and println cluster's text overlay does not move."
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
  (let* ((cdata (println-get-cluster-data))
         (str (println-render cdata))
         (indentation (println-cluster->indentation cdata))
         (content (concat indentation str "\n"))
         (point (point)))
    (println-delete-current)
    (insert content)
    (goto-char point)))

(defun println-count-kill-text (cdata)
  "Returns count of kill-text items in `cdata'."
  (seq-count (lambda (elt) (println-killed-text-p elt))
             (println-cluster->items cdata)))

(defun println-single-kill-text (cdata)
  "Returns t if there is only single kill-text item in `cdata' otherwise nil."
  (= 1 (println-count-kill-text cdata)))

(defun println-no-kill-text (cdata)
  "Returns t if there is no kill-text item in `cdata' otherwise nil."
  (= 0 (println-count-kill-text cdata)))

(defun println-ignore ()
  "Add line-data of current line to the ignored list so that it is not rendered.
Useful when kill-ring contains garbage data which should not be printed."
  (interactive)
  (println-modify-and-refresh-cluster cdata line-data
    (if (println-singleline-p cdata)
        (println-toggle-multiline cdata)
      (cond ((println-stamp-p line-data)
             (message "Cannot ignore stamp"))
            ((println-killed-text-p line-data)
             (let* ((current-line-item-position (1+ (seq-position (println-cluster->items cdata) line-data)))
                    (items-size (length (println-cluster->items cdata)))
                    (point-on-last-line (= current-line-item-position items-size))
                    (single-kill-text (println-single-kill-text cdata)))
               (when single-kill-text
                 (println-add-next-kill cdata)) ;; This may modify cdata
               (when (not (println-single-kill-text cdata))
                 ;; Modify println cluster only when we have at least
                 ;; two kill-text items at this point of execution. When
                 ;; on end of kill-ring we don't want to delete println
                 ;; cluster, in order to keep ignored items so far.
                 (setf (println-cluster->items cdata) (delete line-data (println-cluster->items cdata)))
                 (push (println-killed-text->killed-text line-data) (println-cluster->ignore-list cdata))
                 (when (and (not single-kill-text)
                            point-on-last-line)
                   (line-move-1 -1)))))))))

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

(defun println-multiline-p (cdata)
  (println-flags->multiline (println-cluster->flags cdata)))

(defun println-singleline-p (cdata)
  "Returns t if items from DATA are printed on a single line, otherwise nil"
  (not (println-multiline-p cdata)))

(defun println-align-p (cdata)
  (println-flags->align (println-cluster->flags cdata)))

(defun println-show-identifier-p (cdata)
  (println-flags->show-identifier (println-cluster->flags cdata)))

(defun println-render (cdata)
  (let ((items (println-cluster->items cdata))
        (identifier (when (println-show-identifier-p cdata) (println-cluster->identifier cdata)))
        (indentation (println-cluster->indentation cdata)))
    (if (println-multiline-p cdata)
        (if (println-align-p cdata)
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
           (cdata (println-cluster-create :items nil :identifier identifier :flags flags :indentation indentation :ignore-list nil)))
      (dotimes (item (min prefix (length kill-ring)))
        (let ((current (nth item kill-ring)))
          (println-data-add-item cdata current)))
      (forward-line)
      (println-write cdata))))

(defun println-add-stamp ()
  (when (println-get-cluster-data)
    (println-delete-current))
  (let* ((identifier (println-search-identifier))
         (global-flags (println-preferences->flags println-global-preferences))
         (flags (copy-println-flags global-flags))
         (indentation (println-indentation))
         (stamp-item (println-stamp-create :order
                                           (setf (println-preferences->counter println-global-preferences)
                                                 (1+ (println-preferences->counter println-global-preferences)))))
         (cdata (println-cluster-create
                 :items (list stamp-item)
                 :identifier identifier
                 :flags flags
                 :indentation indentation
                 :ignore-list nil)))
    (forward-line)
    (println-force-multiline cdata)
    (println-write cdata)))

;;;###autoload
(defun println-insert (prefix)
  (interactive "p")
  (pcase (println-preferences->mode println-global-preferences)
    (:killed-text (println-standard prefix))
    (:stamp (println-add-stamp))
    (_ (error "Unknown mode %s" (println-preferences->mode println-global-preferences)))))

(defun println-refresh (cdata)
  (println-delete-current)
  (if (println-cluster->items cdata)
      (println-write cdata)
    ;; No items to render, go to previous line
    (forward-line -1)
    (indent-according-to-mode)))

(defun println-write (cdata)
  (let ((str (println-render cdata)))
    (println-render-content cdata str)))

(defun println-render-content (cdata content)
  (let* ((indentation (println-cluster->indentation cdata))
         (content (concat indentation content "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'println-cluster-overlay)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap println-keymap)
      (overlay-put overlay 'println-cluster cdata)
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
