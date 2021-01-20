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

(defface print-ln-diff-hunk-heading-2
  '((((class color) (background dark))
     :extend t
     :background "black"))
  "Face for print-ln generated lines."
  :group 'print-ln-faces)

(defvar println-basic-renderer (make-hash-table))
(defvar println-aligned-renderer (make-hash-table))
(defvar println-single-line-renderer (make-hash-table))
(defvar println-identifier-founder (make-hash-table))
(defvar println-stamp-renderer (make-hash-table))

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
                                   (data (println-cluster-data-create :items nil :identifier identifier :flags flags :indentation nil)))
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



(cl-defstruct (println-preferences (:constructor println-preferences-create)
                                   (:copier nil)
                                   (:conc-name println-preferences->))
  mode ;; can be either :item or :stamp
  flags)

(cl-defstruct (println-stamp-data (:constructor println-stamp-data-create)
                                  (:copier nil)
                                  (:conc-name println-stamp-data->))
  order)

(cl-defstruct (println-item-data (:constructor println-item-data-create)
                                 (:copier nil)
                                 (:conc-name println-item-data->))
  item)

(let ((stamp (println-stamp-data-create :order 1))
      (item (println-item-data-create :item "POKLOP")))
  (cond ((println-item-data-p item)
         (message "HERE"))
        (t (message "NOPE %s" item)))
 )

(cl-defstruct (println-cluster-data (:constructor println-cluster-data-create)
                                    (:copier nil)
                                    (:conc-name println-cluster-data->))
  items identifier flags indentation)

(cl-defstruct (print-ln-flags (:constructor print-ln-flags-create)
                              (:copier nil)
                              (:conc-name print-ln-flags->))
  multiline align show-identifier)

(defconst println-default-flags
  (print-ln-flags-create :multiline t :align nil :show-identifier nil))

(defvar println-global-preferences
  (println-preferences-create
   :mode :item
   :flags println-default-flags))


(defun println-table-update-identifier (data identifier)
  (setf (println-cluster-data->identifier data)
        identifier))

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
      (re-search-forward (concat regex " " scala-syntax:plainid-re) nil t)
      (match-string num))))

(defun println-search-def ()
  (println-search-regex "def" 1))

(defun println-search-class ()
  (println-search-regex "\\(class\\|object\\|trait\\)" 2))

(defun println-search-gdscript-func ()
  (println-search-regex "func" 1))

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

(defun print-ln-align ()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (print-ln-toggle-align data)
    (print-ln-delete-current)
    (print-ln-render data)))

(defun print-ln-multiline ()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (print-ln-toggle-multiline data)
    (print-ln-delete-current)
    (print-ln-render data)))

(defun print-ln-identifier ()
  (interactive)
  (let ((println-inhibit-modification-hooks t)
        (data (println-get-data)))
    (print-ln-toggle-identifier data)
    (print-ln-delete-current)
    (print-ln-render data)))

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
  (let* ((println-inhibit-modification-hooks t)
         (data (println-get-data))
         (next-kill (println-next-kill data)))
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


(defun print-ln-stamp (prefix)
  (interactive "p")

  (let* ((data (println-get-data))
         (line (thing-at-point 'line))
         (item-old (get-text-property (next-single-property-change 0 'item-data line) 'item-data line))
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
      (error "Index of element '%s' not found" item-old))

    (let ((point (line-beginning-position))
          (println-inhibit-modification-hooks t))
      (print-ln-delete-current)
      (print-ln-render data)
      (goto-char point)
      (goto-char (line-end-position)))))

(defun print-ln-reset ()
  (interactive)
  (setq println-counter 0)
  (message "Println counter reset."))

(defun print-ln-reverse ()
  (interactive)
  (let* ((data (println-get-data))
         (items (println-cluster-data->items data)))
    (setf (println-cluster-data->items data)
          (reverse items))
    (print-ln-delete-current)
    (print-ln-render data)))


(defun println-exclude-current ()
  (interactive)
  (let* ((data (println-get-data))
         (items (println-cluster-data->items data)))
    (message "[println-exclude-current] items: %s" items)
    ))

(defvar print-ln-keymap
  (let ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "C-i") 'print-ln-foreach)
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
  (let ((println-inhibit-modification-hooks t))
    (print-ln-delete-current)))

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
  (cond ((println-item-data-p item)
         (when-let ((basic-renderer (gethash major-mode println-basic-renderer)))
           (propertize (funcall basic-renderer (println-item-data->item item) identifier) 'item-data item)))
        ((println-stamp-data-p item)
         (when-let ((stamp-renderer (gethash major-mode println-stamp-renderer)))
           (propertize (funcall stamp-renderer (println-stamp-data->order item)) 'item-data item)))))

(defun println-to-string-aligned (item longest identifier)
  (message "println-to-string-aligned HERE %s %s %s" (println-item-data-p item) (println-stamp-data-p item) item)
  (cond ((println-item-data-p item)
         (when-let ((aligned-renderer (gethash major-mode println-aligned-renderer)))
           (propertize (funcall aligned-renderer (println-item-data->item item) longest identifier) 'item-data item)))
        ((println-stamp-data-p item)
         (when-let ((stamp-renderer (gethash major-mode println-stamp-renderer)))
           (propertize (funcall stamp-renderer (println-stamp-data->order item)) 'item-data item)))))

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
    ;; TODO
    (funcall single-line-renderer (mapcar #'println-item-data->item (seq-filter #'println-item-data-p items)) identifier)))

(defun println-multiline-p (data)
  (print-ln-flags->multiline (println-cluster-data->flags data)))

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
                       (println-to-string item identifier)) items (concat "\n" indentation)))
      (println-render-single-line items identifier))))

;; (let ((buffer (generate-new-buffer "limbo")))
;;   (with-current-buffer buffer
;;     (scala-mode)
;;     (insert lorem-scala)
;;     (display-buffer (current-buffer))))

(defun println-search-identifier ()
  (when-let ((identifier-founder (gethash major-mode println-identifier-founder)))
    (message "identifier-founder %s " identifier-founder)
    (funcall identifier-founder)))

(defun println2 ()
  (interactive)
  (message "HIIIII"))

(defun println-indentation ()
  (save-excursion (forward-line) (indent-according-to-mode) (if indent-tabs-mode
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
    (message "identifier %s" identifier)
    (dotimes (item (min prefix (length kill-ring)))
      (let ((current (nth item kill-ring)))
        (println-data-add-item data current)))

    (forward-line)
    (print-ln-render data)))

(defun print-ln-add-stamp ()
  (when (println-get-data)
    (print-ln-delete-current))
  (message "HERE AAAA")
  (let* ((identifier (println-search-identifier))
         (flags (println-preferences->flags println-global-preferences))
         (indentation (println-indentation))
         (data (println-cluster-data-create
                :items (list (println-stamp-data-create :order (setq println-counter (1+ println-counter))))
                :identifier identifier
                :flags flags
                :indentation indentation)))
    (message "data %s" identifier)
    ;; (dotimes (item (min prefix (length kill-ring)))
    ;;   (let ((current (nth item kill-ring)))
    ;;     (println-data-add-item data current)))

    (forward-line)
    (print-ln-render data)))

(defun println (prefix)
  (interactive "p")
  (let ((println-inhibit-modification-hooks t))
    (pcase (println-preferences->mode println-global-preferences)
      (:item (println-standard prefix))
      (:stamp (print-ln-add-stamp))
      (_ (error "Unknown mode %s" (println-preferences->mode println-global-preferences))))
    ))

(defun print-ln-render (data)
  (let ((str (println-render data)))
    (print-ln-render-content data str)))

(defun println-modification (overlay after beginning end &optional pre-length)
  (message "overlay %s after %s start %s-%s pre-length %s" overlay after beginning end pre-length))

(defun print-ln-text-prop-to-overlay (beg end)
  (let ((a beg) (b beg) overlay)
    (while (not (eq a end))
      (setq a (next-single-property-change b :print-ln-current nil end)
            b (next-single-property-change a :print-ln-current nil end)
            overlay (make-overlay a b nil nil t))
      (overlay-put overlay 'face 'print-ln-diff-hunk-heading-2)
      (overlay-put overlay :print-ln-current (get-text-property a :print-ln-current))
      (overlay-put overlay 'modification-hooks '(println-modify))
      (overlay-put overlay 'insert-in-front-hooks '(println-insert-in-front))
      (overlay-put overlay 'insert-behind-hooks '(println-insert-behind))
      (overlay-put overlay 'evaporate t))

    (remove-list-of-text-properties beg end '(:print-ln-current))))

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
      ;;(overlay-put overlay 'modification-hooks '(println-modification))

      (overlay-put overlay 'print-ln data)
      ;;(put-text-property start (point) 'print-ln data)
      ;;(put-text-property start (point) 'keymap print-ln-keymap)

      (print-ln-text-prop-to-overlay start (point))
      (backward-char))))

(defun println-insert-in-front (overlay after start end &optional pre-change-len)

  (message "[println-insert-in-front] %s %s" start end))

(defvar println-inhibit-modification-hooks nil)

(defun println-modify (overlay after start end &optional pre-change-len)
  (when (not after)
    (message "%s [MODIFY.BEFORE] start %s end %s after %s pre-change-len %s" overlay start end after pre-change-len))
  (when (and after (not undo-in-progress) (not println-inhibit-modification-hooks))
    (message "%s [MODIFY.AFTER ] start %s end %s after %s pre-change-len %s" overlay start end after pre-change-len)
    (let* ((inserted (buffer-substring-no-properties start end))
           (current-item (overlay-get overlay :print-ln-current))
           (modified-item (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))
           (print-ln-overlay (print-ln-find-overlay-specifying 'print-ln))
           (data (println-get-data))
           (line-number (line-number-at-pos))
           (point (point))
           (cc (current-column))
           (before (progn (save-excursion
                           (re-search-backward (concat current-item "\\([[:space:]]*\\):") (line-beginning-position)))
                         (match-string-no-properties 1))))
      (message "[println-modify] BEFORE1 current-column: %s" (current-column))
      (println-data-update-item print-ln-overlay current-item modified-item)
      (print-ln-delete-current)
      (print-ln-render data)
      (message "[println-modify] BEFORE2 current-column: %s %s" (current-column) (- end start pre-change-len))
      ;;(goto-char (point-min))
      ;;(forward-line line-number)

      (cond ((= pre-change-len 0)
             (goto-char (+ (line-beginning-position (1+ (- line-number (line-number-at-pos) ))) cc (- end start pre-change-len)))
             (save-excursion
               (re-search-backward (concat modified-item "\\([[:space:]]*\\):") (line-beginning-position)))
             (unless (and
                      (s-blank? (match-string-no-properties 1))
                      (s-blank? before))
               (backward-char 1)))
            ((> pre-change-len 0)
             (goto-char (+ (line-beginning-position (1+ (- line-number (line-number-at-pos) ))) cc (- end start pre-change-len)))
             (save-excursion
               (re-search-backward (concat modified-item "\\([[:space:]]*\\):") (line-beginning-position)))
             (unless (s-blank? (match-string-no-properties 1))
               (forward-char 1))))


        ;; )
      ;; (goto-char (+ point (- end start pre-change-len)))
      ;; (when (println-align-p data)
      ;;   (let ((point (point)))
      ;;     (save-excursion (re-search-backward (concat modified-item "\\([[:space:]]*\\):") (line-beginning-position)))
      ;;     (message "[println-modify] (match-beginning 1): %s %s blank? %s current-column %s" (match-beginning 1) (match-end 1) (s-blank? (match-string-no-properties 1))  (current-column))
      ;;     (if (s-blank? (match-string-no-properties 1))
      ;;         (forward-char 1)
      ;;       (backward-char 1))))
      ;; (message "[println-modify] AFTER   current-column: %s" (current-column))
      )))

(defun abc (str)
  (message "[a] abc: %s" abc)

;;         1234567890123456789012345678901234567890


  (let ((point (point)))
    (re-search-backward (concat str "\\([[:space:]]*\\):") (line-beginning-position))
    (goto-char point)
    (if (s-blank? (match-string-no-properties 1))
        (forward-char 1)
      (backward-char 1))))

(defun println-insert-behind (overlay after start end &optional pre-change-len)

  (when (and after (not undo-in-progress) (not println-inhibit-modification-hooks))

    (message "[BEHIND] star t %s end %s undo-in-progress %s" start end undo-in-progress)
    (println-modify overlay after start end pre-change-len)))

(defun println-editable (item)
  (propertize item :print-ln-current item))

;; Scala support
(defun println-scala-to-string (item identifier)
  (concat "println(\"" (println-identifier identifier) (println-safe-string item) ": \" + "
          (println-editable item) ")"))

(defun println-scala-to-string-aligned (item longest identifier)
  (concat "println(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-safe-string item)) "\" + " (println-editable item) ")"))

(defun println-scala-render-single-line (items identifier)
  (concat "println(\"" (println-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-scala-to-single-line-string items " + ")) ")"))

(defun println-scala-identifier ()
  (format "%s.%s" (println-search-class) (println-search-def)))

(defun println-scala-stamp (order)
  (format "println(\"HeRe %s%s%s\")" order order order))

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
          (mapconcat #'println-editable items " ")
          ")"))

(defun println-emacs-lisp-search-defun ()
  (save-excursion
    (re-search-backward "^(defun \\([^( ]*\\)")
    (match-string 1)))

(defun println-emacs-lisp-stamp (order)
  (format "(message \"HerE %s%s%s\")" order order order))

;; GDScript support
(defun println-gdscript-to-string (item identifier)
  (concat "printt(\"" (println-identifier identifier) (println-safe-string item) ": \", "
          (println-editable item) ")"))

(defun println-gdscript-to-string-aligned (item longest identifier)
  (concat "printt(\"" (println-identifier identifier) (format (concat "%-" (number-to-string longest) "s: ") (println-safe-string item)) "\", " (println-editable item) ")"))

(defun println-gdscript-to-single-line-string (item)
  (concat "\", " (println-safe-string item) ": \", " item))

(defun println-gdscript-render-single-line (items identifier)
  (concat "printt(\"" (println-identifier identifier) (s-chop-prefix "\", " (mapconcat #'println-gdscript-to-single-line-string items ", ")) ")"))

(defun println-gdscript-identifier ()
  (message "[println-gdscript-identifi] (println-search-gdscript-func): %s" (println-search-gdscript-func))
  (format "%s.%s" (buffer-name) (println-search-gdscript-func)))

(defun println-gdscript-stamp (order)
  (format "printt(\"HeRe %s%s%s\")" order order order))

(defun println-register-major-mode (major-mode basic aligned single identifier stamp)
  (puthash major-mode basic println-basic-renderer)
  (puthash major-mode aligned println-aligned-renderer)
  (puthash major-mode single println-single-line-renderer)
  (puthash major-mode identifier println-identifier-founder)
  (puthash major-mode stamp println-stamp-renderer))

(println-register-major-mode 'scala-mode
                             #'println-scala-to-string
                             #'println-scala-to-string-aligned
                             #'println-scala-render-single-line
                             #'println-scala-identifier
                             #'println-scala-stamp)

(println-register-major-mode 'emacs-lisp-mode
                             #'println-emacs-lisp-to-string
                             #'println-emacs-lisp-to-string-aligned
                             #'println-emacs-lisp-render-single-line
                             #'println-emacs-lisp-search-defun
                             #'println-emacs-lisp-stamp)

(println-register-major-mode 'gdscript-mode
                             #'println-gdscript-to-string
                             #'println-gdscript-to-string-aligned
                             #'println-gdscript-render-single-line
                             #'println-gdscript-identifier
                             #'println-gdscript-stamp)


(defvar println-xml-section-start-re "^\\[XML\\.START\\]$")
(defvar println-xml-section-end-re "^\\[XML\\.END\\]$")

(defvar println-sbt-output "")

(defun println-xml (sbt-output)
  (when (or
         (string-match println-xml-section-start-re println-sbt-output)
         (string-match println-xml-section-start-re sbt-output))
    (setq println-sbt-output (concat println-sbt-output sbt-output))
    (when-let ((start (match-end 0))
               (end (when (string-match println-xml-section-end-re println-sbt-output)
                      (match-beginning 0))))
      (let ((xml (substring println-sbt-output start end)))
        (setq println-sbt-output "")
        (unless (s-blank? xml)
          (let ((buffer (generate-new-buffer (format "*XML at %s in %s* " (current-time-string) (buffer-name)))))
            (with-current-buffer buffer
              (insert (s-trim xml))
              (nxml-pretty-format)
              (toggle-truncate-lines 1)
              (display-buffer (current-buffer)))))))))


(defun println-comint ()
  (dolist (buffer (buffer-list) nil)
    (with-current-buffer buffer
      (when (eq 'sbt-mode major-mode)
        (unless (member 'println-xml comint-output-filter-functions)
          (add-hook 'comint-output-filter-functions 'println-xml))))))

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
