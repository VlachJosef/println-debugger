;;; println-gen.el --- Quick generating of println expressions -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(eval-when-compile (require 'subr-x))

(defface println-gen-cluster-overlay
  '((((class color) (background dark))
     :extend t
     :background "DarkSlateBlue"))
  "Face for lines controlled by println."
  :group 'println-gen-faces)

(defvar println-gen-basic-renderer (make-hash-table))
(defvar println-gen-basic-value-renderer (make-hash-table))
(defvar println-gen-basic-literal-string-renderer (make-hash-table))
(defvar println-gen-foreach-renderer (make-hash-table))
(defvar println-gen-aligned-renderer (make-hash-table))
(defvar println-gen-single-line-renderer (make-hash-table))
(defvar println-gen-identifier-founder (make-hash-table))
(defvar println-gen-stamp-renderer (make-hash-table))

(defun println-gen-register-major-mode (mode basic literal-string value foreach aligned single identifier stamp)
  "Register text renderers for major-mode MODE.

STAMP - renderer to print a unique marker only.  When used marker
number automatically increase by 1.  Reset marker number back to
1 by `println-gen-reset'.

  Scala example
    println(\"HeRe 111\")

  Javascript example
    console.log(\"HeRe 111\");

BASIC - rendered for basic printing of killed-text as variable
name and its value.

  Scala example
    println(\"foo: \" + foo)
    println(\"lorem: \" + lorem)

  Javascript example
    console.log(\"foo: \", foo);
    console.log(\"lorem: \", lorem);

ALIGNED - same as BASIC but values are aligned when variable
names differ in length.

  Scala example
    println(\"foo  : \" + foo)
    println(\"lorem: \" + lorem)

  Javascript example
    console.log(\"foo  : \", foo);
    console.log(\"lorem: \", lorem);

LITERAL-STRING - rendered for printing killed-text content as a
string literal.

  Scala example
    println(\"foo\")
    println(\"lorem\")

  Javascript example
    console.log(\"foo\");
    console.log(\"lorem\");

VALUE - renderer for printing only killed-text content as a value.

  Scala example
    println(foo)
    println(lorem)

  Javascript example
    console.log(foo);
    console.log(lorem);

FOREACH - renderer for printing content of a list, array etc. one
item per line.

  Scala example
    foo.foreach(println)
    lorem.foreach(println)

  Javascript example
    foo.forEach((item) => console.log(item));
    lorem.forEach((item) => console.log(item));

SINGLE - renderer which prints variable names and values on
single line

  Scala example
    println(\"foo: \" + foo + \", lorem: \" + lorem)

  Javascript example
    console.log(\"foo: \", foo, \" lorem: \", lorem);

IDENTIFIER - helper function for BASIC and ALIGNED renderers when
user want to include enclosing method/object etc. identifier in
print expression."
  (puthash mode basic println-gen-basic-renderer)
  (puthash mode literal-string println-gen-basic-literal-string-renderer)
  (puthash mode value println-gen-basic-value-renderer)
  (puthash mode foreach println-gen-foreach-renderer)
  (puthash mode aligned println-gen-aligned-renderer)
  (puthash mode single println-gen-single-line-renderer)
  (puthash mode identifier println-gen-identifier-founder)
  (puthash mode stamp println-gen-stamp-renderer))

(cl-defstruct (println-gen-preferences
               (:constructor println-gen-preferences-create)
               (:copier nil)
               (:conc-name println-gen-preferences->))
  ;; number used by the :stamp mode
  counter
  ;; can be either :killed-text or :stamp
  mode
  ;; local preferences for cluster of println statements
  flags)

(cl-defstruct (println-gen-stamp
               (:constructor println-gen-stamp-create)
               (:copier nil)
               (:conc-name println-gen-stamp->))
  order)

(cl-defstruct (println-gen-killed-text
               (:constructor println-gen-killed-text-create)
               (:copier nil)
               (:conc-name println-gen-killed-text->))
  killed-text
  (type :rich :read-only nil :documentation "One of :rich :plain :string-literal :foreach or :foreach-delimited"))

(cl-defstruct (println-gen-cluster
               (:constructor println-gen-cluster-create)
               (:copier nil)
               (:conc-name println-gen-cluster->))
  ;; list of println-gen-killed-texts and/or println-gen-stamps
  items
  ;; name of function/defun wrapping current println cluster. It is determined by mode specific function.
  identifier
  ;; cluster local flags
  flags
  ;; indentation of the cluster
  indentation
  ;; list of kill-texts to ignore
  ignore-list)

(cl-defstruct (println-gen-flags
               (:constructor println-gen-flags-create)
               (:conc-name println-gen-flags->))
  multiline align show-identifier)

(defvar println-gen-global-preferences
  (println-gen-preferences-create
   ;; Counter for :stamp mode.
   :counter 0
   ;; Either :killed-text or :stamp. Determines mode to start new println cluster in.
   :mode :killed-text
   :flags (println-gen-flags-create
           :multiline t
           :align nil
           :show-identifier nil))
  "Preferences to use when creating new cluster of print statements.")

(defun println-gen-data-add-item (cdata item)
  (setf (println-gen-cluster->items cdata)
        (append (println-gen-cluster->items cdata) (list (println-gen-killed-text-create :killed-text item)))))

(defun println-gen-toggle-multiline (cdata)
  (let* ((flags (println-gen-cluster->flags cdata))
         (global (println-gen-preferences->flags println-gen-global-preferences))
         (multiline (not (println-gen-flags->multiline flags))))
    (setf (println-gen-flags->multiline flags) multiline
          (println-gen-flags->multiline global) multiline)))

(defun println-gen-toggle-align (cdata)
  (let* ((flags (println-gen-cluster->flags cdata))
         (global (println-gen-preferences->flags println-gen-global-preferences))
         (align (not (println-gen-flags->align flags))))
    (setf (println-gen-flags->align flags) align
          (println-gen-flags->align global) align)))

(defun println-gen-get-line-data ()
  "Return a line-data for current line.
Return nil when line has no line-data, for example when rendering
println cluster as a single line."
  (if-let ((println-gen-line-data (get-text-property (if (eolp) (1- (point)) (point)) 'println-gen-line-data)))
      println-gen-line-data
    (if-let ((line (thing-at-point 'line))
             (prop-position (next-single-property-change 0 'println-gen-line-data line)))
        (get-text-property prop-position 'println-gen-line-data line))))

(defun println-gen-get-cluster-data ()
  "Return cluster data for current point."
  (get-char-property (point) 'println-gen-cluster))

(defmacro println-gen-with-cluster-data (cdata line-data body)
  "Bind cluster data to CDATA, line-data to LINE-DATA and execute BODY.
Both CDATA and LINE-DATA are accesible in BODY."
  (declare (indent 2) (debug t))
  `(let* ((,cdata (println-gen-get-cluster-data))
          (,line-data (println-gen-get-line-data)))
     (if (println-gen-cluster-p ,cdata)
         ,body
       (user-error "No cluster data found at the point."))))

(defmacro println-gen-modify-and-refresh-cluster (cdata line-data body)
  "Execute BODY where CDATA binds cluster data and LINE-DATA binds line data.
After BODY is evaluated, render new println cluster from CDATA. Old cluster
is replace."
  (declare (indent 2) (debug t))
  `(println-gen-with-cluster-data ,cdata ,line-data
     (progn ,body
            (let ((old-line-num (line-number-at-pos))
                  (old-column (current-column))
                  (old-eol (eolp)))
              (println-gen-refresh ,cdata)
              (goto-char (point-min))
              (forward-line (1- old-line-num))
              (goto-char (if old-eol
                             (line-end-position)
                           (min (line-end-position) (+ (line-beginning-position) old-column))))))))

(defmacro println-gen-modify-and-refresh-cluster-point-at-end (cdata body &rest rest)
  (declare (indent 1) (debug t))
  `(println-gen-with-cluster-data ,cdata _ignore-line-data
     (progn ,body
            ,@rest
            (println-gen-refresh ,cdata))))

(defun println-gen-align ()
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata _line-data
    (println-gen-toggle-align cdata)))

(defun println-gen-multiline ()
  (interactive)
  (println-gen-modify-and-refresh-cluster-point-at-end cdata
    (unless (println-gen-no-kill-text cdata)
      (println-gen-toggle-multiline cdata))))

(defun println-gen-show-identifier ()
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata _line-data
    (let* ((flags (println-gen-cluster->flags cdata))
           (global (println-gen-preferences->flags println-gen-global-preferences))
           (show-identifier (not (println-gen-flags->show-identifier flags))))
      (setf (println-gen-flags->show-identifier flags) show-identifier
            (println-gen-flags->show-identifier global) show-identifier))))

(defun println-gen-next-kill (cdata)
  (let ((items (seq-map #'println-gen-killed-text->killed-text
                        (seq-filter #'println-gen-killed-text-p
                                    (println-gen-cluster->items cdata))))
        (ignore-list (println-gen-cluster->ignore-list cdata))
        (kill-items (println-gen-preprocess-kill-ring)))
    (seq-find (lambda (kill-item)
                (and
                 (not (member kill-item items))
                 (not (member kill-item ignore-list))))
              kill-items)))

(defun println-gen-preprocess-kill-ring ()
  (let ((result))
    (dolist (element kill-ring)
      (let ((element (string-clean-whitespace element)))
        (unless (or
                 (string-blank-p element)
                 (string-match-p "[\r\n]+" element))
          (setq result (cons element result)))))
    (reverse result)))

(defun println-gen-add-next-kill (cdata)
  (let ((next-kill (println-gen-next-kill cdata)))
    (if (not next-kill)
        (message "No more elements in kill-ring")
      (println-gen-data-add-item cdata next-kill))))

(defun println-gen-increase ()
  (interactive)
  (println-gen-modify-and-refresh-cluster-point-at-end cdata
    (println-gen-add-next-kill cdata)))

(defun println-gen-force-multiline (cdata)
  "Set multiline flag of CDATA to t."
  (setf (println-gen-flags->multiline (println-gen-cluster->flags cdata)) t))

(defun println-gen-decrease ()
  (interactive)
  (println-gen-modify-and-refresh-cluster-point-at-end cdata
    (setf (println-gen-cluster->items cdata)
          (reverse (cdr (reverse (println-gen-cluster->items cdata)))))
    (when (println-gen-no-kill-text cdata)
      (println-gen-force-multiline cdata))))

(defun println-gen-new-stamp ()
  "Return new stamp data and increase counter by 1."
  (println-gen-stamp-create :order
                            (setf (println-gen-preferences->counter println-gen-global-preferences)
                                  (1+ (println-gen-preferences->counter println-gen-global-preferences)))))

(defun println-gen-stamp ()
  "Toggle current line between stamp data and killed-text data.
When line at point contains killed-text replace it with stamp. When line at point contains stamp
replace it with killed-text. When there is no kill-ring item to print display informative message."
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata line-data
    (if (println-gen-singleline-p cdata)
        (println-gen-toggle-multiline cdata)
      (let* ((items (println-gen-cluster->items cdata))
             (index (seq-position items line-data))
             (item-new (cond ((println-gen-killed-text-p line-data)
                              (setf (println-gen-preferences->mode println-gen-global-preferences) :stamp)
                              (println-gen-new-stamp))
                             ((println-gen-stamp-p line-data)
                              (let ((next-kill (println-gen-next-kill cdata)))
                                (if (not next-kill)
                                    (message "Nothing to replace stamp with, kill-ring is empty")
                                  (setf (println-gen-preferences->mode println-gen-global-preferences) :killed-text)
                                  (println-gen-killed-text-create :killed-text (println-gen-next-kill cdata))))))))
        (when (and (numberp index)
                   (or (println-gen-stamp-p item-new)
                       (println-gen-killed-text-p item-new)))
          (setf (nth index items) item-new))))))

(defun println-gen-reset ()
  "Reset global count to 0.
Next stamp rendered will be generated from number 1."
  (interactive)
  (setf (println-gen-preferences->counter println-gen-global-preferences) 0)
  (message "println counter reset to 1"))

(defun println-gen-reverse ()
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata _line-data
    (let* ((items (println-gen-cluster->items cdata))
           (items-with-index (seq-map-indexed (lambda (elt idx) (list idx elt)) items))
           (indexed-stamps-only (seq-filter  (lambda (s) (println-gen-stamp-p (cadr s))) items-with-index))
           (items-only (seq-filter  #'println-gen-killed-text-p items))
           (reversed (reverse items-only)))
      (dolist (element indexed-stamps-only)
        (let ((stamp-index (car element))
              (stamp (cadr element)))
          (push stamp (nthcdr stamp-index reversed))))
      (setf (println-gen-cluster->items cdata) reversed))))

(defun println-gen-literal-or-identifier ()
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata line-data
    (if (println-gen-singleline-p cdata)
        (println-gen-toggle-multiline cdata)
      (if (println-gen-stamp-p line-data)
          (message "Can't apply literal-or-identifier on stamp data")
        (when (println-gen-align-p cdata)
          (println-gen-toggle-align cdata))
        (setf (println-gen-killed-text->type line-data)
              (pcase (println-gen-killed-text->type line-data)
                (:rich :string-literal)
                (:string-literal :plain)
                (:plain :rich)
                (_ :rich)))))))

(defun println-gen-foreach ()
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata line-data
    (if (println-gen-singleline-p cdata)
        (println-gen-toggle-multiline cdata)
      (if (println-gen-stamp-p line-data)
          (message "Can't apply foreach on stamp data")
        (when (println-gen-align-p cdata)
          (println-gen-toggle-align cdata))
        (setf (println-gen-killed-text->type line-data)
              (pcase (println-gen-killed-text->type line-data)
                (:foreach :foreach-delimited)
                (:foreach-delimited :foreach)
                (_ :foreach)))))))

(defun println-gen-newline ()
  "When at the end of managed println cluster, pressing RET opens
new line and println cluster's text overlay does not move."
  (interactive)
  (when-let ((overlay (println-gen-find-overlay-specifying 'println-gen-cluster)))
    (if (not (= 1 (- (overlay-end overlay) (point))))
        (let ((original-binding (key-binding (kbd "RET") nil nil (point-min))))
          ;; Call whatever was originally bind by "RET" key.
          (unless (eq original-binding 'println-gen-newline)
            (funcall original-binding)))
      (goto-char (overlay-end overlay))
      (insert "\n")
      (forward-line -1)
      (indent-according-to-mode))))

(defun println-gen-commit ()
  (interactive)
  (let* ((cdata (println-gen-get-cluster-data))
         (str (println-gen-render cdata))
         (indentation (println-gen-cluster->indentation cdata))
         (content (concat indentation str "\n"))
         (point (point)))
    (println-gen-delete-current)
    (insert content)
    (goto-char point)))

(defun println-gen-count-kill-text (cdata)
  "Returns count of kill-text items in `cdata'."
  (seq-count (lambda (elt) (println-gen-killed-text-p elt))
             (println-gen-cluster->items cdata)))

(defun println-gen-single-kill-text (cdata)
  "Returns t if there is only single kill-text item in `cdata' otherwise nil."
  (= 1 (println-gen-count-kill-text cdata)))

(defun println-gen-no-kill-text (cdata)
  "Returns t if there is no kill-text item in `cdata' otherwise nil."
  (= 0 (println-gen-count-kill-text cdata)))

(defun println-gen-ignore ()
  "Add line-data of current line to the ignored list so that it is not rendered.
Useful when kill-ring contains garbage data which should not be printed."
  (interactive)
  (println-gen-modify-and-refresh-cluster cdata line-data
    (if (println-gen-singleline-p cdata)
        (println-gen-toggle-multiline cdata)
      (cond ((println-gen-stamp-p line-data)
             (message "Cannot ignore stamp"))
            ((println-gen-killed-text-p line-data)
             (let* ((current-line-item-position (1+ (seq-position (println-gen-cluster->items cdata) line-data)))
                    (items-size (length (println-gen-cluster->items cdata)))
                    (point-on-last-line (= current-line-item-position items-size))
                    (single-kill-text (println-gen-single-kill-text cdata)))
               (when single-kill-text
                 (println-gen-add-next-kill cdata)) ;; This may modify cdata
               (when (not (println-gen-single-kill-text cdata))
                 ;; Modify println cluster only when we have at least
                 ;; two kill-text items at this point of execution. When
                 ;; on end of kill-ring we don't want to delete println
                 ;; cluster, in order to keep ignored items so far.
                 (setf (println-gen-cluster->items cdata) (delete line-data (println-gen-cluster->items cdata)))
                 (push (println-gen-killed-text->killed-text line-data) (println-gen-cluster->ignore-list cdata))
                 (when (and (not single-kill-text)
                            point-on-last-line)
                   (line-move-1 -1)))))))))

(defun println-gen-delete-at-point ()
  "Delete println cluster."
  (interactive)
  (println-gen-delete-current))

(defun println-gen-find-overlay-specifying (prop)
  "Get overlay with property name PROP on current point."
  (let ((overlays (overlays-at (point))))
    (seq-find (lambda (overlay) (overlay-get overlay prop)) overlays)))

(defun println-gen-delete-current ()
  "Delete println cluster."
  (when-let ((overlay (println-gen-find-overlay-specifying 'println-gen-cluster)))
    (delete-region (overlay-start overlay) (overlay-end overlay))))

(defun println-gen-safe-string (str)
  (replace-regexp-in-string "\"" "" str))

(defun println-gen-identifier (identifier)
  (if identifier
      (format "[%s] " identifier)
    ""))

(defun println-gen-renderer (type)
  "Return renderer for TYPE.
Indicate error when rendered for TYPE is not found in underlying hash-map."
  (let ((renderer
         (gethash major-mode (pcase type
                               (:rich println-gen-basic-renderer)
                               (:plain println-gen-basic-value-renderer)
                               (:string-literal println-gen-basic-literal-string-renderer)
                               ((or :foreach :foreach-delimited) println-gen-foreach-renderer)
                               (:align println-gen-aligned-renderer)
                               (:single println-gen-single-line-renderer)
                               (:stamp println-gen-stamp-renderer)))))
    (unless renderer
      (user-error "No %s renderer found for %s." type major-mode))
    renderer))

(defun println-gen-apply-renderer-by-item-type (item identifier indentation)
  (let* ((type (println-gen-killed-text->type item))
         (killed-text (println-gen-killed-text->killed-text item))
         (renderer (println-gen-renderer type)))
    (pcase type
      (:rich
       (funcall renderer killed-text identifier))
      (:plain
       (funcall renderer killed-text))
      (:string-literal
       (funcall renderer killed-text))
      ((or :foreach :foreach-delimited)
       (replace-regexp-in-string "\n" (format "\n%s" indentation) (funcall renderer killed-text type))))))

(defun println-gen-to-string (item identifier indentation)
  (cond ((println-gen-killed-text-p item)
         (propertize (println-gen-apply-renderer-by-item-type item identifier indentation) 'println-gen-line-data item))
        ((println-gen-stamp-p item)
         (propertize (funcall (println-gen-renderer :stamp) (println-gen-stamp->order item)) 'println-gen-line-data item))))

(defun println-gen-to-string-aligned (item longest identifier)
  (cond ((println-gen-killed-text-p item)
         (propertize (funcall (println-gen-renderer :align) (println-gen-killed-text->killed-text item) longest identifier) 'println-gen-line-data item))
        ((println-gen-stamp-p item)
         (propertize (funcall (println-gen-renderer :stamp) (println-gen-stamp->order item)) 'println-gen-line-data item))))

(defun println-gen-render-single-line (items identifier)
  (funcall (println-gen-renderer :single) (mapcar #'println-gen-killed-text->killed-text (seq-filter #'println-gen-killed-text-p items)) identifier))

(defun println-gen-multiline-p (cdata)
  "Return t if items from CDATA are printed on a multiple lines, otherwise nil."
  (println-gen-flags->multiline (println-gen-cluster->flags cdata)))

(defun println-gen-singleline-p (cdata)
  "Return t if items from CDATA are printed on a single line, otherwise nil."
  (not (println-gen-multiline-p cdata)))

(defun println-gen-align-p (cdata)
  "Return t if align flag from CDATA is set, otherwise nil."
  (println-gen-flags->align (println-gen-cluster->flags cdata)))

(defun println-gen-show-identifier-p (cdata)
  "Return t if show-identifier flag from CDATA is set, otherwise nil."
  (println-gen-flags->show-identifier (println-gen-cluster->flags cdata)))

(defun println-gen-render (cdata)
  (let ((items (println-gen-cluster->items cdata))
        (identifier (when (println-gen-show-identifier-p cdata) (println-gen-cluster->identifier cdata)))
        (indentation (println-gen-cluster->indentation cdata)))
    (if (println-gen-multiline-p cdata)
        (if (println-gen-align-p cdata)
            (let* ((widths (seq-map (lambda (item) (string-width (or (println-gen-killed-text->killed-text item) ""))) (seq-filter #'println-gen-killed-text-p items)))
                   (longest (if widths (seq-max widths) 0)))
              (mapconcat (lambda (item)
                           (println-gen-to-string-aligned item longest identifier))
                         items (concat "\n" indentation)))
          (mapconcat (lambda (item)
                       (println-gen-to-string item identifier indentation))
                     items (concat "\n" indentation)))
      (println-gen-render-single-line items identifier))))

(defun println-gen-search-identifier ()
  (when-let ((identifier-founder (gethash major-mode println-gen-identifier-founder)))
    (funcall identifier-founder)))

(defun println-gen-indentation ()
  "Get indentations of a new line below current line"
  (save-excursion
    (goto-char (line-end-position))
    (newline 1)
    (indent-according-to-mode)
    (prog1 (if indent-tabs-mode
               (make-string (/ (current-indentation) tab-width) ?\t)
             (make-string (current-indentation) ?\ ))
      (delete-region (1- (line-beginning-position)) (point)))))

(defun println-gen-standard (prefix)
  "Render fresh killed-text cluster.
Render number of PREFIX items from kill-ring."
  (if (not kill-ring)
      (message "Nothing to print, kill-ring is empty")
    (when (println-gen-get-cluster-data)
      (println-gen-delete-current))
    (let* ((kill-ring (println-gen-preprocess-kill-ring))
           (identifier (println-gen-search-identifier))
           (global-flags (println-gen-preferences->flags println-gen-global-preferences))
           (flags (copy-println-gen-flags global-flags))
           (indentation (println-gen-indentation))
           (cdata (println-gen-cluster-create :items nil :identifier identifier :flags flags :indentation indentation :ignore-list nil)))
      (dotimes (item (min prefix (length kill-ring)))
        (let ((current (nth item kill-ring)))
          (println-gen-data-add-item cdata current)))
      (forward-line)
      (println-gen-write cdata))))

(defun println-gen-add-stamp ()
  "Render fresh stamp cluster and increase the count by 1."
  (when (println-gen-get-cluster-data)
    (println-gen-delete-current))
  (let* ((identifier (println-gen-search-identifier))
         (global-flags (println-gen-preferences->flags println-gen-global-preferences))
         (flags (copy-println-gen-flags global-flags))
         (indentation (println-gen-indentation))
         (stamp-item (println-gen-new-stamp))
         (cdata (println-gen-cluster-create
                 :items (list stamp-item)
                 :identifier identifier
                 :flags flags
                 :indentation indentation
                 :ignore-list nil)))
    (forward-line)
    (println-gen-force-multiline cdata)
    (println-gen-write cdata)))

(defun println-gen-refresh (cdata)
  (println-gen-delete-current)
  (if (println-gen-cluster->items cdata)
      (println-gen-write cdata)
    ;; No items to render, go to previous line
    (forward-line -1)
    (indent-according-to-mode)))

(defun println-gen-write (cdata)
  "Stringify CDATA and insert it into buffer with println control overlay."
  (let* ((indentation (println-gen-cluster->indentation cdata))
         (str (println-gen-render cdata))
         (content (concat indentation str "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'println-gen-cluster-overlay)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap println-gen-keymap)
      (overlay-put overlay 'println-gen-cluster cdata)
      (backward-char))))

(defvar println-gen-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'println-gen-commit)
    (define-key map (kbd "RET") 'println-gen-newline)
    (define-key map (kbd "C-M-r") 'println-gen-reverse)
    (define-key map (kbd "C-M-i") 'println-gen-reset)
    (define-key map (kbd "C-M-o") 'println-gen-stamp)
    (define-key map (kbd "C-M-s") 'println-gen-show-identifier)
    (define-key map (kbd "C-M-m") 'println-gen-multiline)
    (define-key map (kbd "C-M-a") 'println-gen-align)
    (define-key map (kbd "C-M-p") 'println-gen-decrease)
    (define-key map (kbd "C-M-n") 'println-gen-increase)
    (define-key map (kbd "C-M-k") 'println-gen-ignore)
    (define-key map (kbd "C-M-d") 'println-gen-delete-at-point)
    (define-key map (kbd "C-M-x") 'println-gen-literal-or-identifier)
    (define-key map (kbd "C-M-c") 'println-gen-foreach)
    map)
  "Keymap for println cluster text overlay.")

(provide 'println-gen)

;;; println-gen.el ends here
