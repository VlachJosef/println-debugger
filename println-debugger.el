;;; println-debugger.el --- Convenient function for generating println statements -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/println-debugger
;; Package-Version:  0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools

;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(eval-when-compile (require 'subr-x))

(defface println-debugger-cluster-overlay
  '((((class color) (background dark))
     :extend t
     :background "DarkSlateBlue"))
  "Face for lines controlled by println."
  :group 'println-debugger-faces)

(defvar println-debugger-basic-renderer (make-hash-table))
(defvar println-debugger-basic-value-renderer (make-hash-table))
(defvar println-debugger-basic-literal-string-renderer (make-hash-table))
(defvar println-debugger-foreach-renderer (make-hash-table))
(defvar println-debugger-aligned-renderer (make-hash-table))
(defvar println-debugger-single-line-renderer (make-hash-table))
(defvar println-debugger-identifier-founder (make-hash-table))
(defvar println-debugger-stamp-renderer (make-hash-table))

(defun println-debugger-register-major-mode (mode basic literal-string value foreach aligned single identifier stamp)
  "Register text renderers for major-mode MODE.

STAMP - renderer to print a unique marker only.  When used marker
number automatically increase by 1.  Reset marker number back to
1 by `println-debugger-reset'.

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
  (puthash mode basic println-debugger-basic-renderer)
  (puthash mode literal-string println-debugger-basic-literal-string-renderer)
  (puthash mode value println-debugger-basic-value-renderer)
  (puthash mode foreach println-debugger-foreach-renderer)
  (puthash mode aligned println-debugger-aligned-renderer)
  (puthash mode single println-debugger-single-line-renderer)
  (puthash mode identifier println-debugger-identifier-founder)
  (puthash mode stamp println-debugger-stamp-renderer))

(cl-defstruct (println-debugger-preferences
               (:constructor println-debugger-preferences-create)
               (:copier nil)
               (:conc-name println-debugger-preferences->))
  ;; number used by the :stamp mode
  counter
  ;; can be either :killed-text or :stamp
  mode
  ;; local preferences for cluster of println statements
  flags)

(cl-defstruct (println-debugger-stamp
               (:constructor println-debugger-stamp-create)
               (:copier nil)
               (:conc-name println-debugger-stamp->))
  order)

(cl-defstruct (println-debugger-killed-text
               (:constructor println-debugger-killed-text-create)
               (:copier nil)
               (:conc-name println-debugger-killed-text->))
  killed-text
  (type :rich :read-only nil :documentation "One of :rich :plain :string-literal :foreach or :foreach-delimited"))

(cl-defstruct (println-debugger-cluster
               (:constructor println-debugger-cluster-create)
               (:copier nil)
               (:conc-name println-debugger-cluster->))
  ;; list of println-debugger-killed-texts and/or println-debugger-stamps
  items
  ;; name of function/defun wrapping current println cluster. It is determined by mode specific function.
  identifier
  ;; cluster local flags
  flags
  ;; indentation of the cluster
  indentation
  ;; list of kill-texts to ignore
  ignore-list)

(cl-defstruct (println-debugger-flags
               (:constructor println-debugger-flags-create)
               (:conc-name println-debugger-flags->))
  multiline align show-identifier)

(defvar println-debugger-global-preferences
  (println-debugger-preferences-create
   ;; Counter for :stamp mode.
   :counter 0
   ;; Either :killed-text or :stamp. Determines mode to start new println cluster in.
   :mode :killed-text
   :flags (println-debugger-flags-create
           :multiline t
           :align nil
           :show-identifier nil))
  "Preferences to use when creating new cluster of print statements.")

(defun println-debugger-data-add-item (cdata item)
  (setf (println-debugger-cluster->items cdata)
        (append (println-debugger-cluster->items cdata) (list (println-debugger-killed-text-create :killed-text item)))))

(defun println-debugger-toggle-multiline (cdata)
  (let* ((flags (println-debugger-cluster->flags cdata))
         (global (println-debugger-preferences->flags println-debugger-global-preferences))
         (multiline (not (println-debugger-flags->multiline flags))))
    (setf (println-debugger-flags->multiline flags) multiline
          (println-debugger-flags->multiline global) multiline)))

(defun println-debugger-toggle-align (cdata)
  (let* ((flags (println-debugger-cluster->flags cdata))
         (global (println-debugger-preferences->flags println-debugger-global-preferences))
         (align (not (println-debugger-flags->align flags))))
    (setf (println-debugger-flags->align flags) align
          (println-debugger-flags->align global) align)))

(defun println-debugger-get-line-data ()
  "Return a line-data for current line.
Return nil when line has no line-data, for example when rendering
println cluster as a single line."
  (if-let ((println-debugger-line-data (get-text-property (if (eolp) (1- (point)) (point)) 'println-debugger-line-data)))
      println-debugger-line-data
    (if-let ((line (thing-at-point 'line))
             (prop-position (next-single-property-change 0 'println-debugger-line-data line)))
        (get-text-property prop-position 'println-debugger-line-data line))))

(defun println-debugger-get-cluster-data ()
  "Return cluster data for current point."
  (get-char-property (point) 'println-debugger-cluster))

(defmacro println-debugger-with-cluster-data (cdata line-data body)
  "Bind cluster data to CDATA, line-data to LINE-DATA and execute BODY.
Both CDATA and LINE-DATA are accesible in BODY."
  (declare (indent 2) (debug t))
  `(let* ((,cdata (println-debugger-get-cluster-data))
          (,line-data (println-debugger-get-line-data)))
     (if (println-debugger-cluster-p ,cdata)
         ,body
       (user-error "No cluster data found at the point."))))

(defmacro println-debugger-modify-and-refresh-cluster (cdata line-data body)
  "Execute BODY where CDATA binds cluster data and LINE-DATA binds line data.
After BODY is evaluated, render new println cluster from CDATA. Old cluster
is replace."
  (declare (indent 2) (debug t))
  `(println-debugger-with-cluster-data ,cdata ,line-data
     (progn ,body
            (let ((old-line-num (line-number-at-pos))
                  (old-column (current-column))
                  (old-eol (eolp)))
              (println-debugger-refresh ,cdata)
              (goto-char (point-min))
              (forward-line (1- old-line-num))
              (goto-char (if old-eol
                             (line-end-position)
                           (min (line-end-position) (+ (line-beginning-position) old-column))))))))

(defmacro println-debugger-modify-and-refresh-cluster-point-at-end (cdata body &rest rest)
  (declare (indent 1) (debug t))
  `(println-debugger-with-cluster-data ,cdata _ignore-line-data
     (progn ,body
            ,@rest
            (println-debugger-refresh ,cdata))))

(defun println-debugger-align ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata _line-data
    (println-debugger-toggle-align cdata)))

(defun println-debugger-multiline ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster-point-at-end cdata
    (unless (println-debugger-no-kill-text cdata)
      (println-debugger-toggle-multiline cdata))))

(defun println-debugger-show-identifier ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata _line-data
    (let* ((flags (println-debugger-cluster->flags cdata))
           (global (println-debugger-preferences->flags println-debugger-global-preferences))
           (show-identifier (not (println-debugger-flags->show-identifier flags))))
      (setf (println-debugger-flags->show-identifier flags) show-identifier
            (println-debugger-flags->show-identifier global) show-identifier))))

(defun println-debugger-next-kill (cdata)
  (let ((items (seq-map #'println-debugger-killed-text->killed-text
                        (seq-filter #'println-debugger-killed-text-p
                                    (println-debugger-cluster->items cdata))))
        (ignore-list (println-debugger-cluster->ignore-list cdata))
        (kill-items (println-debugger-preprocess-kill-ring)))
    (seq-find (lambda (kill-item)
                (and
                 (not (member kill-item items))
                 (not (member kill-item ignore-list))))
              kill-items)))

(defun println-debugger-preprocess-kill-ring ()
  (let ((result))
    (dolist (element kill-ring)
      (let ((element (string-clean-whitespace element)))
        (unless (or
                 (string-blank-p element)
                 (string-match-p "[\r\n]+" element))
          (setq result (cons element result)))))
    (reverse result)))

(defun println-debugger-add-next-kill (cdata)
  (let ((next-kill (println-debugger-next-kill cdata)))
    (if (not next-kill)
        (message "No more elements in kill-ring")
      (println-debugger-data-add-item cdata next-kill))))

(defun println-debugger-increase ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster-point-at-end cdata
    (println-debugger-add-next-kill cdata)))

(defun println-debugger-force-multiline (cdata)
  "Set multiline flag of CDATA to t."
  (setf (println-debugger-flags->multiline (println-debugger-cluster->flags cdata)) t))

(defun println-debugger-decrease ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster-point-at-end cdata
    (setf (println-debugger-cluster->items cdata)
          (reverse (cdr (reverse (println-debugger-cluster->items cdata)))))
    (when (println-debugger-no-kill-text cdata)
      (println-debugger-force-multiline cdata))))

(defun println-debugger-new-stamp ()
  "Return new stamp data and increase counter by 1."
  (println-debugger-stamp-create :order
                                 (setf (println-debugger-preferences->counter println-debugger-global-preferences)
                                       (1+ (println-debugger-preferences->counter println-debugger-global-preferences)))))

(defun println-debugger-stamp ()
  "Toggle current line between stamp data and killed-text data.
When line at point contains killed-text replace it with stamp. When line at point contains stamp
replace it with killed-text. When there is no kill-ring item to print display informative message."
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata line-data
    (if (println-debugger-singleline-p cdata)
        (println-debugger-toggle-multiline cdata)
      (let* ((items (println-debugger-cluster->items cdata))
             (index (seq-position items line-data))
             (item-new (cond ((println-debugger-killed-text-p line-data)
                              (setf (println-debugger-preferences->mode println-debugger-global-preferences) :stamp)
                              (println-debugger-new-stamp))
                             ((println-debugger-stamp-p line-data)
                              (let ((next-kill (println-debugger-next-kill cdata)))
                                (if (not next-kill)
                                    (message "Nothing to replace stamp with, kill-ring is empty")
                                  (setf (println-debugger-preferences->mode println-debugger-global-preferences) :killed-text)
                                  (println-debugger-killed-text-create :killed-text (println-debugger-next-kill cdata))))))))
        (when (and (numberp index)
                   (or (println-debugger-stamp-p item-new)
                       (println-debugger-killed-text-p item-new)))
          (setf (nth index items) item-new))))))

(defun println-debugger-reset ()
  "Reset global count to 0.
Next stamp rendered will be generated from number 1."
  (interactive)
  (setf (println-debugger-preferences->counter println-debugger-global-preferences) 0)
  (message "println counter reset to 1"))

(defun println-debugger-reverse ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata _line-data
    (let* ((items (println-debugger-cluster->items cdata))
           (items-with-index (seq-map-indexed (lambda (elt idx) (list idx elt)) items))
           (indexed-stamps-only (seq-filter  (lambda (s) (println-debugger-stamp-p (cadr s))) items-with-index))
           (items-only (seq-filter  #'println-debugger-killed-text-p items))
           (reversed (reverse items-only)))
      (dolist (element indexed-stamps-only)
        (let ((stamp-index (car element))
              (stamp (cadr element)))
          (push stamp (nthcdr stamp-index reversed))))
      (setf (println-debugger-cluster->items cdata) reversed))))

(defun println-debugger-literal-or-identifier ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata line-data
    (if (println-debugger-singleline-p cdata)
        (println-debugger-toggle-multiline cdata)
      (if (println-debugger-stamp-p line-data)
          (message "Can't apply literal-or-identifier on stamp data")
        (when (println-debugger-align-p cdata)
          (println-debugger-toggle-align cdata))
        (setf (println-debugger-killed-text->type line-data)
              (pcase (println-debugger-killed-text->type line-data)
                (:rich :string-literal)
                (:string-literal :plain)
                (:plain :rich)
                (_ :rich)))))))

(defun println-debugger-foreach ()
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata line-data
    (if (println-debugger-singleline-p cdata)
        (println-debugger-toggle-multiline cdata)
      (if (println-debugger-stamp-p line-data)
          (message "Can't apply foreach on stamp data")
        (when (println-debugger-align-p cdata)
          (println-debugger-toggle-align cdata))
        (setf (println-debugger-killed-text->type line-data)
              (pcase (println-debugger-killed-text->type line-data)
                (:foreach :foreach-delimited)
                (:foreach-delimited :foreach)
                (_ :foreach)))))))

(defun println-debugger-newline ()
  "When at the end of managed println cluster, pressing RET opens
new line and println cluster's text overlay does not move."
  (interactive)
  (when-let ((overlay (println-debugger-find-overlay-specifying 'println-debugger-cluster)))
    (if (not (= 1 (- (overlay-end overlay) (point))))
        (let ((original-binding (key-binding (kbd "RET") nil nil (point-min))))
          ;; Call whatever was originally bind by "RET" key.
          (unless (eq original-binding 'println-debugger-newline)
            (funcall original-binding)))
      (goto-char (overlay-end overlay))
      (insert "\n")
      (forward-line -1)
      (indent-according-to-mode))))

(defun println-debugger-commit ()
  (interactive)
  (let* ((cdata (println-debugger-get-cluster-data))
         (str (println-debugger-render cdata))
         (indentation (println-debugger-cluster->indentation cdata))
         (content (concat indentation str "\n"))
         (point (point)))
    (println-debugger-delete-current)
    (insert content)
    (goto-char point)))

(defun println-debugger-count-kill-text (cdata)
  "Returns count of kill-text items in `cdata'."
  (seq-count (lambda (elt) (println-debugger-killed-text-p elt))
             (println-debugger-cluster->items cdata)))

(defun println-debugger-single-kill-text (cdata)
  "Returns t if there is only single kill-text item in `cdata' otherwise nil."
  (= 1 (println-debugger-count-kill-text cdata)))

(defun println-debugger-no-kill-text (cdata)
  "Returns t if there is no kill-text item in `cdata' otherwise nil."
  (= 0 (println-debugger-count-kill-text cdata)))

(defun println-debugger-ignore ()
  "Add line-data of current line to the ignored list so that it is not rendered.
Useful when kill-ring contains garbage data which should not be printed."
  (interactive)
  (println-debugger-modify-and-refresh-cluster cdata line-data
    (if (println-debugger-singleline-p cdata)
        (println-debugger-toggle-multiline cdata)
      (cond ((println-debugger-stamp-p line-data)
             (message "Cannot ignore stamp"))
            ((println-debugger-killed-text-p line-data)
             (let* ((current-line-item-position (1+ (seq-position (println-debugger-cluster->items cdata) line-data)))
                    (items-size (length (println-debugger-cluster->items cdata)))
                    (point-on-last-line (= current-line-item-position items-size))
                    (single-kill-text (println-debugger-single-kill-text cdata)))
               (when single-kill-text
                 (println-debugger-add-next-kill cdata)) ;; This may modify cdata
               (when (not (println-debugger-single-kill-text cdata))
                 ;; Modify println cluster only when we have at least
                 ;; two kill-text items at this point of execution. When
                 ;; on end of kill-ring we don't want to delete println
                 ;; cluster, in order to keep ignored items so far.
                 (setf (println-debugger-cluster->items cdata) (delete line-data (println-debugger-cluster->items cdata)))
                 (push (println-debugger-killed-text->killed-text line-data) (println-debugger-cluster->ignore-list cdata))
                 (when (and (not single-kill-text)
                            point-on-last-line)
                   (line-move-1 -1)))))))))

(defun println-debugger-delete-at-point ()
  "Delete println cluster."
  (interactive)
  (println-debugger-delete-current))

(defun println-debugger-find-overlay-specifying (prop)
  "Get overlay with property name PROP on current point."
  (let ((overlays (overlays-at (point))))
    (seq-find (lambda (overlay) (overlay-get overlay prop)) overlays)))

(defun println-debugger-delete-current ()
  "Delete println cluster."
  (when-let ((overlay (println-debugger-find-overlay-specifying 'println-debugger-cluster)))
    (delete-region (overlay-start overlay) (overlay-end overlay))))

(defun println-debugger-safe-string (str)
  (replace-regexp-in-string "\"" "" str))

(defun println-debugger-identifier (identifier)
  (if identifier
      (format "[%s] " identifier)
    ""))

(defun println-debugger-renderer (type)
  "Return renderer for TYPE.
Indicate error when rendered for TYPE is not found in underlying hash-map."
  (let ((renderer
         (gethash major-mode (pcase type
                               (:rich println-debugger-basic-renderer)
                               (:plain println-debugger-basic-value-renderer)
                               (:string-literal println-debugger-basic-literal-string-renderer)
                               ((or :foreach :foreach-delimited) println-debugger-foreach-renderer)
                               (:align println-debugger-aligned-renderer)
                               (:single println-debugger-single-line-renderer)
                               (:stamp println-debugger-stamp-renderer)))))
    (unless renderer
      (user-error "No %s renderer found for %s." type major-mode))
    renderer))

(defun println-debugger-apply-renderer-by-item-type (item identifier indentation)
  (let* ((type (println-debugger-killed-text->type item))
         (killed-text (println-debugger-killed-text->killed-text item))
         (renderer (println-debugger-renderer type)))
    (pcase type
      (:rich
       (funcall renderer killed-text identifier))
      (:plain
       (funcall renderer killed-text))
      (:string-literal
       (funcall renderer killed-text))
      ((or :foreach :foreach-delimited)
       (replace-regexp-in-string "\n" (format "\n%s" indentation) (funcall renderer killed-text type))))))

(defun println-debugger-to-string (item identifier indentation)
  (cond ((println-debugger-killed-text-p item)
         (propertize (println-debugger-apply-renderer-by-item-type item identifier indentation) 'println-debugger-line-data item))
        ((println-debugger-stamp-p item)
         (propertize (funcall (println-debugger-renderer :stamp) (println-debugger-stamp->order item)) 'println-debugger-line-data item))))

(defun println-debugger-to-string-aligned (item longest identifier)
  (cond ((println-debugger-killed-text-p item)
         (propertize (funcall (println-debugger-renderer :align) (println-debugger-killed-text->killed-text item) longest identifier) 'println-debugger-line-data item))
        ((println-debugger-stamp-p item)
         (propertize (funcall (println-debugger-renderer :stamp) (println-debugger-stamp->order item)) 'println-debugger-line-data item))))

(defun println-debugger-render-single-line (items identifier)
  (funcall (println-debugger-renderer :single) (mapcar #'println-debugger-killed-text->killed-text (seq-filter #'println-debugger-killed-text-p items)) identifier))

(defun println-debugger-multiline-p (cdata)
  "Return t if items from CDATA are printed on a multiple lines, otherwise nil."
  (println-debugger-flags->multiline (println-debugger-cluster->flags cdata)))

(defun println-debugger-singleline-p (cdata)
  "Return t if items from CDATA are printed on a single line, otherwise nil."
  (not (println-debugger-multiline-p cdata)))

(defun println-debugger-align-p (cdata)
  "Return t if align flag from CDATA is set, otherwise nil."
  (println-debugger-flags->align (println-debugger-cluster->flags cdata)))

(defun println-debugger-show-identifier-p (cdata)
  "Return t if show-identifier flag from CDATA is set, otherwise nil."
  (println-debugger-flags->show-identifier (println-debugger-cluster->flags cdata)))

(defun println-debugger-render (cdata)
  (let ((items (println-debugger-cluster->items cdata))
        (identifier (when (println-debugger-show-identifier-p cdata) (println-debugger-cluster->identifier cdata)))
        (indentation (println-debugger-cluster->indentation cdata)))
    (if (println-debugger-multiline-p cdata)
        (if (println-debugger-align-p cdata)
            (let* ((widths (seq-map (lambda (item) (string-width (or (println-debugger-killed-text->killed-text item) ""))) (seq-filter #'println-debugger-killed-text-p items)))
                   (longest (if widths (seq-max widths) 0)))
              (mapconcat (lambda (item)
                           (println-debugger-to-string-aligned item longest identifier))
                         items (concat "\n" indentation)))
          (mapconcat (lambda (item)
                       (println-debugger-to-string item identifier indentation))
                     items (concat "\n" indentation)))
      (println-debugger-render-single-line items identifier))))

(defun println-debugger-search-identifier ()
  (when-let ((identifier-founder (gethash major-mode println-debugger-identifier-founder)))
    (funcall identifier-founder)))

(defun println-debugger-indentation ()
  "Get indentations of a new line below current line"
  (save-excursion
    (goto-char (line-end-position))
    (newline 1)
    (indent-according-to-mode)
    (prog1 (if indent-tabs-mode
               (make-string (/ (current-indentation) tab-width) ?\t)
             (make-string (current-indentation) ?\ ))
      (delete-region (1- (line-beginning-position)) (point)))))

(defun println-debugger-standard (prefix)
  "Render fresh killed-text cluster.
Render number of PREFIX items from kill-ring."
  (if (not kill-ring)
      (message "Nothing to print, kill-ring is empty")
    (when (println-debugger-get-cluster-data)
      (println-debugger-delete-current))
    (let* ((kill-ring (println-debugger-preprocess-kill-ring))
           (identifier (println-debugger-search-identifier))
           (global-flags (println-debugger-preferences->flags println-debugger-global-preferences))
           (flags (copy-println-debugger-flags global-flags))
           (indentation (println-debugger-indentation))
           (cdata (println-debugger-cluster-create :items nil :identifier identifier :flags flags :indentation indentation :ignore-list nil)))
      (dotimes (item (min prefix (length kill-ring)))
        (let ((current (nth item kill-ring)))
          (println-debugger-data-add-item cdata current)))
      (forward-line)
      (println-debugger-write cdata))))

(defun println-debugger-add-stamp ()
  "Render fresh stamp cluster and increase the count by 1."
  (when (println-debugger-get-cluster-data)
    (println-debugger-delete-current))
  (let* ((identifier (println-debugger-search-identifier))
         (global-flags (println-debugger-preferences->flags println-debugger-global-preferences))
         (flags (copy-println-debugger-flags global-flags))
         (indentation (println-debugger-indentation))
         (stamp-item (println-debugger-new-stamp))
         (cdata (println-debugger-cluster-create
                 :items (list stamp-item)
                 :identifier identifier
                 :flags flags
                 :indentation indentation
                 :ignore-list nil)))
    (forward-line)
    (println-debugger-force-multiline cdata)
    (println-debugger-write cdata)))

(defun println-debugger-refresh (cdata)
  (println-debugger-delete-current)
  (if (println-debugger-cluster->items cdata)
      (println-debugger-write cdata)
    ;; No items to render, go to previous line
    (forward-line -1)
    (indent-according-to-mode)))

(defun println-debugger-write (cdata)
  "Stringify CDATA and insert it into buffer with println control overlay."
  (let* ((indentation (println-debugger-cluster->indentation cdata))
         (str (println-debugger-render cdata))
         (content (concat indentation str "\n"))
         (start (line-beginning-position)))
    (beginning-of-line nil)
    (insert content)
    (let ((overlay (make-overlay start (point) (current-buffer) t nil)))
      (overlay-put overlay 'face 'println-debugger-cluster-overlay)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'keymap println-debugger-keymap)
      (overlay-put overlay 'println-debugger-cluster cdata)
      (backward-char))))

(defvar println-debugger-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'println-debugger-commit)
    (define-key map (kbd "RET") 'println-debugger-newline)
    (define-key map (kbd "C-M-r") 'println-debugger-reverse)
    (define-key map (kbd "C-M-i") 'println-debugger-reset)
    (define-key map (kbd "C-M-o") 'println-debugger-stamp)
    (define-key map (kbd "C-M-s") 'println-debugger-show-identifier)
    (define-key map (kbd "C-M-m") 'println-debugger-multiline)
    (define-key map (kbd "C-M-a") 'println-debugger-align)
    (define-key map (kbd "C-M-p") 'println-debugger-decrease)
    (define-key map (kbd "C-M-n") 'println-debugger-increase)
    (define-key map (kbd "C-M-k") 'println-debugger-ignore)
    (define-key map (kbd "C-M-d") 'println-debugger-delete-at-point)
    (define-key map (kbd "C-M-x") 'println-debugger-literal-or-identifier)
    (define-key map (kbd "C-M-c") 'println-debugger-foreach)
    map)
  "Keymap for println cluster text overlay.")

;;;###autoload
(defun println-debugger (prefix)
  (interactive "p")
  (pcase (println-debugger-preferences->mode println-debugger-global-preferences)
    (:killed-text (println-debugger-standard prefix))
    (:stamp (println-debugger-add-stamp))))

(provide 'println-debugger)

;;; println-debugger.el ends here
