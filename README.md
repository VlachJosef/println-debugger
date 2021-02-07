# println-debugger

Convenient function for generating println statements in Emacs Lisp, Scala, Javascript and GDScript.

```elisp
(use-package println-debugger
  :bind (:map
         global-map ("C-x C-k P" . print-ln) ;; fallback when package is unstable
         :map
         emacs-lisp-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)
         :map scala-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)
         :map gdscript-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)
         :map js-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)))
 ```

println-debugger in action (outdated)

![println-debugger in action](images/println-debugger-in-action.gif)
