# println-debugger

Quick generating of println expressions in Emacs Lisp, Scala, Javascript, Typescript and Rust.

```elisp
(use-package println-debugger
  :bind (:map
         emacs-lisp-mode-map
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-n" . println-insert-after)
         :map scala-mode-map
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-n" . println-insert-after)
         :map js-mode-map
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-n" . println-insert-after)))
 ```

println-debugger in action (outdated)

![println-debugger in action](images/println-debugger-in-action.gif)

Useful resources for starting with tree-sitter on emacs:  https://emacsconf.org/2022/talks/treesitter/ (https://www.youtube.com/watch?v=MZPR_SC9LzE)
