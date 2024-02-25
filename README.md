# println-debugger

Quick generating of println expressions in Emacs Lisp, Scala, Javascript, Typescript and Rust.

## Example config with autoload

Binding <kbd>C-M-n</kbd> to `println-debugger` by remapping from original command.

```elisp
(use-package println-debugger
  :load-path "~/println-debugger/"
  :commands println-debugger
  :bind (:map emacs-lisp-mode-map
              ([remap forward-list] . println-debugger)))

(use-package println-debugger-emacs-lisp
  :after println-debugger
  :load-path "~/println-debugger/")

(use-package println-debugger-scala
  :after println-debugger
  :load-path "~/println-debugger/")

(with-eval-after-load 'scala-ts-mode
  (keymap-set scala-ts-mode-map "<remap> <forward-list>" #'println-debugger))

(use-package println-debugger-rust
  :after println-debugger
  :load-path "~/println-debugger/")

(with-eval-after-load 'rust-ts-mode
  (keymap-set rust-ts-mode-map "<remap> <forward-list>" #'println-debugger))

(use-package println-debugger-javascript
  :after println-debugger
  :load-path "~/println-debugger/")

(with-eval-after-load 'typescript-ts-mode
    (keymap-set typescript-ts-mode-map "<remap> <forward-list>" #'println-debugger)
    (keymap-set tsx-ts-mode-map "<remap> <forward-list>" #'println-debugger))
 ```

println-debugger in action (outdated)

![println-debugger in action](images/println-debugger-in-action.gif)

# Development

## Running tests

1) Open file `println-debugger-tests.el`
   `M-x find-file test/println-debugger-tests.el RET`

2) Eval content of buffer `println-debugger-tests.el`
   `M-x eval-buffer RET`

3) Run all tests by
   `M-x ert RET t RET`
