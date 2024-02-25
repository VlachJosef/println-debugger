# println-debugger

Quick generating of println expressions in Emacs Lisp, Scala, Javascript, Typescript and Rust from `kill-ring` entries.

## Usage

When run, `println-debugger` generates println expression(s) from entries in `kill-ring`. Each set of expressions is highlighted with an overlay. This overlay defines a keymap property with following commands:

 - <kbd>C-c C-c</kbd> - `println-debugger-commit` - remove overlay and println expressions will become unmanaged by println-debugger.
 - <kbd>C-M-n</kbd>   - `println-debugger-increase` - take next entry from `kill-ring` and generates new println expression.
 - <kbd>C-M-p</kbd>   - `println-debugger-decrease` - remove last println expression.
 - <kbd>C-M-r</kbd>   - `println-debugger-reverse` - reverse order of generated println expressions.
 - <kbd>C-M-s</kbd>   - `println-debugger-show-identifier` - show enclosing defun's name as part of println expression.
 - <kbd>C-M-m</kbd>   - `println-debugger-multiline` - put all println expressions to a single line.
 - <kbd>C-M-a</kbd>   - `println-debugger-align` - align all println expressions for nicer output.
 - <kbd>C-M-x</kbd>   - `println-debugger-literal-or-identifier` - toggle between following println modes:
    - `println("a " + a)`
    - `println("a")`
    - `println(a)`
 - <kbd>C-M-c</kbd>   - `println-debugger-foreach` - generates `foreach` loop (make sure that current `kill-ring` item refers to a collection type). Example [foreach usage](#print-foreach-loop).
 - <kbd>C-M-o</kbd>   - `println-debugger-stamp` - generate stamp saying "HeRe 111" which will get increased with every use. Order is a global variable. Example [stamp usage](#print-unique-stamps).
 - <kbd>C-M-i</kbd>   - `println-debugger-reset` - reset counter used by `println-debugger-stamp` to 1.
 - <kbd>C-M-k</kbd>   - `println-debugger-ignore` - remove println expression under the cursor and put its killed-text into local ignore list.
 - <kbd>C-M-d</kbd>   - `println-debugger-delete-at-point` - remove all println expressions in current set.

## Example config with autoload

Binding <kbd>C-M-n</kbd> to `println-debugger` by remapping from original command `forward-list`.

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
## Screencasts

### Print item names with their values

![println-debugger in action](https://github.com/VlachJosef/println-debugger/blob/screenshots/rust.gif?raw=true)

### Print unique stamps

![println-debugger in action](https://github.com/VlachJosef/println-debugger/blob/screenshots/rust-stamp.gif?raw=true)

### Print foreach loop

![println-debugger in action](https://github.com/VlachJosef/println-debugger/blob/screenshots/rust-foreach.gif?raw=true)

# Development

## Running tests

1) Open file `println-debugger-tests.el`
   `M-x find-file test/println-debugger-tests.el RET`

2) Eval content of buffer `println-debugger-tests.el`
   `M-x eval-buffer RET`

3) Run all tests by
   `M-x ert RET t RET`
