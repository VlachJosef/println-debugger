(ert-deftest pd:debugger-1 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-1.scala")
       (progn
         (insert "package foo.bar

object Hello {
|hello
}
")
         (re-search-backward "|")
         (delete-char 1)
         (kill-new "world") ;; when on nonempty line kill-ring must be ignored
         (call-interactively 'print-ln)
         (buffer-string)))
    "package foo.bar

object Hello {
  println(\"hello: \" + (hello))
}
")))

(ert-deftest pd:debugger-2 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-2.scala")
       (progn
         (insert "package foo.bar

object Hello {
|
}
")
         (re-search-backward "|")
         (delete-char 1)
         (kill-new "hello")
         (call-interactively 'print-ln)
         (buffer-string)))
    "package foo.bar

object Hello {
  println(\"hello: \" + (hello))
}
")))

(ert-deftest pd:debugger-3 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-3.scala")
       (progn
         (insert "package foo.bar

object Hello {
|
}
")
         (re-search-backward "|")
         (delete-char 1)
         (kill-new "hello1")
         (kill-new "hello2")
         (funcall-interactively 'print-ln 2)
         (buffer-string)))
    "package foo.bar

object Hello {
  println(\"hello1: \" + (hello1))
  println(\"hello2: \" + (hello2))
}
")))

(ert-deftest pd:debugger-4 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-4.scala")
       (progn
         (insert "package foo.bar

object Hello {
|  world
}
")
         (re-search-backward "|")
         (delete-char 1)
         (kill-new "hello1")
         (kill-new "hello2")
         (funcall-interactively 'print-ln 2)
         (buffer-string)))
    "package foo.bar

object Hello {
  println(\"hello1: \" + (hello1))
  println(\"hello2: \" + (hello2))
  world
}
")))
