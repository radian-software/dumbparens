;;; dumbparens-test.el --- Unit tests for Dumbparens -*- lexical-binding: t -*-

(require 'map)
(require 'subr-x)

(require 'dumbparens)

(defvar dumbparens-test-mode-keymap
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "q") #'quit-window)))
  "Keymap for use in `dumbparens-test-mode'.")

(define-minor-mode dumbparens-test-mode
  "Minor mode to add some keybindings in test result buffers."
  :keymap dumbparens-test-mode-keymap)

(defvar dumbparens-tests nil
  "List of unit tests, an alist.")
(setq dumbparens-tests nil)

(cl-defmacro dumbparens-test (name desc &rest kws &key mode before keys after)
  "Declare a unit test."
  (declare (indent defun) (doc-string 2))
  (ignore mode before keys after)
  `(progn
     (when (alist-get ',name dumbparens-tests)
       (message "Overwriting existing test: %S" ',name))
     (setf (alist-get ',name dumbparens-tests) '(:desc ,desc ,@kws))))

(defun dumbparens-run-test (name)
  "Run a single unit test. Return non-nil if passed, nil if failed."
  (interactive
   (list
    (intern
     (completing-read
      "Run test: "
      (mapcar #'symbol-name (map-keys dumbparens-tests))))))
  (let* ((test (alist-get name dumbparens-tests))
         (bufname (format " *dumbparens test %S*" name))
         (result nil))
    (unless (and (symbolp (plist-get test :mode))
                 (stringp (plist-get test :before))
                 (stringp (plist-get test :keys))
                 (stringp (plist-get test :after)))
      (user-error "Incomplete test: %S" name))
    (when (get-buffer bufname)
      (kill-buffer bufname))
    (cl-block nil
      (save-window-excursion
        (pop-to-buffer bufname)
        (let ((mode (symbol-name (or (plist-get test :mode) 'elisp))))
          (when (equal mode "elisp")
            (setq mode "emacs-lisp"))
          (unless (string-suffix-p "-mode" mode)
            (setq mode (concat mode "-mode")))
          (setq mode (intern mode))
          (funcall mode))
        (dumbparens-mode +1)
        (when (fboundp 'smartparens-mode)
          (smartparens-mode -1))
        (when (fboundp 'electric-pair-mode)
          (electric-pair-mode -1))
        (when (fboundp 'paredit-mode)
          (paredit-mode -1))
        (save-excursion
          (insert (plist-get test :before)))
        (search-forward "|")
        (delete-region (match-beginning 0) (match-end 0))
        (condition-case e
            (execute-kbd-macro (kbd (plist-get test :keys)))
          (error
           (save-excursion
             (goto-char (point-max))
             (insert " [" (error-message-string e) "]"))))
        (insert "|")
        (setq result (buffer-string))))
    (if (equal result (plist-get test :after))
        (progn
          (message "Test %S passed" name)
          t)
      (message "Test %S failed" name)
      (with-current-buffer bufname
        (erase-buffer)
        (insert
         "TEST:\n\n"
         (symbol-name name)
         "\n"
         (plist-get test :desc)
         "\n\nBEFORE:\n\n"
         (plist-get test :before)
         "\n\nKEYS:\n\n"
         (plist-get test :keys)
         "\n\nEXPECTED:\n\n"
         (plist-get test :after)
         "\n\nGOT:\n\n"
         result
         "\n")
        (dumbparens-test-mode +1))
      (if noninteractive
          (progn
            (message "%s" (with-current-buffer bufname
                            (string-trim (buffer-string))))
            (kill-emacs 1))
        (pop-to-buffer bufname))
      nil)))

(defun dumbparens-run-all-tests ()
  "Run all the unit tests until a failure is encountered."
  (interactive)
  (cl-block nil
    (dolist (name (nreverse (map-keys dumbparens-tests)))
      (unless (dumbparens-run-test name)
        (cl-return)))
    (message "All tests passed")))

(dumbparens-test open-pair
  "Typing an open paren should insert a close paren"
  :before "|"
  :keys "("
  :after "(|)")

(dumbparens-test type-over-close
  "You can type over a close paren"
  :before "(|)"
  :keys ")"
  :after "()|")

(dumbparens-test open-string
  "Typing a quote should insert a matched pair"
  :before "|"
  :keys "\""
  :after "\"|\"")

(dumbparens-test type-over-close-quote
  "You can type over a close quote"
  :before "\"|\""
  :keys "\""
  :after "\"\"|")

(dumbparens-test delete-pair
  "Backspacing inside an empty pair should delete it"
  :before "(|)"
  :keys "DEL"
  :after "|")

(dumbparens-test delete-string
  "Backspacing inside an empty string should delete it"
  :before "\"|\""
  :keys "DEL"
  :after "|")

(dumbparens-test escape-cancels-type-over-close-quote
  "You won't type over a close quote right after a backslash"
  :before "\"\\|\""
  :keys "\""
  :after "\"\\\"|\"")

(dumbparens-test open-pair-with-prefix-arg
  "You can open multiple pairs at once with a prefix argument"
  :before "|"
  :keys "C-u 3 ("
  :after "(((|)))")

(dumbparens-test type-over-close-with-prefix-arg
  "You can type over multiple close parens at once with a prefix argument"
  :before "(((|)))"
  :keys "C-u 2 )"
  :after "((())|)")

(dumbparens-test delete-pair-forward
  "Typing C-d inside an empty pair should delete it"
  :before "(|)"
  :keys "C-d"
  :after "|")

(dumbparens-test type-lone-close-paren
  "You can type a close paren even with no open paren"
  :before "|"
  :keys ")"
  :after ")|")

(dumbparens-test no-pair-opened-before-symbol
  "No paired close paren is inserted right before a symbol"
  :before "|foo"
  :keys "("
  :after "(|foo")

(dumbparens-test double-escape-does-not-cancel-type-over-close-quote
  "You can type over a close quote after a double backslash"
  :before "\"\\\\|\""
  :keys "\""
  :after "\"\\\\\"|")

(dumbparens-test delete-open-paren
  "You can delete an open paren even if the close paren can't go with it"
  :before "(|foo bar)"
  :keys "DEL"
  :after "|foo bar)")

(dumbparens-test delete-close-paren-forward
  "You can delete a close paren from in front"
  :before "(foo bar|)"
  :keys "C-d"
  :after "(foo bar|")

(dumbparens-test delete-close-paren-backward
  "You can delete a close paren from behind"
  :before "(foo bar)|"
  :keys "DEL"
  :after "(foo bar|")

(dumbparens-test re-type-missing-close-paren
  "Regression: You can type a close paren even if parens are imbalanced"
  :before "(()|"
  :keys ")"
  :after "(())|")

(dumbparens-test delete-escaped-quote
  "Regression: You can delete an escaped quote without breaking the string"
  :before "\"\\\"|\""
  :keys "DEL"
  :after "\"\\|\"")

(dumbparens-test delete-pair-after-space
  "SP regression: Deleting a pair does not also delete the preceding space"
  :before " (|)"
  :keys "DEL"
  :after " |")

(dumbparens-test forward-to-end-of-symbol
  "You can use C-M-f to move to the end of a symbol"
  :before "light|ness"
  :keys "C-M-f"
  :after "lightness|")

(dumbparens-test forward-over-symbol
  "You can use C-M-f to move over a symbol"
  :before "lightness| of being"
  :keys "C-M-f"
  :after "lightness of| being")

(dumbparens-test forward-out-of-list
  "You can use C-M-f to move out of a list"
  :before "(lightness of| ) being"
  :keys "C-M-f"
  :after "(lightness of )| being")

(dumbparens-test forward-over-list
  "You can use C-M-f to move over a list"
  :before "unbearable| (lightness of) being"
  :keys "C-M-f"
  :after "unbearable (lightness of)| being")

(dumbparens-test forward-with-prefix-arg
  "You can use C-M-f with a prefix arg to repeat multiple times"
  :before "((fo|o bar) baz quux)"
  :keys "C-u 4 C-M-f"
  :after "((foo bar) baz| quux)")

(dumbparens-test forward-out-of-string
  "Using C-M-f inside a string exits the string"
  :before "\"hello| world\""
  :keys "C-M-f"
  :after "\"hello world\"|")

(dumbparens-test forward-stops-at-punctuation
  "C-M-f stops between symbol and punctuation"
  :mode python
  :before "foo(|bar, baz, quux)"
  :keys "C-M-f"
  :after "foo(bar|, baz, quux)")

(dumbparens-test forward-passes-through-punctuation
  "C-M-f will continue through punctuation when needed"
  :mode python
  :before "foo(|bar, baz, quux)"
  :keys "C-u 2 C-M-f"
  :after "foo(bar, baz|, quux)")

(dumbparens-test forward-through-comments
  "C-M-f will skip comments"
  :before "foo| ; comment\n;; another comment\n  bar; lol\nbaz"
  :keys "C-M-f"
  :after "foo ; comment\n;; another comment\n  bar|; lol\nbaz")

(dumbparens-test forward-escapes-comments
  "C-M-f can escape the current comment"
  :before ";; com|ment\nfoo bar"
  :keys "C-M-f"
  :after ";; comment\nfoo| bar")

(dumbparens-test forward-includes-escapes-in-symbols
  "C-M-f counts escape sequences as part of symbols"
  :before "(foo |b\\(ar baz)"
  :keys "C-M-f"
  :after "(foo b\\(ar| baz)")

(dumbparens-test forward-from-within-escape
  "C-M-f works correctly even starting in the middle of an escape sequence"
  :before "(foo b\\|(bar baz)"
  :keys "C-M-f"
  :after "(foo b\\(bar| baz)")

(dumbparens-test forward-skips-expression-prefixes-before-symbols-only
  "C-M-f skips expression prefixes before symbols but not after"
  :before "| ,foo, bar"
  :keys "C-M-f"
  :after " ,foo|, bar")

(dumbparens-test backward-to-beginning-of-symbol
  "You can use C-M-b to move to the beginning of a symbol"
  :before "light|ness"
  :keys "C-M-b"
  :after "|lightness")

(dumbparens-test backward-over-symbol
  "You can use C-M-b to move over a symbol backwards"
  :before "lightness of |being"
  :keys "C-M-b"
  :after "lightness |of being")

(dumbparens-test backward-out-of-list
  "You can use C-M-b to move out of a list backwards"
  :before "lightness ( |of being)"
  :keys "C-M-b"
  :after "lightness |( of being)")

(dumbparens-test backward-with-prefix-arg
  "You can use C-M-b with a prefix arg to repeat multiple times"
  :before "(foo bar (baz qu|ux))"
  :keys "C-u 4 C-M-b"
  :after "(foo |bar (baz quux))")

(dumbparens-test backward-out-of-string
  "Using C-M-b inside a string exits the string"
  :before "\"hello |world\""
  :keys "C-M-b"
  :after "|\"hello world\"")

(dumbparens-test backward-skips-punctuation-after-symbols-only
  "C-M-b skips punctuation after symbols but not before"
  :before "foo(bar,baz,|quux)"
  :keys "C-M-b"
  :after "foo(bar,|baz,quux)")

(dumbparens-test backward-through-comments
  "C-M-b will skip comments"
  :before "foo bar ; baz\n |quux"
  :keys "C-M-b"
  :after "foo |bar ; baz\n quux")

(dumbparens-test backward-escapes-comments
  "C-M-b can escape the current comment"
  :before "foo bar ; ba|z\n quux"
  :keys "C-M-b"
  :after "foo |bar ; baz\n quux")

(dumbparens-test backward-includes-escapes-in-symbols
  "C-M-b counts escape sequences as part of symbols"
  :before "(foo b\\(ar| baz)"
  :keys "C-M-b"
  :after "(foo |b\\(ar baz)")

(dumbparens-test backward-from-within-escape
  "C-M-b works correctly even starting in the middle of an escape sequence"
  :before "(foo b\\|(bar baz)"
  :keys "C-M-b"
  :after "(foo |b\\(bar baz)")

(dumbparens-test backward-over-string
  "You can use C-M-b to move backward over a string"
  :before "hello \"world\" |lol"
  :keys "C-M-b"
  :after "hello |\"world\" lol")

(dumbparens-test forward-with-negative-arg
  "You can use C-M-f to move backwards with a negative prefix arg"
  :before "light|ness"
  :keys "C-u -1 C-M-f"
  :after "|lightness")

(dumbparens-test backward-with-negative-arg
  "You can use C-M-b to move forward with a negative prefix arg"
  :before "light|ness"
  :keys "C-u -1 C-M-b"
  :after "lightness|")

(dumbparens-test up-forward-from-list
  "You can use C-M-n to move to the end of a list"
  :before "(foo (ba|r baz) quux)"
  :keys "C-M-n"
  :after "(foo (bar baz)| quux)")

(dumbparens-test up-forward-outside-list
  "Using C-M-n outside a list moves to the end of the buffer"
  :before "foo| (bar baz) quux"
  :keys "C-M-n"
  :after "foo (bar baz) quux| [End of buffer]")

(dumbparens-test up-forward-from-string
  "You can use C-M-n to move to the end of a string"
  :before "hello \"w (o|r) ld\" lol"
  :keys "C-M-n"
  :after "hello \"w (or) ld\"| lol")

(dumbparens-test up-forward-from-within-escape-sequence
  "C-M-n works correctly even within an escape sequence"
  :before "(foo bar\\|(baz) quux)"
  :keys "C-M-n"
  :after "(foo bar\\(baz)| quux)")

(dumbparens-test up-forward-from-comment
  "C-M-n ignores stuff in comments"
  :before "(foo ; bar (baz| quux)\nlol) qat"
  :keys "C-M-n"
  :after "(foo ; bar (baz quux)\nlol)| qat")

(dumbparens-test up-backward-from-list
  "You can use C-M-u to move to the beginning of a list"
  :before "(foo (ba|r baz) quux)"
  :keys "C-M-u"
  :after "(foo |(bar baz) quux)")

(dumbparens-test up-backward-outside-list
  "Using C-M-u outside a list moves to the end of the buffer"
  :before "foo (bar baz) |quux"
  :keys "C-M-u"
  :after "|foo (bar baz) quux [Beginning of buffer]")

(dumbparens-test up-backward-from-string
  "You can use C-M-u to move to the beginning of a string"
  :before "hello \"w (o|r) ld\" lol"
  :keys "C-M-u"
  :after "hello |\"w (or) ld\" lol")

(dumbparens-test up-backward-from-within-escape-sequence
  "C-M-u works correctly even within an escape sequence"
  :before "(foo bar\\|(baz) quux)"
  :keys "C-M-u"
  :after "|(foo bar\\(baz) quux)")

(dumbparens-test up-backward-from-comment
  "C-M-u ignores stuff in comments"
  :before "(foo ; bar (baz| quux)\nlol) qat"
  :keys "C-M-u"
  :after "|(foo ; bar (baz quux)\nlol) qat")

(dumbparens-test kill-from-bol
  "C-k kills to the end of the current line"
  :before "foo\n|bar\nbaz\n"
  :keys "C-k"
  :after "foo\n|\nbaz\n")

(dumbparens-test kill-empty-line
  "C-k kills an empty line"
  :before "foo\n|\nbaz\n"
  :keys "C-k"
  :after "foo\n|baz\n")

(dumbparens-test kill-inside-list
  "C-k kills only to end of list"
  :before "foo (bar| baz) quux"
  :keys "C-k"
  :after "foo (bar|) quux")

(dumbparens-test kill-inside-list-backwards
  "C-k backwards kills only to beginning of list"
  :before "foo (bar| baz) quux"
  :keys "C-u 0 C-k"
  :after "foo (| baz) quux")

(dumbparens-test kill-entire-line
  "C-k includes the trailing newline with a prefix arg"
  :before "foo\n|bar\nbaz\n"
  :keys "C-u 1 C-k"
  :after "foo\n|baz\n")

(dumbparens-test kill-two-lines
  "C-k kills multiple lines with a prefix arg"
  :before "foo\n|bar\nbaz\nquux\n"
  :keys "C-u 2 C-k"
  :after "foo\n|quux\n")

(dumbparens-test kill-two-lines-backwards
  "C-k kills multiple lines backward with a negative prefix arg"
  :before "foo\nbar\nbaz\n|quux\n"
  :keys "C-u -2 C-k"
  :after "foo\n|quux\n")

(dumbparens-test kill-inside-string
  "C-k kills only to end of string"
  :before "foo \"bar| baz\" quux"
  :keys "C-k"
  :after "foo \"bar|\" quux")

(dumbparens-test kill-to-end-of-buffer
  "C-k can kill to end of buffer with no error"
  :before "foo| bar"
  :keys "C-k"
  :after "foo|")

(dumbparens-test kill-to-end-of-buffer-with-prefix-arg
  "C-k with prefix arg can kill to end of buffer with no error"
  :before "foo| bar"
  :keys "C-u 4 C-k"
  :after "foo|")

(dumbparens-test kill-inside-escape
  "C-k handles escape sequences correctly"
  :before "(foo bar\\|(baz quux)"
  :keys "C-k"
  :after "(foo bar\\|)")

;; (dumbparens-test wrap-symbol
;;   "You can wrap a symbol with M-("
;;   :before "foo b|ar baz"
;;   :keys "M-("
;;   :after "foo (|bar) baz")

;; (dumbparens-test wrap-list
;;   "You can wrap a list with M-("
;;   :before "foo| (bar baz) quux"
;;   :keys "M-("
;;   :after "foo(| (bar baz)) quux")

;; (dumbparens-test wrap-string
;;   "You can wrap a string with M-("
;;   :before "foo| \"bar baz\" quux"
;;   :keys "M-("
;;   :after "foo(| \"bar baz\") quux")

;; (dumbparens-test wrap-string-from-inside
;;   "You can wrap a string from inside of it with M-("
;;   :before "foo \"bar| baz\" quux"
;;   :keys "M-("
;;   :after "foo (|\"bar baz\") quux")

;; (dumbparens-test wrap-symbol-with-string
;;   "You can wrap a symbol with a string"
;;   :before "foo | bar baz"
;;   :keys "M-\""
;;   :after "foo \" bar\" baz")

;; (dumbparens-test wrap-string-with-string-from-inside
;;   "You can't wrap a string in another string"
;;   :before "foo \"bar| baz\" quux"
;;   :keys "M-\""
;;   :after "foo \"|bar baz\" quux")

;; (dumbparens-test wrap-two-symbols
;;   "You can use a prefix argument to wrap multiple forms with M-("
;;   :before "foo| bar baz quux"
;;   :keys "C-u 2 M-("
;;   :after "foo( bar baz) quux")

;; (dumbparens-test wrap-too-many-symbols
;;   "Large prefix arg wraps as many symbols as possible"
;;   :before "foo (bar |baz quux) lol"
;;   :keys "C-u 4 M-("
;;   :after "foo (bar (|baz quux)) lol")

;; (dumbparens-test wrap-backwards
;;   "You can use M-( with a negative prefix arg"
;;   :before "foo bar |baz"
;;   :keys "C-u -1 M-("
;;   :after "foo (bar |)baz")

;; (dumbparens-test wrap-backwards-from-inside
;;   "You can use M-( with negative prefix arg on the current symbol"
;;   :before "foo b|ar baz"
;;   :keys "C-u -1 M-("
;;   :after "foo (bar) baz")

;; (dumbparens-test wrap-on-empty-buffer
;;   "You can use M-( even with nothing to wrap"
;;   :before "|"
;;   :keys "M-("
;;   :after "(|)")

;; (dumbparens-test wrap-nothing
;;   "You can use a zero prefix arg on M-( to wrap zero forms"
;;   :before "foo b|ar baz"
;;   :keys "C-u 0 M-("
;;   :after "foo (|)bar baz")

;; (dumbparens-test wrap-through-comment
;;   "M-( will skip comments to find forms to wrap"
;;   :before "foo| ; bar\nbaz"
;;   :keys "M-("
;;   :after "foo(| ;bar\nbaz)")

;; (dumbparens-test wrap-from-within-comment
;;   "M-( will jump out of comments before wrapping"
;;   :before "foo ; b|ar\nbaz"
;;   :keys "M-("
;;   :after "foo ; bar\n(baz)")

;; (dumbparens-test wrap-with-square
;;   "M-[ wraps with square brackets"
;;   :before "|foo"
;;   :keys "M-["
;;   :after "[|foo]")

;; (dumbparens-test wrap-with-curly
;;   "C-{ wraps with curly braces"
;;   :mode c
;;   :before "|foo"
;;   :keys "C-{"
;;   :after "{|foo}")

;; (dumbparens-test wrap-with-single-quote
;;   "M-' wraps with single quotes"
;;   :mode python
;;   :before "|foo"
;;   :keys "M-'"
;;   :after "'|foo'")

;; (dumbparens-test wrap-string-and-symbol-with-string
;;   "Combine a symbol backwards into a string with M-\""
;;   :before "|\"foo\" bar"
;;   :keys "C-u 2 M-\""
;;   :after "\"|foo bar\"")

;; (dumbparens-test wrap-symbol-and-string-with-string
;;   "Combine a symbol forwards into a string with M-\""
;;   :before "|foo \"bar\""
;;   :keys "C-u 2 M-\""
;;   :after "\"|foo bar\"")

;; (dumbparens-test wrap-string-and-string-with-string
;;   "Combine two strings with M-\""
;;   :before "|\"foo\" \"bar\""
;;   :keys "C-u 2 M-\""
;;   :after "\"foo bar\"")

;; (dumbparens-test wrap-extending-math-bidirectionally
;;   "Use C-( to wrap multiple forms with math mode in TeX"
;;   :mode latex
;;   :before "|foo $bar$ baz"
;;   :keys "C-u 3 C-( $"
;;   :after "$|foo bar baz$")

;; (dumbparens-test wrap-nested-string
;;   "Wrap a list containing a string with M-\""
;;   :before "|(foo \"bar\" baz)"
;;   :keys "M-\""
;;   :after "\"|(foo bar baz)\"")

;; (dumbparens-test wrap-string-with-different-string
;;   "Use M-\" to change quote style"
;;   :mode python
;;   :before "\"foo| bar\""
;;   :keys "M-'"
;;   :after "'|foo bar'")

;; (dumbparens-test wrap-string-handles-embedded-quotes
;;   "When changing quote style, embedded quotes are escaped"
;;   :mode python
;;   :before "\"Mo'|at\""
;;   :keys "M-'"
;;   :after "|'Mo\\'|at'")

;; (dumbparens-test wrap-string-handles-embedded-escaped-quotes
;;   "When changing quote style, embedded quotes are not re-escaped"
;;   :mode python
;;   :before "'Mo\\|'|at'"
;;   :keys "M-\""
;;   :after "\"|Mo\\\"|at\"")

;; (dumbparens-test wrap-round-to-square
;;   "You can use M-[ with prefix arg to replace existing parens"
;;   :before "foo (bar| baz) quux"
;;   :keys "C-u M-["
;;   :after "foo [bar| baz] quux")

;; multiple C-u's
;; M-)

(provide 'dumbparens-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dumbparens.el ends here
