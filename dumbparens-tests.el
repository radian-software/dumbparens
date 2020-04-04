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
         (failed nil))
    (unless (and (plist-get test :mode)
                 (symbolp (plist-get test :mode))
                 (stringp (plist-get test :before))
                 (stringp (plist-get test :keys))
                 (stringp (plist-get test :after)))
      (user-error "Incomplete test: %S" name))
    (when (get-buffer bufname)
      (kill-buffer bufname))
    (cl-block nil
      (save-window-excursion
        (pop-to-buffer bufname)
        (let ((mode (symbol-name (plist-get test :mode))))
          (when (equal mode "elisp")
            (setq mode "emacs-lisp"))
          (unless (string-suffix-p "-mode" mode)
            (setq mode (concat mode "-mode")))
          (setq mode (intern mode))
          (funcall mode))
        (dumbparens-mode +1)
        (when (bound-and-true-p smartparens-mode)
          (smartparens-mode -1))
        (when (bound-and-true-p electric-pair-mode)
          (electric-pair-mode -1))
        (when (bound-and-true-p paredit-mode)
          (paredit-mode -1))
        (save-excursion
          (insert (plist-get test :before)))
        (search-forward "|")
        (delete-region (match-beginning 0) (match-end 0))
        (condition-case e
            (execute-kbd-macro (kbd (plist-get test :keys)))
          (error
           (insert "|")
           (end-of-buffer)
           (insert " [" (error-message-string e) "]")
           (setq failed (error-message-string e))
           (cl-return)))
        (insert "|")
        (unless (equal (buffer-string) (plist-get test :after))
          (setq failed "text does not match expected")
          (cl-return))))
    (if (not failed)
        (progn
          (message "Test %S passed" name)
          t)
      (message "Test %S failed: %s" name failed)
      (with-current-buffer bufname
        (let ((actual (buffer-string)))
          (erase-buffer)
          (insert
           "BEFORE:\n\n"
           (plist-get test :before)
           "\n\nKEYS:\n\n"
           (plist-get test :keys)
           "\n\nEXPECTED:\n\n"
           (plist-get test :after)
           "\n\nGOT:\n\n"
           actual
           "\n"))
        (dumbparens-test-mode +1))
      (pop-to-buffer bufname)
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
  :mode elisp
  :before "|"
  :keys "("
  :after "(|)")

(dumbparens-test type-over-close
  "You can type over a close paren"
  :mode elisp
  :before "(|)"
  :keys ")"
  :after "()|")

(dumbparens-test open-string
  "Typing a quote should insert a matched pair"
  :mode elisp
  :before "|"
  :keys "\""
  :after "\"|\"")

(dumbparens-test type-over-close-quote
  "You can type over a close quote"
  :mode elisp
  :before "\"|\""
  :keys "\""
  :after "\"\"|")

(dumbparens-test delete-pair
  "Backspacing inside an empty pair should delete it"
  :mode elisp
  :before "(|)"
  :keys "DEL"
  :after "|")

(dumbparens-test delete-string
  "Backspacing inside an empty string should delete it"
  :mode elisp
  :before "\"|\""
  :keys "DEL"
  :after "|")

(dumbparens-test escape-cancels-type-over-close-quote
  "You won't type over a close quote right after a backslash"
  :mode elisp
  :before "\"\\|\""
  :keys "\""
  :after "\"\\\"|\"")

(dumbparens-test open-pair-with-prefix-arg
  "You can open multiple pairs at once with a prefix argument"
  :mode elisp
  :before "|"
  :keys "C-u 3 ("
  :after "(((|)))")

(dumbparens-test type-over-close-with-prefix-arg
  "You can type over multiple close parens at once with a prefix argument"
  :mode elisp
  :before "(((|)))"
  :keys "C-u 2 )"
  :after "((())|)")

(dumbparens-test delete-pair-forward
  "Typing C-d inside an empty pair should delete it"
  :mode elisp
  :before "(|)"
  :keys "C-d"
  :after "|")

(dumbparens-test type-lone-close-paren
  "You can type a close paren even with no open paren"
  :mode elisp
  :before "|"
  :keys ")"
  :after ")|")

(dumbparens-test no-pair-opened-before-symbol
  "No paired close paren is inserted right before a symbol"
  :mode elisp
  :before "|foo"
  :keys "("
  :after "(|foo")

(dumbparens-test double-escape-does-not-cancel-type-over-close-quote
  "You can type over a close quote after a double backslash"
  :mode elisp
  :before "\"\\\\|\""
  :keys "\""
  :after "\"\\\\\"|")

(dumbparens-test delete-open-paren
  "You can delete an open paren even if the close paren can't go with it"
  :mode elisp
  :before "(|foo bar)"
  :keys "DEL"
  :after "|foo bar)")

(dumbparens-test delete-close-paren-forward
  "You can delete a close paren from in front"
  :mode elisp
  :before "(foo bar|)"
  :keys "C-d"
  :after "(foo bar|")

(dumbparens-test delete-close-paren-backward
  "You can delete a close paren from behind"
  :mode elisp
  :before "(foo bar)|"
  :keys "DEL"
  :after "(foo bar|")

(dumbparens-test re-type-missing-close-paren
  "Regression: you can type a close paren even if parens are imbalanced"
  :mode elisp
  :before "(()|"
  :keys ")"
  :after "(())|")

(dumbparens-test delete-escaped-quote
  "Regression: You can delete an escaped quote without breaking the string"
  :mode elisp
  :before "\"\\\"|\""
  :keys "DEL"
  :after "\"\\|\"")

(dumbparens-test forward-to-end-of-symbol
  "You can use C-M-f to move to the end of a symbol"
  :mode elisp
  :before "light|ness"
  :keys "C-M-f"
  :after "lightness|")

(dumbparens-test forward-over-symbol
  "You can use C-M-f to move over a symbol"
  :mode elisp
  :before "lightness| of being"
  :keys "C-M-f"
  :after "lightness of| being")

(dumbparens-test forward-out-of-list
  "You can use C-M-f to move out of a list"
  :mode elisp
  :before "(lightness of| ) being"
  :keys "C-M-f"
  :after "(lightness of )| being")

(dumbparens-test forward-over-list
  "You can use C-M-f to move over a list"
  :mode elisp
  :before "unbearable| (lightness of) being"
  :keys "C-M-f"
  :after "unbearable (lightness of)| being")

(dumbparens-test forward-with-prefix-arg
  "You can use C-M-f with a prefix arg to repeat multiple times"
  :mode elisp
  :before "((fo|o bar) baz quux)"
  :keys "C-u 4 C-M-f"
  :after "((foo bar) baz| quux)")

(dumbparens-test forward-out-of-string
  "Using C-M-f inside a string exits the string"
  :mode elisp
  :before "\"hello| world\""
  :keys "C-M-f"
  :after "\"hello world\"|")

(dumbparens-test forward-with-negative-arg
  "You can use C-M-f to move backwards with a negative prefix arg"
  :mode elisp
  :before "light|ness"
  :keys "C-u -1 C-M-f"
  :after "|lightness")

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

(provide 'dumbparens-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dumbparens.el ends here
