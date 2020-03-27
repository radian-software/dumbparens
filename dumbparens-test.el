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

(cl-defmacro dumbparens-test (name desc &rest kws &key mode before keys after)
  "Declare a unit test."
  (declare (indent defun) (doc-string 2))
  `(setf (alist-get ',name dumbparens-tests) '(:desc ,desc ,@kws)))

(defun dumbparens-run-test (name)
  "Run a single unit test."
  (interactive
   (list
    (intern
     (completing-read
      "Run test: "
      (mapcar #'symbol-name (map-keys dumbparens-tests))))))
  (let* ((test (alist-get name dumbparens-tests))
         (bufname (format "*dumbparens test %S*" name))
         (failed nil))
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
           (setq failed (error-message-string e))
           (cl-return)))
        (insert "|")
        (unless (equal (buffer-string) (plist-get test :after))
          (setq failed "text does not match expected")
          (cl-return))))
    (if (not failed)
        (message "Test %S passed" name)
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
      (pop-to-buffer bufname))))

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

(provide 'dumbparens-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dumbparens.el ends here