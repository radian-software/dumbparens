;;; dumbparens.el --- Simple paren-matching solution -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 12 Dec 2019
;; Homepage: https://github.com/raxod502/dumbparens
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; Version: 0

;;; Commentary:

;; Please see https://github.com/raxod502/dumbparens for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;; Required reading on syntax tables:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html

(defun dumbparens--buffer-substring-with-parens (start end)
  "Like `buffer-substring' but add text properties at each paren.
Each character that functions like a paren gets a text property
`dumbparens--paren' whose value is the raw syntax descriptor.
START and END are as in `buffer-substring' but they must be in
the correct order."
  (let ((str (buffer-substring start end))
        (idx 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((desc (syntax-after (point))))
          (when (memq (car desc) '(4 5 7 8 14 15))
            (put-text-property idx (1+ idx) 'dumbparens--paren desc str)))
        (forward-char)
        (cl-incf idx)))
    str))

(defvar dumbparens--change-list nil
  "List of buffer changes to be handled on `post-command-hook'.
Each element is a three-item list of a marker, the old text, and
the new text.")

(defun dumbparens--before-change-function (start end)
  "Track buffer changes to be handled on `post-command-hook'."
  (push (list (set-marker (make-marker) start)
              (dumbparens--buffer-substring-with-parens start end) nil)
        dumbparens--change-list))

(defun dumbparens--after-change-function (start end len)
  "Track buffer changes to be handled on `post-command-hook'."
  (unless (= len (length (nth 1 (car dumbparens--change-list))))
    (error "after-change-functions: bookkeeping error"))
  (setf (nth 2 (car dumbparens--change-list))
        (dumbparens--buffer-substring-with-parens start end)))

(defun dumbparens--post-command-function ()
  "Fix up buffer to keep parens matched as much as possible."
  (setq dumbparens--change-list (nreverse dumbparens--change-list))
  (while dumbparens--change-list
    (cl-destructuring-bind (loc before after) (pop dumbparens--change-list)
      )))

;;;###autoload
(define-minor-mode dumbparens-mode
  "Minor mode for dealing with paired delimiters in a simple way."
  nil nil nil
  (if dumbparens-mode
      (progn
        (add-hook 'before-change-functions #'dumbparens--before-change-function
                  nil 'local)
        (add-hook 'after-change-functions #'dumbparens--after-change-function
                  nil 'local)
        (add-hook 'post-command-hook #'dumbparens--post-command-function
                  nil 'local))
    (remove-hook 'before-change-functions #'dumbparens--before-change-function
                 'local)
    (remove-hook 'after-change-functions #'dumbparens--after-change-function
                 'local)
    (remove-hook 'post-command-hook #'dumbparens--post-command-function
                 'local)))

;;;###autoload
(define-globalized-minor-mode dumbparens-global-mode
  dumbparens-mode dumbparens-mode)

;;;; Closing remarks

(provide 'dumbparens)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; dumbparens.el ends here
