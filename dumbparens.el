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

(defun dumbparens--post-self-insert-hook ()
  "Insert paired delimiter if necessary."
  (let ((syntax (syntax-after (1- (point)))))
    ;; Check if the user just inserted an open-parenthesis character.
    (when (= (syntax-class syntax) 4)
      (let ((state (syntax-ppss)))
        ;; Make sure we're not in a string or comment.
        (unless (nth 8 state)
          (save-excursion
            ;; Insert the corresponding close-parenthesis.
            (insert (cdr syntax))))))))

(define-minor-mode dumbparens-mode
  "Minor mode for dealing with paired delimiters in a simple way."
  nil nil nil
  (if dumbparens-mode
      (add-hook 'post-self-insert-hook #'dumbparens--post-self-insert-hook
                nil 'local)
    (remove-hook 'post-self-insert-hook #'dumbparens--post-self-insert-hook
                 nil 'local)))

(define-globalized-minor-mode dumbparens-global-mode
  dumbparens-mode dumbparens-mode)

;;;; Closing remarks

(provide 'dumbparens)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; dumbparens.el ends here
