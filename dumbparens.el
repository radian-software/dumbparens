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
  "Insert or remove paired delimiters as necessary."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html
  (atomic-change-group
    (let ((arg (prefix-numeric-value current-prefix-arg))
          (inserted (char-before)))
      (delete-char (- arg))
      (let ((state (save-excursion (syntax-ppss))))
        (unless (nth 8 state)
          (cond
           ((when (nth 1 state)
              (when (memq (car (syntax-after (nth 1 state))) '(4 8))
                (when-let ((closer (cdr (syntax-after (nth 1 state)))))
                  (when (= inserted closer)
                    (prog1 t
                      (let ((orig-point (point)))
                        (cl-block nil
                          (condition-case _
                              (up-list arg)
                            (error
                             (goto-char orig-point)
                             (insert (make-string arg inserted))))))))))))
           ((memq (car (aref (syntax-table) inserted)) '(4 8))
            (insert (make-string arg inserted))
            (save-excursion
              (insert
               (make-string arg (cdr (aref (syntax-table) inserted))))))
           (t
            (insert (make-string arg inserted)))))))))

(define-minor-mode dumbparens-mode
  "Minor mode for dealing with paired delimiters in a simple way."
  nil nil nil
  (if dumbparens-mode
      (add-hook 'post-self-insert-hook #'dumbparens--post-self-insert-hook
                nil 'local)
    (remove-hook 'post-self-insert-hook #'dumbparens--post-self-insert-hook
                 'local)))

(define-globalized-minor-mode dumbparens-global-mode
  dumbparens-mode dumbparens-mode)

;;;; Closing remarks

(provide 'dumbparens)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; dumbparens.el ends here
