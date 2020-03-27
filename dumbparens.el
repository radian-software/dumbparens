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
      (when (> arg 0)
        (delete-char (- arg))
        (let ((state (save-excursion (syntax-ppss))))
          (cond
           ;; If typing close paren, then skip out of current list
           ;; instead.
           ((and (null (nth 8 state))
                 (nth 1 state)
                 (memq (car (syntax-after (nth 1 state))) '(4 8))
                 (= inserted (cdr (syntax-after (nth 1 state)))))
            (let ((orig-point (point)))
              (cl-block nil
                (condition-case _
                    (up-list arg)
                  (error
                   (goto-char orig-point)
                   (insert (make-string arg inserted)))))))
           ;; If typing open paren, then insert close paren too.
           ((and (null (nth 8 state))
                 (memq (car (aref (syntax-table) inserted)) '(4 8)))
            (insert (make-string arg inserted))
            (save-excursion
              (insert
               (make-string arg (cdr (aref (syntax-table) inserted))))))
           ;; If typing quote at end of string, then type over the end
           ;; of the string instead.
           ((and (null (nth 4 state))
                 (nth 3 state)
                 (if (eq (nth 3 state) t)
                     (and (eq (char-after) inserted)
                          (= (car (syntax-after)) 15))
                   (and (eq (char-after) inserted)
                        (eq inserted (nth 3 state)))))
            (forward-char)
            (cl-decf arg)
            (while (> arg 0)
              (insert (make-string 2 inserted))
              (cl-decf arg 2)
              (when (< arg 0)
                (backward-char))))
           ;; If typing quote outside string, then insert matched
           ;; pair.
           ((and (null (nth 8 state))
                 (memq (car (aref (syntax-table) inserted)) '(7 15)))
            (insert (make-string arg inserted))
            (save-excursion
              (insert
               (make-string arg inserted))))
           ;; Otherwise, don't do anything special.
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
