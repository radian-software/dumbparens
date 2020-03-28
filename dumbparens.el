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
      (dotimes (_ arg)
        (let ((state (save-excursion (syntax-ppss))))
          (cond
           ;; Type over a closing paren.
           ((and (null (nth 8 state))
                 (nth 1 state)
                 (memq (car (syntax-after (nth 1 state))) '(4 8))
                 (eq inserted (cdr (syntax-after (nth 1 state)))))
            (forward-char))
           ;; Insert a closing paren to match an open paren.
           ((and (null (nth 8 state))
                 (memq (car (aref (syntax-table) inserted)) '(4 8))
                 (or (eobp) (memq (car (syntax-after (point))) '(0 5 12))))
            (insert inserted)
            (save-excursion
              (insert (cdr (aref (syntax-table) inserted)))))
           ;; Type over a closing quote.
           ((and (null (nth 4 state))
                 (nth 3 state)
                 (if (eq (nth 3 state) t)
                     (and (eq (char-after) inserted)
                          (= (car (syntax-after)) 15))
                   (and (eq (char-after) inserted)
                        (eq inserted (nth 3 state)))))
            (forward-char))
           ;; Insert a closing quote to match an open quote.
           ((and (null (nth 8 state))
                 (memq (car (aref (syntax-table) inserted)) '(7 15)))
            (insert inserted)
            (save-excursion
              (insert inserted)))
           ;; Don't do anything special.
           (t
            (insert inserted))))))))

;;;###autoload
(define-minor-mode dumbparens-mode
  "Minor mode for dealing with paired delimiters in a simple way."
  nil nil nil
  (if dumbparens-mode
      (add-hook 'post-self-insert-hook #'dumbparens--post-self-insert-hook
                nil 'local)
    (remove-hook 'post-self-insert-hook #'dumbparens--post-self-insert-hook
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
