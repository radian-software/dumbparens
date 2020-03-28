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

(defun dumbparens--post-self-insert-command ()
  "Insert or remove paired delimiters as necessary.
For use on `post-self-insert-hook'."
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
                 (null (nth 5 state))
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

(defun dumbparens--handle-delete-char (func &rest args)
  "Delete paired delimiters as necessary.
For use as `:around' advice on `delete-forward-char' and
`delete-backward-char'. FUNC and ARGS are as in any `:around'
advice."
  (cl-letf* ((delete-char (symbol-function #'delete-char))
             ((symbol-function #'delete-char)
              (lambda (n &optional killflag)
                (let ((num-matched 0)
                      (lhs-point (1- (point)))
                      (rhs-point (point))
                      (start (point))
                      (end (point)))
                  (cl-block nil
                    (dotimes (_ (abs n))
                      (let ((state (save-excursion (syntax-ppss))))
                        (unless (and (null (nth 5 state))
                                     (pcase (car (syntax-after lhs-point))
                                       (`4 (and (null (nth 8 state))
                                                (eq (char-after rhs-point)
                                                    (cdr (syntax-after lhs-point)))))
                                       (`7 (and (nth 3 state)
                                                (eq (char-after rhs-point)
                                                    (char-after lhs-point))))
                                       (`8 (and (null (nth 8 state))
                                                (eq (char-after rhs-point)
                                                    (char-after lhs-point))))
                                       (`15 (and (nth 3 state)
                                                 (eq 15 (car (syntax-after rhs-point)))))))
                          (cl-return))
                        (cl-incf num-matched)
                        (cl-decf lhs-point)
                        (cl-incf rhs-point))))
                  (cond
                   ((> n 0)
                    (cl-incf end n)
                    (cl-decf start num-matched))
                   ((< n 0)
                    (cl-decf start (- n))
                    (cl-incf end num-matched)))
                  (when killflag
                    (kill-new (buffer-substring start end)))
                  (delete-region start end)))))
    (apply func args)))

;;;###autoload
(define-minor-mode dumbparens-mode
  "Minor mode for dealing with paired delimiters in a simple way."
  nil nil nil
  (if dumbparens-mode
      (progn
        (add-hook 'post-self-insert-hook #'dumbparens--post-self-insert-command
                  nil 'local)
        (advice-add #'delete-forward-char :around
                    #'dumbparens--handle-delete-char)
        (advice-add #'delete-backward-char :around
                    #'dumbparens--handle-delete-char))
    (remove-hook 'post-self-insert-hook #'dumbparens--post-self-insert-command
                 'local)
    (advice-remove #'delete-forward-char #'dumbparens--handle-delete-char)
    (advice-remove #'delete-backward-char #'dumbparens--handle-delete-char)))

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
