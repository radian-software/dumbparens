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

;; Required reading for developing this package:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html
;; the docstring for `parse-partial-sexp'
;;
;; Yes, the code is hard to read. But the logic is simple and we have
;; a lot of unit tests. The only things that make it hard are the
;; large number of cases and the fact that we have to use a lot of
;; arbitrary constants that you look up in tables (internal and
;; external syntax descriptors, indices into `syntax-ppss' return
;; values). On the plus side, once the code is written, it largely
;; does not need maintenance, because the syntax table is where the
;; buck stops: no endless series of weirder and weirder special cases
;; added by request, like in Smartparens.

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(defgroup dumbparens nil
  "Finally, reasonable parenthesis-matching for Emacs"
  :group 'convenience
  :prefix "dumbparens-"
  :link '(url-link "https://github.com/raxod502/dumbparens"))

(defcustom dumbparens-mode-bindings
  '(([remap delete-char]  . delete-forward-char)
    ("C-M-f"     . dumbparens-forward)
    ("C-M-b"     . dumbparens-backward)
    ("C-M-n"     . dumbparens-up-forward)
    ("C-M-u"     . dumbparens-up-backward)
    ("C-k"       . dumbparens-kill-line)
    ("C-("       . dumbparens-wrap)
    ("M-("       . dumbparens-wrap-round)
    ("M-["       . dumbparens-wrap-square)
    ("C-{"       . dumbparens-wrap-curly)
    ("M-s"       . dumbparens-splice)
    ("M-<down>"  . dumbparens-splice-killing-forward)
    ("M-<up>"    . dumbparens-splice-killing-backward)
    ("M-r"       . dumbparens-raise)
    ("M-?"       . dumbparens-convolute)
    ("C-<right>" . dumbparens-slurp-forward)
    ("C-<left>"  . dumbparens-barf-forward)
    ("M-S"       . dumbparens-split)
    ("M-j"       . dumbparens-join))
  "Keybindings enabled in `dumbparens-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`dumbparens-mode' is (re-)enabled. The keys are strings or raw
key events and the values are command symbols."
  :type '(alist
          :key-type sexp
          :value-type function)
  :set (lambda (var val)
         (set var val)
         (when (bound-and-true-p dumbparens-mode)
           (dumbparens-mode +1))))

(defun dumbparens--skip-syntax-forward (syntax)
  "Like `skip-syntax-forward', but signal `end-of-buffer' on failed search.
For SYNTAX see `skip-syntax-forward'."
  (prog1 (skip-syntax-forward syntax)
    (when (eobp)
      (signal 'end-of-buffer nil))))

(defun dumbparens--post-self-insert-command ()
  "Insert or remove paired delimiters as necessary.
For use on `post-self-insert-hook'."
  (atomic-change-group
    (let ((arg (prefix-numeric-value current-prefix-arg))
          (inserted (char-before)))
      (delete-char (- arg))
      (dotimes (_ arg)
        (let ((state (syntax-ppss)))
          (cond
           ;; Type over a closing paren.
           ((and (null (nth 8 state))
                 (nth 1 state)
                 (memq (car (syntax-after (nth 1 state))) '(4 8))
                 (eq inserted (cdr (syntax-after (nth 1 state))))
                 (eq inserted (char-after)))
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
                          (= (car (syntax-after (point))) 15))
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
  (cl-letf*
      (((symbol-function #'delete-char)
        (lambda (n &optional killflag)
          (let ((num-matched 0)
                (lhs-point (1- (point)))
                (rhs-point (point))
                (start (point))
                (end (point)))
            (cl-block nil
              (dotimes (_ (abs n))
                (let ((state (save-excursion (syntax-ppss lhs-point))))
                  (unless (and (null (nth 5 state))
                               (pcase (car (syntax-after lhs-point))
                                 (`4 (and (null (nth 8 state))
                                          (eq (char-after rhs-point)
                                              (cdr (syntax-after lhs-point)))))
                                 (`7 (and (null (nth 8 state))
                                          (eq (char-after rhs-point)
                                              (char-after lhs-point))))
                                 (`8 (and (null (nth 8 state))
                                          (eq (char-after rhs-point)
                                              (char-after lhs-point))))
                                 (`15 (and (null (nth 8 state))
                                           (eq 15 (car (syntax-after
                                                        rhs-point)))))))
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

;; Motion commands

(defun dumbparens--skip-whitespace-and-comments-forward
    (&optional include-punctuation)
  "Move forward over whitespace and comments from point.
INCLUDE-PUNCTUATION non-nil means to also move forward over
punctuation and expression prefixes."
  (cl-block nil
    (while t
      (let ((state (syntax-ppss)))
        (cond
         ;; If inside a string, we're done.
         ((nth 3 state)
          (cl-return))
         ;; If inside a comment, move out of it.
         ((nth 4 state)
          (goto-char (nth 8 state))
          (dumbparens--skip-syntax-forward
           (if (eq (car (syntax-after (point))) 11)
               "^>"
             "^!"))
          (forward-char))
         ;; Skip over whitespace, comment starters and enders (the
         ;; latter in case of empty comments), and possibly also
         ;; punctuation.
         ((memq (car (syntax-after (point)))
                (append '(0 11 12) (when include-punctuation '(1 6))))
          (skip-syntax-forward
           (concat "-<>" (when include-punctuation ".'"))))
         ;; Otherwise, we are done.
         (t
          (cl-return)))))))

(defun dumbparens--skip-whitespace-and-comments-backward
    (&optional include-punctuation)
  "Move backward over whitespace and comments from point.
INCLUDE-PUNCTUATION non-nil means to also move forward over
punctuation and expression prefixes."
  (cl-block nil
    (while t
      (let ((state (syntax-ppss)))
        (cond
         ;; If inside a string, we're done.
         ((nth 3 state)
          (cl-return))
         ;; If inside a comment, move out of it.
         ((nth 4 state)
          (goto-char (nth 8 state)))
         ;; Skip over whitespace, comment starters and enders (the
         ;; latter in case of empty comments), and possibly also
         ;; punctuation.
         ((memq (car (syntax-after (1- (point))))
                (append '(0 11 12) (when include-punctuation '(1 6))))
          (skip-syntax-backward
           (concat "-<>" (when include-punctuation ".'"))))
         ;; Otherwise, we are done.
         (t
          (cl-return)))))))

(defun dumbparens--up-string-forward ()
  "Assuming point is within a string, move after its closing quote."
  (let ((state (syntax-ppss)))
    (if-let ((closer (nth 3 state)))
        (progn
          (goto-char (nth 8 state))
          (forward-char)
          (cl-block nil
            (while t
              (dumbparens--skip-syntax-forward
               (if (eq closer t)
                   "^\\/|"
                 "^\"/|"))
              (when (if (eq closer t)
                        (eq (car (syntax-after (point))) 15)
                      (eq (char-after) closer))
                (forward-char)
                (cl-return))
              (if (memq (car (syntax-after (point))) '(9 10))
                  (forward-char 2)
                (forward-char)))))
      (error "Not currently inside a string"))))

(defun dumbparens-forward (&optional n)
  "Move to end of current or next form. With argument, repeat N times.
If at end of enclosing form, call `dumbparens-up-forward'
instead. With negative N, call `dumbparens-backward' instead."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (dumbparens-backward (- n))
    (dotimes (_ n)
      (let ((state (syntax-ppss)))
        (if (nth 3 state)
            ;; If inside string, move to end of it.
            (dumbparens--up-string-forward)
          (dumbparens--skip-whitespace-and-comments-forward
           'include-punctuation)
          (setq state (syntax-ppss))
          (when (nth 5 state)
            (condition-case _
                (forward-char)
              (end-of-buffer)))
          (cond
           ;; If at end of list, move out of it.
           ((and (nth 1 state)
                 (= (1+ (point))
                    (scan-lists (nth 1 state) 1 0)))
            (forward-char))
           ;; If at beginning of list, move over it.
           ((memq (car (syntax-after (point))) '(4 8))
            (goto-char (scan-lists (point) 1 0)))
           ;; If at beginning of string, move over it.
           ((memq (car (syntax-after (point))) '(7 15))
            (forward-char)
            (dumbparens--up-string-forward))
           ;; Otherwise, move over one symbol.
           (t
            (cl-block nil
              (while t
                (skip-syntax-forward "w_")
                (if (memq (car (syntax-after (point))) '(9 10))
                    (condition-case _
                        (forward-char 2)
                      (end-of-buffer (cl-return)))
                  (cl-return)))))))))))

(defun dumbparens-backward (&optional n)
  "Move to start of current or previous form. With argument, repeat N times.
If at beginning of enclosing form, call `dumbparens-up-backward'
instead. With negative N, call `dumbparens-forward' instead."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (dumbparens-forward (- n))
    (dotimes (_ n)
      (let ((state (syntax-ppss)))
        ;; If inside string, move out of it.
        (if (nth 3 state)
            (goto-char (nth 8 state))
          (dumbparens--skip-whitespace-and-comments-backward
           'include-punctuation)
          (setq state (syntax-ppss))
          (cond
           ;; If at beginning of list, move out of it.
           ((and (nth 1 state)
                 (= (1- (point))
                    (nth 1 state)))
            (backward-char))
           ;; If at end of list, move over it.
           ((memq (car (syntax-after (1- (point)))) '(5 8))
            (backward-char)
            (if-let ((beg (nth 1 (syntax-ppss))))
                (goto-char beg)
              (goto-char (point-min))
              (signal 'beginning-of-buffer nil)))
           ;; If at end of string, move over it.
           ((memq (car (syntax-after (1- (point)))) '(7 15))
            (backward-char)
            (if-let ((beg (nth 8 (syntax-ppss))))
                (goto-char beg)
              (goto-char (point-min))
              (signal 'beginning-of-buffer nil)))
           ;; Otherwise, move over one symbol.
           (t
            (cl-block nil
              (while t
                (skip-syntax-backward "w_\\/")
                (if (and (/= (point) (point-min))
                         (nth 5 (save-excursion
                                  (syntax-ppss (1- (point))))))
                    (condition-case _
                        (backward-char 2)
                      (beginning-of-buffer (cl-return)))
                  (cl-return)))))))))))

(defun dumbparens-up-forward (&optional n)
  "Move past end of enclosing form. With argument, repeat N times.
With negative N, call `dumbparens-up-backward' instead."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (dumbparens-up-backward (- n))
    (dotimes (_ n)
      (let ((state (syntax-ppss)))
        (if (nth 3 state)
            ;; If inside string, move to end of it.
            (dumbparens--up-string-forward)
          ;; Get out of any comments, since they may confuse
          ;; `scan-lists'.
          (dumbparens--skip-whitespace-and-comments-forward)
          (setq state (syntax-ppss))
          ;; If we're after an escape character, then `scan-lists'
          ;; will also do the wrong thing.
          (when (nth 5 state)
            (backward-char))
          ;; Otherwise, move out of current list.
          (condition-case _
              (goto-char (scan-lists (point) 1 1))
            (scan-error
             (goto-char (point-max))
             (signal 'end-of-buffer nil))))))))

(defun dumbparens-up-backward (&optional n)
  "Move before start of enclosing form. With argument, repeat N times.
With negative N, call `dumbparens-up-forward' instead."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (dumbparens-up-forward (- n))
    (dotimes (_ n)
      (let ((state (syntax-ppss)))
        (cond
         ;; If inside string, move to beginning of it.
         ((nth 3 state)
          (goto-char (nth 8 state)))
         ;; Otherwise, move out of current list.
         ((nth 1 state)
          (goto-char (nth 1 state)))
         ;; If not inside any list, we hit the beginning of the
         ;; buffer.
         (t
          (goto-char (point-min))
          (signal 'beginning-of-buffer nil)))))))

;; Deletion commands

(defun dumbparens-kill-line (&optional n)
  "Kill remainder of current line, stopping at close paren.
If killing an open paren, kill all the way to the close paren as
well. With argument N, kill that many lines in either direction,
as in `kill-line'. Without argument, don't kill the trailing
newline unless point is at end-of-line already."
  (interactive "P")
  (let* ((arg-given n)
         (n (or (prefix-numeric-value n) 1))
         (bound (save-excursion
                  (if (> n 0)
                      (condition-case _
                          (progn
                            (dumbparens-up-forward)
                            (1- (point)))
                        (end-of-buffer (point-max)))
                    (condition-case _
                        (progn
                          (dumbparens-up-backward)
                          (1+ (point)))
                      (beginning-of-buffer (point-min))))))
         (normal-kill-to
          (save-excursion
            (cond
             (arg-given (forward-line n))
             ((eolp) (condition-case _
                         (forward-char)
                       (end-of-buffer)))
             (t (end-of-line)))
            (point)))
         (keep-killing (lambda ()
                         (if (> n 0)
                             (< (point) normal-kill-to)
                           (> (point) normal-kill-to))))
         (kill-to (save-excursion
                    (cl-block nil
                      (while t
                        (if (> n 0)
                            (dumbparens--skip-whitespace-and-comments-forward)
                          (dumbparens--skip-whitespace-and-comments-backward))
                        (unless (funcall keep-killing)
                          (cl-return normal-kill-to))
                        (if (> n 0)
                            (dumbparens-forward)
                          (dumbparens-backward))
                        (unless (funcall keep-killing)
                          (cl-return (point))))))))
    (if (> n 0)
        (setq kill-to (min bound kill-to))
      (setq kill-to (max bound kill-to)))
    (kill-region (point) kill-to)))

;; Depth-changing commands

(defun dumbparens-wrap (char &optional n)
  "Wrap following form in paren CHAR and its matched pair.
With argument N, wrap in that many pairs. With negative N, wrap
preceding form."
  (interactive "c\np")
  (setq n (or n 1))
  (ignore char))

(defun dumbparens-wrap-round (&optional n)
  "Wrap following form in pair of round parens.
With argument N, wrap in that many pairs. With negative N, wrap
preceding form."
  (interactive "p")
  (setq n (or n 1))
  (dumbparens-wrap ?\( n))

(defun dumbparens-wrap-square (&optional n)
  "Wrap following form in pair of square brackets.
With argument N, wrap in that many pairs. With negative N, wrap
preceding form."
  (interactive "p")
  (setq n (or n 1))
  (dumbparens-wrap ?\[ n))

(defun dumbparens-wrap-curly (&optional n)
  "Wrap following form in pair of curly braces.
With argument N, wrap in that many pairs. With negative N, wrap
preceding form."
  (interactive "p")
  (setq n (or n 1))
  (dumbparens-wrap ?{ n))

(defun dumbparens-splice (&optional n)
  "Remove parens of enclosing form. With argument, repeat N times.
Negative N is the same as positive."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-splice-killing-forward (&optional n)
  "Kill to end of enclosing form and then remove parens.
With argument, repeat N times. With negative N, kill to start
instead."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-splice-killing-backward (&optional n)
  "Kill to start of enclosing form and then remove parens.
With argument, repeat N times. With negative N, kill to end
instead."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-raise (&optional n)
  "Kill contents of enclosing form except current form and then remove parens.
With argument, repeat N times. Negative N is the same as
positive."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-convolute (&optional n)
  "Convolute forms. Sorry, there's no simple explanation for this one.
Kill to start of enclosing form and splice. Then wrap the new
enclosing form and insert the killed forms at beginning, before
the new enclosing form. With argument N, go up N enclosing forms
instead of just one. Negative N is the same as positive."
  (interactive "p")
  (setq n (or n 1)))

;; Slurp/barf commands

(defun dumbparens-slurp-forward (&optional n)
  "Slurp next (or next N) forms into enclosing form.
With negative N, call `dumbparens-slurp-backward' instead."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-slurp-backward (&optional n)
  "Slurp previous (or previous N) forms into enclosing form.
With negative N, call `dumbparens-slurp-forward' instead."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-barf-forward (&optional n)
  "Barf last (or last N) forms out of enclosing form.
With negative N, call `dumbparens-barf-backward' instead."
  (interactive "p")
  (setq n (or n 1)))

(defun dumbparens-barf-backward (&optional n)
  "Barf first (or first N) forms out of enclosing form.
With negative N, call `dumbparens-barf-forward' instead."
  (interactive "p")
  (setq n (or n 1)))

;; Miscellaneous commands

(defun dumbparens-split ()
  "Split current form at point into two separate forms."
  (interactive))

(defun dumbparens-join ()
  "Join forms on either side of point into a single form."
  (interactive))

(defvar dumbparens-mode--keymap (make-sparse-keymap)
  "Keymap for `dumbparens-mode'. Populated when mode is enabled.
See `dumbparens-mode-bindings'.")

;;;###autoload
(define-minor-mode dumbparens-mode
  "Minor mode for dealing with paired delimiters in a simple way."
  :keymap dumbparens-mode--keymap
  (when dumbparens-mode
    ;; Hack to clear out keymap. Presumably there's a `clear-keymap'
    ;; function lying around somewhere...?
    (setcdr dumbparens-mode--keymap nil)
    (map-apply
     (lambda (key cmd)
       (when (stringp key)
         (setq key (kbd key)))
       (define-key dumbparens-mode--keymap key cmd))
     dumbparens-mode-bindings))
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
;; sentence-end-double-space: nil
;; End:

;;; dumbparens.el ends here
