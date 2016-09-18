(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

(defun my-fancy-newline ()
  "Add two newlines and put the cursor at the right indentation
between them if a newline is attempted when the cursor is between
two curly braces, otherwise do a regular newline and indent"
  (interactive)
  (if (and (equal (char-before) 123) ;; {
           (equal (char-after) 125)) ;; }
      (progn (newline-and-indent)
             (split-line)
             (indent-for-tab-command))
    (newline-and-indent)))

(defun my-todo-highlighter ()
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 1 font-lock-warning-face prepend))))

(defun my-enblocken ()
  (interactive)
  (kill-line)
  (insert-char 123)
  (newline-and-indent)
  (yank)
  (insert-char 125)
  (backward-char)
  (newline-and-indent)
  (previous-line)
  (beginning-of-line)
  (back-to-indentation))

(defun my-next-error-wrapped (&optional arg reset)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move forwards (or
backwards, if negative). With just C-u as prefix moves to first error"
  (interactive "P")
  (condition-case nil
      (call-interactively 'next-error)
    ('user-error (next-error 1 t))))

(defun my-jump-to-last-error (buffer)
  "Jump to last error in the BUFFER, this assumes that
the error is at last but third line"
  (save-selected-window
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))
    (forward-line -3)
    (call-interactively 'compile-goto-error)))

(defun my-previous-error-wrapped (&optional arg)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)."
  (interactive "P")
  (condition-case nil
      (if (compilation-buffer-p (current-buffer))
          (compilation-previous-error 1)
        (call-interactively 'previous-error))
    ('user-error (progn
                   (let ((error-buffer (next-error-find-buffer)))
                     ;; If the buffer has an associated error buffer use it to
                     ;; to move to last error
                     (if (and (not (eq (current-buffer) error-buffer))
                              (compilation-buffer-p error-buffer))
                         (my-jump-to-last-error error-buffer)
                       ;; Otherwise move to last point and invoke previous error
                       (goto-char (point-max))
                       (call-interactively 'previous-error)))))))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-c i"))
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-B") 'helm-mini)
(global-set-key (kbd "M-N") 'mc/mark-next-like-this)
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-j") 'my-fancy-newline)
(global-set-key (kbd "C-o") 'split-line)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-C--") 'text-scale-decrease)
(global-set-key (kbd "M-C-=") 'text-scale-increase)
(global-set-key (kbd "C-x e") 'delete-horizontal-space)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-<tab>") 'indent-rigidly)
(global-set-key (kbd "C-x f =") 'diff-buffer-with-file)
(global-set-key (kbd "C-x f d") 'dired)
(global-set-key (kbd "C-x f r") 'revert-buffer)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-M") 'magit-status)
(global-set-key (kbd "M-K") 'kill-buffer)
(global-set-key (kbd "C-M-s") 'helm-do-ag)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c !") 'org-time-stamp-inactive)
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key (kbd "M-`") 'org-agenda)
(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "M-R") 'query-replace-regexp)

(define-key prog-mode-map (kbd "C-x C-n") #'forward-page)
(define-key prog-mode-map (kbd "C-x C-p") #'backward-page)
(define-key prog-mode-map (kbd "C-c C-]") #'my-enblocken)
(define-key prog-mode-map (kbd "C-c C-n") 'my-next-error-wrapped)
(define-key prog-mode-map (kbd "C-c C-p") 'my-previous-error-wrapped)

(require 'php-mode)
(define-key php-mode-map (kbd "C-c C-n") 'my-next-error-wrapped)
(define-key php-mode-map (kbd "C-c C-p") 'my-previous-error-wrapped)

(require 'stylus-mode)
(define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-j") 'dired-find-file)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-h") 'helm-ff-delete-char-backward)
(define-key helm-projectile-find-file-map (kbd "C-h") 'helm-ff-delete-char-backward)
(define-key helm-find-files-map (kbd "C-h") 'helm-ff-delete-char-backward)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-o") 'occur-from-isearch)

(require 'typescript-mode)
(define-key typescript-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(define-key typescript-mode-map (kbd "C-c C-r") 'tide-rename-symbol)

(require 'company)

(require 'rust-mode)
(define-key rust-mode-map (kbd "<tab>") 'company-indent-or-complete-common)

(require 'js-doc)
(define-key js2-mode-map (kbd "C-c C-d") 'js-doc-insert-function-doc)
(define-key js2-mode-map "@" 'js-doc-insert-tag)

(require 'company)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "C-h") 'backward-delete-char)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-r")

(require 'smart-forward)
(global-set-key (kbd "C-M-p") 'smart-up)
(global-set-key (kbd "C-M-n") 'smart-down)
(global-set-key (kbd "C-M-b") 'smart-backward)
(global-set-key (kbd "C-M-f") 'smart-forward)

(require 'magit)
;; (define-key git-rebase-mode-map (kbd "f") #'(lambda ()
;;                                               (interactive)
;;                                               (git-rebase-fixup)
;;                                               (forward-line)))
;; (define-key git-rebase-mode-map (kbd "r") #'(lambda ()
;;                                               (interactive)
;;                                               (git-rebase-reword)
;;                                               (forward-line)))
;; (define-key git-rebase-mode-map (kbd "s") #'(lambda ()
;;                                               (interactive)
;;                                               (git-rebase-squash)
;;                                               (forward-line)))
