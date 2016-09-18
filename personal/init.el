(server-start)
(sml/setup)
(disable-theme 'zenburn)
(color-theme-almost-monokai)
(unicode-fonts-setup)
(scroll-bar-mode 0)

(setq backup-by-copying t
      create-lockfiles nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist '(("." . "~/.emacs.d/autosaves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      visible-bell t
      js-doc-mail-address "brianloveswords@gmail.com"
      js-doc-author (format "Brian J Brennan <%s>" js-doc-mail-address)
      ;;js-doc-url "url of your website"
      js-doc-license "MIT")

(yas-global-mode 1)

(setq yas-snippet-dirs '("~/.emacs.d/personal/snippets"))

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(setq org-agenda-files (list "~/Dropbox/work.org"
                             "~/Dropbox/personal.org"))

(setq mac-option-modifier nil)
(setq org-default-notes-file "~/Dropbox/notes.org")


(setq org-agenda-custom-commands
      '(("h" "Habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if
             'notregexp ":@"))))
        ;; other commands here
        ))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/notes.org" "Tasks")
         "* TODO %?\n  %U\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/notes.org")
         "* %?\nEntered on %U\n\n%i")))

(setq org-catch-invisible-edits t)

(add-hook
 'org-mode-hook
 (lambda()
   (whitespace-mode 0)
   (company-mode 0)
   (visual-line-mode 1)
   (org-indent-mode 1)
   (yas/minor-mode 0)
   (flycheck-mode 0)))

(add-hook
 'markdown-mode-hook
 (lambda()
   (whitespace-mode 0)
   (visual-line-mode 1)
   (flyspell-mode 1)))

(add-hook
 'js2-mode-hook
 (lambda()
   (js2-refactor-mode 1)))

(add-hook
 'rust-mode-hook
 (lambda()
   (racer-mode 1)
   (whitespace-mode 0)
   (subword-mode 1)
   (toggle-truncate-lines 1)
   (flycheck-rust-setup)))

(add-hook
 'text-mode-hook
 (lambda()
   (whitespace-mode 0)
   (visual-line-mode 1)))

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook
 'js2-mode-hook
 (lambda() (subword-mode 1)))

(setq company-tooltip-align-annotations t)
(setq magit-last-seen-setup-instructions "1.4.0")

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Hack"))

(require 'undohist)
(undohist-initialize)

(setq org-capture-templates
      '(("w" "Work Todo" entry
         (file+headline "~/Dropbox/work.org" "Bocoup Tasks")
         "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:Effort:   ?
:Created:  %U
:END:

\n")
        ("p" "Personal Todo" entry
         (file+headline "~/Dropbox/personal.org" "Personal Tasks")
         "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:Created:  %U
:END:

\n")
        ("e" "From Email" entry
         (file+headline "~/Dropbox/personal.org" "From Email")
         "* TODO %u %? :email:
:PROPERTIES:
:Created:  %U
:END:

\n")
        ("j" "Journal" entry
         (file+datetree "~/Dropbox/journal.org")
         "* %?\nEntered on %U\n\n")
        ("K" "KAdam 1-on-1" entry
         (file+headline "~/Dropbox/work.org" "KAdam White") "* %U \n\n%?\n\n%i")
        ("S" "Sue 1-on-1" entry
         (file+headline "~/Dropbox/work.org" "Sue Lockwood") "* %U \n\n%?\n\n%i")
        ("P" "Pam 1-on-1" entry
         (file+headline "~/Dropbox/work.org" "Pam Drouin") "* %U \n\n%?\n\n%i")
        ("B" "Bmac 1-on-1" entry
         (file+headline "~/Dropbox/work.org" "Brendan McLoughlin") "* %U \n\n%?\n\n%i")
        ("W" "Wilto 1-on-1" entry
         (file+headline "~/Dropbox/work.org" "Mat Marquis") "* %U \n\n%?\n\n%i")))

(org-agenda nil "a")
