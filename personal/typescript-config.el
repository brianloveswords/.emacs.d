;;; https://gist.github.com/ef4/6590b0843bae5c58f977 by Edward Faulkner
;; Install these packages from melpa: tide, company
;; You also need `npm install -g tslint`

(require 'company)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save
                                                        idle-change
                                                        new-line
                                                        mode-enabled))
            (eldoc-mode t)
            (company-mode-on)))

(setq company-tooltip-align-annotations t)

(flycheck-def-config-file-var flycheck-tslint tslint "tslint.json"
  :safe #'stringp)

;; tide provides a built-in flycheck checker that talks to tsserver (the typescript compiler daemon).
;; But I also want tslint, so:
(flycheck-define-checker tslint
  "Use tslint to flycheck TypeScript code."
  :command ("tslint"
            (config-file "-c" flycheck-tslint)
            "-t" "prose"
            source)
  :error-patterns ((warning (file-name) "[" line ", " column "]: " (message)))
  :modes (typescript-mode)
  :next-checkers (typescript-tide))

;; Requiring tide here so that it will append its own flycheck checker before I add tslint
(require 'tide)

;; Add tslint, which will then invoke typescript-tide checker.
(add-to-list 'flycheck-checkers 'tslint)
