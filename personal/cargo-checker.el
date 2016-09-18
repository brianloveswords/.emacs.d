(require 'flycheck)

(flycheck-define-checker cargo-rust
  "A Rust syntax checker using cargo rustc.
This syntax checker needs Rust 1.1 or newer.
See URL `http://www.rust-lang.org'."
  :command ("cargo" "rustc"
            "--lib"
            "--" "-Z" "no-trans"
            "--test")
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": "
          (one-or-more digit) ":" (one-or-more digit) " error: "
          (or
           ;; Multiline errors
           (and (message (minimal-match (one-or-more anything)))
                " [" (id "E" (one-or-more digit)) "]")
           (message))
          line-end)
   (warning line-start (file-name) ":" line ":" column ": "
            (one-or-more digit) ":" (one-or-more digit) " warning: "
            (message) line-end)
   (info line-start (file-name) ":" line ":" column ": "
         (one-or-more digit) ":" (one-or-more digit) " " (or "note" "help") ": "
         (message) line-end))
  :modes rust-mode
  :predicate (lambda ()
               (flycheck-buffer-saved-p)))

(add-to-list 'flycheck-checkers 'cargo-rust)
