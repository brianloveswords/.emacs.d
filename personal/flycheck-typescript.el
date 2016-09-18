;;; flycheck-typescript.el --- Flychecker for TypeScript

;; Copyright (C) 2014 Yoshihisa Tanaka

;; Author: Yoshihisa Tanaka <yt.hisa@gmail.com>
;; Created: 8 Dec 2014
;; Version: 0.1.0
;; Keywords: convenience languages tools
;; Package-Requires: ((flycheck "0.15"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This requires external `tsc` command (TypeScript compiler).

;;; Code:
;; (require 'flycheck)

;; (flycheck-define-checker typescript
;;   "A TypeScript syntax checker using tsc command."
;;   :command ("tsc" "--out" "/dev/null" source-original)
;;   :error-patterns
;;   ((error line-start (file-name) "(" line "," column "): error " (message) line-end))
;;   :modes typescript-mode)

;; (add-to-list 'flycheck-checkers 'typescript)

;; (provide 'flycheck-typescript)
;;; flycheck-typescript.el ends here
