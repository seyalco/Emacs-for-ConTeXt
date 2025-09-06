;;; context-tex-syntax-highlight.el --- Syntax highlighting rules for ConTeXt LMTX -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Keywords: tex, context, lmtx
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; این فایل فقط قواعد هایلایت را تعریف می‌کند.
;; هیچ major mode یا keymapی در اینجا تعریف نمی‌شود.

;;; Code:

(defvar context-lmtx-font-lock-keywords
  (list
   ;; 1. Comments
   '("%.*$" . font-lock-comment-face)

   ;; 2. Commands: \word
   '("\\\\[a-zA-Z@]+" . font-lock-keyword-face)

   ;; 3. Symbols: \#, \%, \{, \}
   '("\\\\[^a-zA-Z@]" . font-lock-constant-face)

   ;; 4. Environment start/stop
   '("\\\\start[[:alpha:]]+" . font-lock-function-name-face)
   '("\\\\stop[[:alpha:]]+"  . font-lock-function-name-face))
  "Highlight rules for ConTeXt LMTX commands.")

(provide 'context-tex-syntax-highlight)
;;; context-tex-syntax-highlight.el ends here

