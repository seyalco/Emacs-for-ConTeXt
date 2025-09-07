;;; context-env-expand.el --- Auto-expand ConTeXt environments -*- lexical-binding: t; -*-

;; Author: Your Name
;; Keywords: context, editing, environment
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.2

;;; Commentary:
;; Provides helper functions to quickly expand \startENV lines into full
;; \startENV ... \stopENV blocks when editing ConTeXt LMTX documents.

;;; Code:

(require 'thingatpt)

;;;###autoload
(defun context-expand-start-environment ()
  "If line starts with '\\startENV', expand into full environment block.
Cursor will be placed inside the block."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "^\\\\start\\([A-Za-z]+\\)" line)
      (let* ((env (match-string 1 line))
             (stop-line (format "\\stop%s" env)))
        (end-of-line)
        (newline)
        (insert "\n" stop-line)
        (forward-line -1)
        (indent-according-to-mode)))))

;;;###autoload
(defun context-tab-handler ()
  "Smart TAB in ConTeXt:
If current line has only a \\startENV, expand it.
Otherwise, do normal indent."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "^\\\\start\\([A-Za-z]+\\)\\s-*$" line)
        (context-expand-start-environment)
      (indent-for-tab-command))))

;; Keybindings are set only after `context-lmtx-mode` is loaded
(with-eval-after-load 'context-lmtx-mode
  (define-key context-lmtx-mode-map (kbd "C-c t") #'context-expand-start-environment)
  (define-key context-lmtx-mode-map (kbd "TAB")    #'context-tab-handler))

(provide 'context-env-expand)

;;; context-env-expand.el ends here

