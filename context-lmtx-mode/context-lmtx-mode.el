;;; context-lmtx-mode.el --- Major mode for ConTeXt LMTX in Emacs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.3
;; Keywords: tex, context, lmtx
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Major mode for editing ConTeXt LMTX documents.

;;; Code:

;;;; ---------------------------------------------------------------------------
;;;; Dependencies
;;;; ---------------------------------------------------------------------------
(require 'context-tex-syntax-highlight) ;; Load syntax highlighting definitions
(require 'context-compile-view)         ;; Support for compile & view functionality
(require 'poly-context-mode)             ;; Polymode integration
(require 'context-lmtx-commands)         ;; Command definitions
(require 'context-lmtx-autocomplete)     ;; Auto-completion definitions
;;(require 'context-env-expand)            ;; Environment expansion helpers

;;;; ---------------------------------------------------------------------------
;;;; Keymap Definition
;;;; ---------------------------------------------------------------------------
(defvar context-lmtx-mode-map
  (let ((map (make-sparse-keymap)))
map)
  "Keymap for `context-lmtx-mode`.")

(defun my-context-lmtx-insert-tab ()
  "Insert spaces instead of a tab character."
  (interactive)
  (insert (make-string tab-width ?\s)))

;;;; ---------------------------------------------------------------------------
;;;; Mode Definition
;;;; ---------------------------------------------------------------------------
;;;###autoload
(define-derived-mode context-lmtx-mode text-mode "ConTeXt-LMTX"
  "Major mode for editing ConTeXt LMTX documents."

  ;; Syntax highlighting
  (setq font-lock-defaults
'(context-lmtx-font-lock-keywords nil nil nil nil
(font-lock-multiline . t)))

  ;; Local indentation settings
  (setq-local indent-tabs-mode nil) ;; فقط فاصله
  (setq-local tab-width 4)          ;; طول تب

  ;; Bind TAB locally
  (define-key context-lmtx-mode-map (kbd "TAB") #'my-context-lmtx-insert-tab))

;;;; ---------------------------------------------------------------------------
;;;; Provide Feature
;;;; ---------------------------------------------------------------------------
(provide 'context-lmtx-mode)

;;; context-lmtx-mode.el ends here
