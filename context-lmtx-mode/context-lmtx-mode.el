;;; context-lmtx-mode.el --- Major mode for ConTeXt LMTX in Emacs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.2
;; Keywords: tex, context, lmtx
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package provides a major mode for editing ConTeXt LMTX documents in Emacs.
;; Syntax highlighting rules are defined in `context-tex-syntax-highlight.el`.
;; Additional functionality includes:
;;  - Environment expansion for \\start... \\stop... constructs
;;  - Auto-indentation support
;;  - Integration with poly-mode for language embedding
;;  - Optional auto-completion via company-mode

;;; Code:

;;;; ---------------------------------------------------------------------------
;;;; Dependencies
;;;; ---------------------------------------------------------------------------
(require 'context-tex-syntax-highlight) ;; Load syntax highlighting definitions
(require 'context-compile-view)         ;; Support for compile & view functionality
(require 'poly-context-mode)             ;; Polymode integration
(require 'context-lmtx-commands)         ;; Command definitions
(require 'context-lmtx-autocomplete)     ;; Auto-completion definitions
(require 'context-env-expand)            ;; Environment expansion helpers

;;;; ---------------------------------------------------------------------------
;;;; Keymap Definition
;;;; ---------------------------------------------------------------------------
(defvar context-lmtx-mode-map
  (make-sparse-keymap)
  "Keymap for `context-lmtx-mode`.")

;;;; ---------------------------------------------------------------------------
;;;; Mode Definition
;;;; ---------------------------------------------------------------------------
;;;###autoload
(define-derived-mode context-lmtx-mode text-mode "ConTeXt-LMTX"
  "Major mode for editing ConTeXt LMTX documents."
  ;; Enable syntax highlighting rules
  (setq font-lock-defaults
        '(context-lmtx-font-lock-keywords nil nil nil nil
          (font-lock-multiline . t))))

;;;; ---------------------------------------------------------------------------
;;;; Provide Feature
;;;; ---------------------------------------------------------------------------
(provide 'context-lmtx-mode)

;;; context-lmtx-mode.el ends here

