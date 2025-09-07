;;; context-tex-syntax-highlight.el --- Syntax highlighting rules for ConTeXt LMTX -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.5
;; Keywords: tex, context, lmtx
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Syntax highlighting rules for ConTeXt LMTX with:
;;  - Full Unicode support for command names
;;  - Support for Zero Width Non-Joiner (U+200C) in commands
;;  - Different highlighting for environments (\start...\stop...)
;;  - Proper highlighting for escaped symbols
;;  - Comments support

;;; Code:

(defconst context-lmtx-command-char-class
  ;; Allowed characters in a ConTeXt command name:
  ;; [:word:] => Any Unicode letter, digit, or underscore
  ;; \u200c   => Zero Width Non-Joiner
  ;; : . -    => Additional valid characters in some commands
  "[[:word:]\u200c:.-]"
  "Allowed characters in a ConTeXt command name.")

(defvar context-lmtx-font-lock-keywords
  (list
   ;; 1. Comments (% ... until end of line)
   '("%.*$" . font-lock-comment-face)

   ;; 2. Environments: \start... and \stop... (highlight full match including backslash)
   `(,(concat "\\\\start" context-lmtx-command-char-class "+")
     0 font-lock-function-name-face)
   `(,(concat "\\\\stop" context-lmtx-command-char-class "+")
     0 font-lock-function-name-face)

   ;; 3. General commands (full match including the backslash)
   `(,(concat "\\\\" context-lmtx-command-char-class "+")
     0 font-lock-keyword-face)

   ;; 4. Escaped symbols: \#, \%, \{, ... (anything not in allowed class)
   `(,(concat "\\\\[^" context-lmtx-command-char-class "]")
     0 font-lock-constant-face))
  "Highlight rules for ConTeXt LMTX commands with Unicode and ZWNJ support.")

(provide 'context-tex-syntax-highlight)
;;; context-tex-syntax-highlight.el ends here

