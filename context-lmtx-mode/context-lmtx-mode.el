;;; context-lmtx-mode.el --- Major mode for ConTeXt LMTX in Emacs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.2
;; Keywords: tex, context, lmtx
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Major mode برای ویرایش اسناد ConTeXt LMTX.
;; قوانین هایلایت جداگانه در فایل context-lmtx-syntax-highlight.el تعریف شده‌اند.

;;; Code:
(require 'context-tex-syntax-highlight) ;; بارگذاری قواعد هایلایت


;; ================================================================
;; KEYMAP
(defvar context-lmtx-mode-map
  (let ((map (make-sparse-keymap)))
    ;; کلیدهای میانبر
    (define-key map (kbd "C-c c") #'compile-context-file)
    (define-key map (kbd "C-c v") #'view-context-file)
    (define-key map (kbd "C-c t") #'my/expand-start-environment)
    (define-key map (kbd "TAB")   #'my/context-tab-handler)
    map)
  "Keymap for `context-lmtx-mode`.")

;; ================================================================
;; MODE DEFINITION
;;;###autoload
(define-derived-mode context-lmtx-mode text-mode "ConTeXt-LMTX"
  "Major mode برای ویرایش اسناد ConTeXt LMTX."
  (setq font-lock-defaults
        '(context-lmtx-font-lock-keywords nil nil nil nil (font-lock-multiline . t))))

;; ================================================================
;; AUTO COMPILE & VIEW
(defun compile-context-file ()
  "Compile current ConTeXt file using `context`."
  (interactive)
  (compile (concat "context '" (file-name-nondirectory (buffer-file-name)) "'")))

(defun view-context-file ()
  "View compiled PDF with evince."
  (interactive)
  (async-shell-command
   (format "evince %s.pdf"
           (file-name-sans-extension
            (file-name-nondirectory (buffer-file-name))))))

;; ================================================================
;; AUTO START-STOP INSERTING
(defun my/expand-start-environment ()
  "Expand \\startENV to \\startENV ... \\stopENV and place point between."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "^\\\\start\\([A-Za-z]+\\)" line)
      (let* ((env (match-string 1 line))
             (stop (format "\\stop%s" env)))
        (end-of-line)
        (newline)
        (insert "\n" stop)
        (forward-line -1)
        (indent-according-to-mode)))))

(defun my/context-tab-handler ()
  "Smart TAB for expanding \\startXXX environments."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "^\\\\start\\([A-Za-z]+\\)\\s-*$" line)
        (my/expand-start-environment)
      (indent-for-tab-command))))

;; ================================================================
;; POLYMODE INTEGRATION
(require 'polymode)
(require 'lua-mode)
(require 'markdown-mode)

(define-hostmode poly-context-hostmode
  :mode 'context-lmtx-mode)

(define-innermode poly-context-lua-innermode
  :mode 'lua-mode
  :head-matcher "\\\\startluacode\\b"
  :tail-matcher "\\\\stopluacode\\b"
  :head-mode 'host
  :tail-mode 'host)

(define-innermode poly-context-markdown-innermode
  :mode 'markdown-mode
  :head-matcher "\\\\startmarkdowncode\\b"
  :tail-matcher "\\\\stopmarkdowncode\\b"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-context-mode
  :hostmode 'poly-context-hostmode
  :innermodes '(poly-context-lua-innermode
                poly-context-markdown-innermode))

(add-to-list 'auto-mode-alist '("\\.ctx\\'" . poly-context-mode))
(add-to-list 'auto-mode-alist '("\\.mkiv\\'" . poly-context-mode))
(add-to-list 'auto-mode-alist '("\\.mkxl\\'" . poly-context-mode))

;; ================================================================
;; Auto-Completion via company-mode
(require 'context-lmtx-commands)
(require 'context-lmtx-autocomplete)

(provide 'context-lmtx-mode)
;;; context-lmtx-mode.el ends here

