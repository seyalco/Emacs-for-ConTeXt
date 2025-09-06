;;; context-lmtx-mode.el --- Major mode for ConTeXt LMTX in Emacs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.2
;; Keywords: tex, context, lmtx
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Major mode برای ویرایش اسناد ConTeXt LMTX.
;; قوانین هایلایت جداگانه در context-lmtx-syntax-highlight.el تعریف شده‌اند.

;;; Code:
(require 'context-tex-syntax-highlight) ;; بارگذاری قواعد هایلایت

;; ایجاد کی‌مپ خالی در ابتدا برای استفاده در سراسر فایل
(defvar context-lmtx-mode-map
  (make-sparse-keymap)
  "Keymap for `context-lmtx-mode`.")

;; ================================================================
;; MODE DEFINITION
;;;###autoload
(define-derived-mode context-lmtx-mode text-mode "ConTeXt-LMTX"
  "Major mode برای ویرایش اسناد ConTeXt LMTX."
  (setq font-lock-defaults
        '(context-lmtx-font-lock-keywords nil nil nil nil (font-lock-multiline . t))))

;; ================================================================
;; AUTO COMPILE & VIEW;; Open PDF without popup
(defun view-context-file ()
  "View compiled PDF with evince without creating popup buffers."
  (interactive)
  (start-process "evince" nil
                 "evince"
                 (concat (file-name-sans-extension
                          (file-name-nondirectory (buffer-file-name))) ".pdf")))

(define-key context-lmtx-mode-map (kbd "C-c v") #'view-context-file)

;; Run ConTeXt compilation
(defun compile-context-file ()
  "Compile current ConTeXt file using `context`.
If successful, do nothing; if errors occur, show them in a transient side window."
  (interactive)
  (let ((file (file-name-nondirectory (buffer-file-name)))
        (compilation-buffer-name-function
         (lambda (_) "*ConTeXt Compilation*")))
    (compile (format "context '%s'" file))))

(define-key context-lmtx-mode-map (kbd "C-c c") #'compile-context-file)

;; Check for errors and display temporary window
(defun my/context--buffer-has-errors-p (buf)
  "Check if compilation BUF contains errors."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "error" nil t))))

(defun my/context--show-errors-side (buf status)
  "Show BUF in a side window only if compilation failed."
  (if (and (string-match "finished" status)
           (not (my/context--buffer-has-errors-p buf)))
      ;; If successful → close buffer
      (kill-buffer buf)
    ;; If errors → show side window at the bottom
    (let ((win (display-buffer-in-side-window
                buf '((side . bottom) (window-height . 15)))))
      (select-window win)
      ;; Add help message
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n--------------------------------------------------\n")
        (insert "⚠ The operation encountered an error\n")
        (insert "To close this window: press q or C-c C-q\n"))
      (my/transient-error-mode 1))))

;; Temporary mode for easy closing
(defvar my/transient-error-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'my/context--quit-error-window)
    (define-key map (kbd "C-c C-q") #'my/context--quit-error-window)
    map)
  "Keymap for transient error mode in ConTeXt compilation buffer.")

(define-minor-mode my/transient-error-mode
  "Minor mode for transient ConTeXt compilation windows."
  :lighter " ErrWin"
  :keymap my/transient-error-mode-map)

(defun my/context--quit-error-window ()
  "Close the transient error side window and kill buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (delete-window)
    (kill-buffer buf)))

;; Connect to finish hook
(add-hook 'compilation-finish-functions #'my/context--show-errors-side)

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

(define-key context-lmtx-mode-map (kbd "C-c t") #'my/expand-start-environment)

(defun my/context-tab-handler ()
  "Smart TAB for expanding \\startXXX environments."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "^\\\\start\\([A-Za-z]+\\)\\s-*$" line)
        (my/expand-start-environment)
      (indent-for-tab-command))))

(define-key context-lmtx-mode-map (kbd "TAB") #'my/context-tab-handler)

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

