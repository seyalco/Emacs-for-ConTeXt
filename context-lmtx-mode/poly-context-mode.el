;;; poly-context-mode.el --- Polymode for ConTeXt LMTX with easy LANG blocks -*- lexical-binding: t; -*-

(require 'polymode)

;; --- Host Mode ---
(define-hostmode poly-context-hostmode
  :mode 'context-lmtx-mode)

;; --- لیست innermodeها ---
(defvar poly-context-innermodes nil
  "List of registered innermodes for `poly-context-mode`.")

;; --- تابع افزودن زبان ---
(defun poly-context-add-lang (lang mode &optional mode-file)
  "Register a new LANG block for ConTeXt polymode.
LANG is a symbol like 'lua or 'python.
MODE is the major mode symbol (e.g. 'lua-mode).
MODE-FILE is optional file for autoload if MODE is not loaded."
  (let* ((lang-name (symbol-name lang))
         (inner-sym (intern (format "poly-context-%s-innermode" lang-name)))
         ;; الگوی شروع/پایان بلوک
         (head (format "^%%%%\\[%s\\] start\\s-*$" lang-name))
         (tail (format "^%%%%\\[%s\\] stop\\s-*$" lang-name)))
    ;; اگر mode فعلاً لود نشده، یک autoload ست کن
    (unless (fboundp mode)
      (autoload mode
        (or mode-file (symbol-name mode))
        (format "Major mode for %s code blocks." lang-name) t))
    ;; تعریف innermode
    (eval
     `(define-innermode ,inner-sym
        :mode ',mode
        :head-matcher ,head
        :tail-matcher ,tail
        :head-mode 'host
        :tail-mode 'host))
    ;; اضافه به لیست innermodes
    (add-to-list 'poly-context-innermodes inner-sym)))

;; --- تعریف زبان‌ها ---
(poly-context-add-lang 'lua 'lua-mode "lua-mode.el")
(poly-context-add-lang 'markdown 'markdown-mode "markdown-mode.el")
;; هر زبان جدید ↓ را همینجا اضافه کن
;; (poly-context-add-lang 'python 'python-mode "python.el")
;; (poly-context-add-lang 'html 'html-mode "html-mode.el")

;; --- تعریف polymode بعد از ثبت زبان‌ها ---
(define-polymode poly-context-mode
  :hostmode 'poly-context-hostmode
  :innermodes poly-context-innermodes)

;; --- اتصال به پسوندهای فایل ---
(dolist (ext '("\\.ctx\\'" "\\.mkiv\\'" "\\.mkxl\\'"))
  (add-to-list 'auto-mode-alist `(,ext . poly-context-mode)))

(provide 'poly-context-mode)
;;; poly-context-mode.el ends here

