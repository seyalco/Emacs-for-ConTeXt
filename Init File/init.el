;;; init.el --- My Emacs Configuration --- -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; Package Initialization
;; ------------------------------------------------------------
;; مسیر پکیج‌های شخصی
(let ((base "~/.emacs.d/Packages/"))
  (dolist (path '("context-lmtx-mode"
                  "polymode"
                  "company-mode"
                  "lua-mode"
                  "markdown-mode"))
    (add-to-list 'load-path (expand-file-name path base))))

;; لود فایل اصلی ماژول
(require 'context-lmtx-mode)

;; لود مستقیم Optional ها (بدون عبور از load-path)
(load (expand-file-name
       "context-lmtx-mode/Optional/myextras-loader.el"
       "~/.emacs.d/Packages/"))

;;; init.el ends here

