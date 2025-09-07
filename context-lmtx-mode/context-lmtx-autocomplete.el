;;; context-lmtx-autocomplete.el --- ConTeXt LMTX autocomplete -*- lexical-binding: t; -*-

(require 'company)
(require 'cl-lib)
(require 'context-lmtx-commands)

(defun context-lmtx--extract-args (meta)
  "Extract the first line of `Args:` from META string, preserving delimiters.
Does not add delimiters if META already contains raw arguments (e.g. delimiters=\"none\")."
  (when (and meta
             (string-match "^Args: \\(.*\\)$" meta))
    (let ((args (string-trim (match-string 1 meta))))
      ;; اگر از قبل با یکی از چهار نوع delimiter استاندارد شروع و تموم شده باشه:
      (if (or (and (string-prefix-p "[" args) (string-suffix-p "]" args))
              (and (string-prefix-p "{" args) (string-suffix-p "}" args))
              (and (string-prefix-p "(" args) (string-suffix-p ")" args))
              (and (string-prefix-p "<" args) (string-suffix-p ">" args)))
          args
        ;; اگر فقط متن ساده (delimiters="none") باشد → بدون براکت اضافی
        (if (string-match-p "^[A-Z0-9: _-]+$" args)
            (concat " " args) ;; فاصله برای جدا کردن از نام کامند
          args)))))

(defun context-lmtx--find-meta (candidate)
  "Find META string for CANDIDATE from command list."
  (cl-loop for (cmd ann meta) in context-lmtx-command-list
           when (string= cmd candidate)
           return meta))

(defun context-lmtx--colorize-meta (meta)
  "Colorize META for tooltip safely.
Escapes `\\` to `\\\\` first so that Company tooltip display does not
throw `Invalid use of ‘\\’ in replacement text` errors for commands
with backslashes like \\CSNAME."
  (when meta
    (let ((m (copy-sequence meta)))
      ;; 1️⃣ Escape امن بک‌اسلش‌ها
      (setq m (replace-regexp-in-string "\\\\" "\\\\\\\\" m))

      ;; 2️⃣ هایلایت بخش Args: ...
      (setq m (replace-regexp-in-string
               "^\\(Args:.*\\)$"
               (lambda (mm)
                 (propertize mm 'face 'font-lock-keyword-face))
               m))

      ;; 3️⃣ هایلایت سایر برچسب‌ها (Category, File, ...)
      (dolist (kw '(("Category:" . font-lock-type-face)
                    ("File:"     . font-lock-string-face)
                    ("Level:"    . font-lock-constant-face)
                    ("Keywords:" . font-lock-builtin-face)
                    ("Type:"     . font-lock-function-name-face)))
        (setq m (replace-regexp-in-string
                 (regexp-quote (car kw))
                 (lambda (_match)
                   (propertize (car kw) 'face (cdr kw)))
                 m)))
      m)))

(defun company-context-lmtx-backend (command &optional arg &rest ignored)
  "Company backend for ConTeXt LMTX commands."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-context-lmtx-backend))
    (prefix
     (and (eq major-mode 'context-lmtx-mode)
          (looking-back "\\\\[A-Za-z]*" (line-beginning-position))
          (match-string 0)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (mapcar #'car context-lmtx-command-list)))
    (meta
     (context-lmtx--colorize-meta (context-lmtx--find-meta arg)))
    (annotation
     (or (context-lmtx--extract-args (context-lmtx--find-meta arg)) "[ConTeXt]"))
    (post-completion
     (let* ((meta (context-lmtx--find-meta arg))
            (args (context-lmtx--extract-args meta)))
       ;; جایگزین کردن candidate با نسخه کامل + آرگومان‌ها
       (delete-region (- (point) (length arg)) (point))
       (insert (concat arg (or args "")))))))

;;; فعال‌سازی در major-mode مربوطه
(add-hook 'context-lmtx-mode-hook
          (lambda ()
            (setq-local company-backends
                        (cons 'company-context-lmtx-backend company-backends))
            (company-mode 1)))

(provide 'context-lmtx-autocomplete)
;;; context-lmtx-autocomplete.el ends here

