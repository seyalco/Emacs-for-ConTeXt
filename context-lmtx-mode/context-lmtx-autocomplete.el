;;; context-lmtx-autocomplete.el --- ConTeXt LMTX autocomplete with Args and meta -*- lexical-binding: t; -*-

(require 'company)
(require 'cl-lib)
(require 'context-lmtx-commands)

(defun context-lmtx--extract-args (meta)
  "Extract Args: part from META string.
If found, returns it wrapped in brackets as in [ ... ]."
  (when (and meta (string-match "^Args: \\(.*\\)$" meta))
    (let ((args (match-string 1 meta)))
      ;; حذف فاصله اضافی
      (setq args (string-trim args))
      ;; اگر قبلاً براکت نداشت، خودمون اضافه کنیم
      (if (and (not (string-prefix-p "[" args))
               (not (string-suffix-p "]" args)))
          (format "[%s]" args)
        args))))

(defun context-lmtx--find-meta (candidate)
  "Return meta string for CANDIDATE from `context-lmtx-command-list'."
  (cl-loop for (cmd annotation meta) in context-lmtx-command-list
           when (string= cmd candidate)
           return meta))

(defun context-lmtx--colorize-meta (meta)
  "Add simple faces to META string for better readability."
  (when meta
    (let ((meta-copy (copy-sequence meta)))
      ;; هایلایت Args:
      (setq meta-copy
            (replace-regexp-in-string
             "^\\(Args:.*\\)$"
             (lambda (m) (propertize m 'face 'font-lock-keyword-face))
             meta-copy))
      ;; هایلایت Category/File/Level
      (setq meta-copy
            (replace-regexp-in-string
             "^\\(Category:\\)" (propertize "Category:" 'face 'font-lock-type-face)
             meta-copy))
      (setq meta-copy
            (replace-regexp-in-string
             "^\\(File:\\)" (propertize "File:" 'face 'font-lock-string-face)
             meta-copy))
      (setq meta-copy
            (replace-regexp-in-string
             "^\\(Level:\\)" (propertize "Level:" 'face 'font-lock-constant-face)
             meta-copy))
      meta-copy)))

(defun company-context-lmtx-backend (command &optional arg &rest ignored)
  "Company backend for ConTeXt LMTX commands."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-context-lmtx-backend))

    ;; فقط وقتی major-mode برابر context-lmtx-mode هست و قبلش \ وجود دارد
    (prefix
     (and (eq major-mode 'context-lmtx-mode)
          (looking-back "\\\\[A-Za-z]*" (line-beginning-position))
          (match-string 0)))

    ;; لیست پیشنهادها
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (mapcar #'car context-lmtx-command-list)))

    ;; متن کامل meta info در tooltip یا doc buffer
    (meta
     (let ((meta (context-lmtx--find-meta arg)))
       (context-lmtx--colorize-meta meta)))

    ;; متن کنار آیتم
    (annotation
     (let ((meta (context-lmtx--find-meta arg)))
       (or (context-lmtx--extract-args meta) "[ConTeXt]")))

    ;; وقتی آیتم انتخاب شد
    (post-completion
     (let* ((meta (context-lmtx--find-meta arg))
            (args (context-lmtx--extract-args meta)))
       ;; اول متن اولیه (مثل \define) رو پاک کن:
       (delete-region (- (point) (length arg)) (point))
       ;; سپس متن کامل رو وارد کن
       (if args
           (insert (concat arg args))
         (insert arg))))))

;; فعال شدن اتوماتیک در context-lmtx-mode
(add-hook 'context-lmtx-mode-hook
          (lambda ()
            (setq-local company-backends
                        (cons 'company-context-lmtx-backend company-backends))
            (company-mode 1)))

(provide 'context-lmtx-autocomplete)
;;; context-lmtx-autocomplete.el ends here

