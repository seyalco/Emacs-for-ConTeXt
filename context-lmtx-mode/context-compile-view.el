;;; context-compile-view.el --- Compile and view ConTeXt documents cross-platform -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0
;; Keywords: context, pdf, compile
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;
;; This module adds helper commands for working with ConTeXt (.ctx) documents directly from Emacs.
;; Features:
;; - `C-c c` → Compile the current ConTeXt file using `context` CLI.
;; - `C-c v` → View the generated PDF with the default system viewer (cross-platform).
;; - Automatically closes the compilation buffer if compilation is successful.
;; - Displays compilation errors in a dedicated bottom-side window if they occur.
;;
;; Supported platforms for PDF viewing:
;; - GNU/Linux → Uses `evince` if available, otherwise falls back to `xdg-open`.
;; - macOS → Uses `open`.
;; - Windows → Uses `cmd /c start` with the default associated PDF application.
;;
;; Usage:
;;   (require 'context-compile-view)
;;   Make sure `context-lmtx-mode` is loaded before the keybindings are applied.
;;

;;; Code:

(require 'compile)

;; --------------------------------------------------
;; View PDF Cross-Platform
;; --------------------------------------------------
(defun context-view-pdf ()
  "Open the compiled PDF for the current ConTeXt file using the default system viewer.
This function works on GNU/Linux, macOS, and Windows."
  (interactive)
  (let* ((pdf-path (expand-file-name
                    (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))))
    (if (file-exists-p pdf-path)
        (cond
         ;; GNU/Linux: Prefer evince, fallback to xdg-open
         ((eq system-type 'gnu/linux)
          (if (executable-find "evince")
              (start-process "context-view" nil "evince" pdf-path)
            (start-process "context-view" nil "xdg-open" pdf-path)))

         ;; macOS: use 'open'
         ((eq system-type 'darwin)
          (start-process "context-view" nil "open" pdf-path))

         ;; Windows: use 'start' via cmd.exe, with empty "" arg required
         ((eq system-type 'windows-nt)
          (start-process "context-view" nil "cmd" "/c" "start" "" pdf-path))

         (t (error "Unsupported operating system: %s" system-type)))
      (message "❌ PDF not found: %s" pdf-path))))

;; --------------------------------------------------
;; Compile Current File
;; --------------------------------------------------
(defun compile-context-file ()
  "Compile the current ConTeXt file using the `context` CLI command.
The output is shown in the *ConTeXt Compilation* buffer. If there are no errors,
the compilation buffer will close automatically."
  (interactive)
  (let ((file (file-name-nondirectory (buffer-file-name)))
        (compilation-buffer-name-function
         (lambda (_) "*ConTeXt Compilation*")))
    (compile (format "context '%s'" file))))

;; --------------------------------------------------
;; Detect Errors in Compilation Buffer
;; --------------------------------------------------
(defun my/context--buffer-has-errors-p (buf)
  "Return non-nil if compilation BUF contains the word 'error'."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "error" nil t))))

;; --------------------------------------------------
;; Show Compilation Errors in Side Window
;; --------------------------------------------------
(defun my/context--show-errors-side (buf status)
  "Display BUF in a bottom side window if compilation failed.
If compilation succeeded, close the compilation buffer automatically."
  (if (and (string-match "finished" status)
           (not (my/context--buffer-has-errors-p buf)))
      ;; Success → close compilation buffer
      (kill-buffer buf)
    ;; Failure → open side window
    (let ((win (display-buffer-in-side-window
                buf '((side . bottom) (window-height . 15)))))
      (select-window win)
      ;; Append a friendly hint at the end of the log
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n--------------------------------------------------\n")
        (insert "⚠ Compilation encountered errors\n")
        (insert "Press q or C-c C-q to close this window\n"))
      (my/transient-error-mode 1))))

;; --------------------------------------------------
;; Temporary Minor Mode for Error Window Interaction
;; --------------------------------------------------
(defvar my/transient-error-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'my/context--quit-error-window)
    (define-key map (kbd "C-c C-q") #'my/context--quit-error-window)
    map)
  "Keymap for `my/transient-error-mode' to close the compilation error window.")

(define-minor-mode my/transient-error-mode
  "Minor mode for closing a transient ConTeXt compilation error window.
Available commands:
- `q` → close the window
- `C-c C-q` → close the window"
  :lighter " ErrWin"
  :keymap my/transient-error-mode-map)

(defun my/context--quit-error-window ()
  "Close the transient error window and kill the associated buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (delete-window)
    (kill-buffer buf)))

;; Hook into compilation finish to run our error display logic
(add-hook 'compilation-finish-functions #'my/context--show-errors-side)

;; --------------------------------------------------
;; Install Key Bindings for context-lmtx-mode
;; --------------------------------------------------
(with-eval-after-load 'context-lmtx-mode
  (define-key context-lmtx-mode-map (kbd "C-c c") #'compile-context-file)
  (define-key context-lmtx-mode-map (kbd "C-c v") #'context-view-pdf))

(provide 'context-compile-view)
;;; context-compile-view.el ends here

