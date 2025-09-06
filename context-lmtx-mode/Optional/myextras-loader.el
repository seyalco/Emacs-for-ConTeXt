;;; myextras-loader.el --- Load optional configurations for context-lmtx-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is responsible for loading all optional configuration modules
;; located in the same directory (the "Optional" folder) as this loader.
;;
;; The main purpose of this design is to:
;; - Prevent namespace collisions by NOT adding the Optional folder to `load-path`
;; - Ensure that all optional components are loaded from the correct location
;; - Keep optional features modular and maintainable
;;
;; Usage:
;; Simply load this file directly in your init.el (or main configuration) using:
;;
;;   (load (expand-file-name
;;          "context-lmtx-mode/Optional/myextras-loader.el"
;;          "~/.emacs.d/Packages/"))
;;
;; This ensures the optional modules are completely isolated from global `load-path`.

;;; Code:

;; ---------------------------------------------------------------------------
;; Identify the directory of this loader (the Optional folder)
;; ---------------------------------------------------------------------------
(defvar myextras-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory containing optional configuration files for context-lmtx-mode.")

;; ---------------------------------------------------------------------------
;; List of optional configuration files to load
;; NOTE:
;;  - Order matters: files will be loaded in the sequence listed here
;;  - Include only file names, not full paths
;; ---------------------------------------------------------------------------
(setq myextras-files
      '("appearance.el"
        "keybindings.el"
        "session-saving.el"
        "template-tools.el"
        "miscellaneous.el"))

;; ---------------------------------------------------------------------------
;; Load each optional file safely from the same directory
;; ---------------------------------------------------------------------------
(dolist (file myextras-files)
  (let ((fullpath (expand-file-name file myextras-dir)))
    (when (file-exists-p fullpath)  ; Only load if the file actually exists
      (load fullpath nil 'nomessage))))

;; ---------------------------------------------------------------------------
(provide 'myextras-loader)
;;; myextras-loader.el ends here

