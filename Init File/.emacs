(use-package auctex
  :disabled)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" default))
 '(package-selected-packages '(dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
;;keep the scroll bar thin and not show any extra visual clutter,
(setq-default scroll-bar-width 7)
;;


 (setq-default tab-width 4)
 (add-to-list 'auto-mode-alist '("\\.ctx\\'" . plain-tex-mode))
 (setq compilation-ask-about-save nil)
 (global-set-key (kbd "C-c b") 'windmove-left)
 (global-set-key (kbd "C-c f") 'windmove-right)
 (global-set-key (kbd "C-c p") 'windmove-up)
 (global-set-key (kbd "C-c n") 'windmove-down)
 (global-set-key (kbd "C-\"") (lambda () (interactive) (insert "\"")))
   
;; backspace behavior
(defun ryanmarcus/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))
 
(defun compile-context-file ()
  (interactive)
  (compile (concat "context '" (file-name-nondirectory (buffer-file-name)) "'"))
)

(global-set-key (kbd "C-c c") 'compile-context-file)

(defun view-context-file ()
  (interactive)
  (async-shell-command (format "evince %s.pdf" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
)

(global-set-key (kbd "C-c v") 'view-context-file)


;; Enable automatic pairing of brackets/quotes
(electric-pair-mode 1)

;; Optional: prevent deleting/matching that would unbalance
(setq electric-pair-preserve-balance t)

(defun delete-backward-word (arg)
  "Delete words backward without saving to kill ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-<backspace>") 'delete-backward-word)  

(defun delete-forward-word (arg)
  "Delete words forward without saving to the kill ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(global-set-key (kbd "C-<delete>") 'delete-forward-word)


;;Begin -- session saving
 (require 'desktop)

(defun my/in-main-ctx-p ()
  "Check if the current buffer is visiting a file named 'Main.ctx'."
  (let ((file (buffer-file-name)))
    (and file (string-equal (file-name-nondirectory file) "Main.ctx"))))

(defun my/project-dir-from-main-ctx ()
  "Return the directory of 'Main.ctx' if we are in it, else nil."
  (when (my/in-main-ctx-p)
    (file-name-directory (buffer-file-name))))

(defun my/save-session ()
  "Save Emacs session to .emacs-session/ in the same folder as Main.ctx."
  (interactive)
  (let ((project-dir (my/project-dir-from-main-ctx)))
    (if project-dir
        (let ((session-dir (expand-file-name ".emacs-session/" project-dir)))
          (make-directory session-dir :parents)
          (let ((desktop-dirname session-dir))
            (desktop-save session-dir t))
          (message "✅ Session saved in: %s" session-dir))
      (message "❌ You are not in a 'Main.ctx' file."))))

(defun my/load-session ()
  "Load Emacs session from .emacs-session/ in the same folder as Main.ctx."
  (interactive)
  (let ((project-dir (my/project-dir-from-main-ctx)))
    (if project-dir
        (let ((session-dir (expand-file-name ".emacs-session/" project-dir)))
          (if (file-directory-p session-dir)
              (progn
                (desktop-change-dir session-dir)
                (message "✅ Session loaded from: %s" session-dir))
            (message "❌ No .emacs-session/ directory found in: %s" project-dir)))
      (message "❌ You are not in a 'Main.ctx' file."))))


;; Keybindings
(global-set-key (kbd "C-c S") #'my/save-session)
(global-set-key (kbd "C-c L") #'my/load-session)
;;End -- session saving


;;Begin -- ConTeXt auto start,stop inserting
(defun my/expand-start-environment ()
  "Expand \\startENV to \\startENV\n\n\\stopENV and place point between."
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

;; Bind it to TAB only when appropriate (optional – see below for smart handling)
(global-set-key (kbd "C-c t") 'my/expand-start-environment)

(defun my/context-tab-handler ()
  "Smart TAB for expanding \\startXXX environments."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (string-match "^\\\\start\\([A-Za-z]+\\)\\s-*$" line)
        (my/expand-start-environment)
      (indent-for-tab-command))))

;;This ensures the TAB override is only active in plain text modes (including markdown, etc.).
(add-to-list 'auto-mode-alist '("\\.\\(ctx\\|txt\\|tex\\)\\'" . text-mode))
(add-hook 'text-mode-hook
          (lambda ()
            (local-set-key (kbd "TAB") 'my/context-tab-handler)))
;;End -- ConTeXt auto start,stop inserting

;;Begin -- Template folder
(defvar my/template-folder-path nil
  "Path to the user-defined Template folder.")

(defun my/set-template-folder-path ()
  "Prompt the user to select a Template folder, and save it to `my/template-folder-path`."
  (interactive)
  (let ((dir (read-directory-name "Enter parent path of main 'Template' folder: ")))
    (if (file-directory-p (expand-file-name "Template" dir))
        (progn
          (setq my/template-folder-path (expand-file-name "Template" dir))
          (message "✅ Template folder path set to: %s" my/template-folder-path))
      (message "❌ No 'Template' folder found in: %s" dir))))

(defun my/copy-template-to-current-dir ()
  "Copy the Template directory to the current directory.
Prompts for a path if Template folder is not yet set or missing."
  (interactive)
  (let* ((target-dir (file-name-directory (or buffer-file-name default-directory)))
         (target-path (expand-file-name "Template" target-dir)))

    ;; Step 1: If no template path or missing folder, prompt to set
    (if (or (not my/template-folder-path)
            (not (file-directory-p my/template-folder-path)))
        (when (yes-or-no-p (format "❌ No 'Template' folder found in: %s\nWould you like to set a new path? "
                                   (or my/template-folder-path "N/A")))
          (my/set-template-folder-path)))

    ;; Step 2: Copy template if everything is valid
    (cond
     ((not (file-directory-p my/template-folder-path))
      (message "❌ Still no valid Template folder path set."))

     ((file-directory-p target-path)
      (message "⚠️ A 'Template' folder already exists in this directory: %s" target-dir))

     (t
      (copy-directory my/template-folder-path target-path)
      (message "✅ Template folder copied to: %s" target-dir)))))

;; Bind to C-c N
(global-set-key (kbd "C-c N") #'my/copy-template-to-current-dir)


;; Save variable value across sessions
(setq my/template-folder-path nil)
(setq-default my/template-folder-path nil)

;; Save custom variables to a file
(setq custom-file "~/.emacs-custom.el")
(load custom-file t)

;; Save path
(defun my/save-template-path ()
  (customize-save-variable 'my/template-folder-path my/template-folder-path))

(add-hook 'kill-emacs-hook #'my/save-template-path)
;;End -- Template folder


;; Keybindings
(global-set-key (kbd "C-c 9") 'windmove-right) ;;use it for ensuring the file is correctly readed until the end

