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
 
 (setq-default scroll-bar-width 7)
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
        (let* ((session-dir (expand-file-name ".emacs-session/" project-dir))
               (desktop-file (expand-file-name "desktop" session-dir)))
          (if (file-exists-p desktop-file)
              (let ((desktop-dirname session-dir))
                (desktop-read session-dir)
                (message "✅ Session loaded from: %s" session-dir))
            (message "❌ No saved session found in: %s" session-dir)))
      (message "❌ You are not in a 'Main.ctx' file."))))

;; Keybindings
(global-set-key (kbd "C-c s") #'my/save-session)
(global-set-key (kbd "C-c l") #'my/load-session)


;; Keybindings
;;   (global-set-key (kbd "C-c s") 'my/save-project-session)
   (global-set-key (kbd "C-c 9") 'windmove-right)



