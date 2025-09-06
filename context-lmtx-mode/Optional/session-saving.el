;;; session-saving.el --- Project-based session saving -*- lexical-binding: t; -*-

(require 'desktop)

(defun my/in-directory-with-readme-p ()
  "Check if current buffer's file is in a directory containing README.md."
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file)))
         (readme (and dir (expand-file-name "README.md" dir))))
    (and readme (file-exists-p readme))))

(defun my/project-dir-from-readme ()
  "Return current directory if it contains README.md."
  (let ((file (buffer-file-name)))
    (when (and file (my/in-directory-with-readme-p))
      (file-name-directory file))))

(defun my/save-session ()
  (interactive)
  (let ((project-dir (my/project-dir-from-readme)))
    (if project-dir
        (let ((session-dir (expand-file-name ".emacs-session/" project-dir)))
          (make-directory session-dir :parents)
          (let ((desktop-dirname session-dir))
            (desktop-save session-dir t))
          (message "✅ Session saved in: %s" session-dir))
      (message "❌ No README.md found in current directory."))))

(defun my/load-session ()
  (interactive)
  (let ((project-dir (my/project-dir-from-readme)))
    (if project-dir
        (let ((session-dir (expand-file-name ".emacs-session/" project-dir)))
          (if (file-directory-p session-dir)
              (progn
                (desktop-change-dir session-dir)
                (message "✅ Session loaded from: %s" session-dir))
            (message "❌ No .emacs-session/ directory found.")))
      (message "❌ No README.md found in current directory."))))

(global-set-key (kbd "C-c S") #'my/save-session)
(global-set-key (kbd "C-c L") #'my/load-session)

(provide 'session-saving)
;;; session-saving.el ends here
