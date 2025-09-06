;;; template-tools.el --- Template manager with numbered quick-select -*- lexical-binding: t; -*-

(defvar myextras-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing template-tools.el and related optional modules.")

(defvar my/template-storage-file
  (expand-file-name "Template-folder.el" myextras-dir)
  "File storing the list of template paths for this project.")

(defvar my/template-list '()
  "List of user-defined template folder/file paths.")

(defun my/load-templates ()
  "Load `my/template-list` from storage file."
  (when (file-exists-p my/template-storage-file)
    (load-file my/template-storage-file)))

(defun my/save-templates ()
  "Save `my/template-list` into storage file."
  (with-temp-file my/template-storage-file
    (insert ";; Auto-generated list of user templates.\n")
    (insert ";; Do not edit manually unless you know what you're doing.\n\n")
    (insert (format "(setq my/template-list '%S)\n" my/template-list))))

(my/load-templates)

;; ------------------------------
;; افزودن قالب
;; ------------------------------
(defun my/add-template ()
  "Prompt to add a folder or file as a template."
  (interactive)
  (let* ((type (completing-read "Add a (f)older or a (F)ile? " '("folder" "file") nil t))
         (path (if (string= type "folder")
                   (read-directory-name "Select template folder: " nil nil t)
                 (read-file-name "Select template file: " nil nil t))))
    (if (file-exists-p path)
        (progn
          (unless (member path my/template-list)
            (push path my/template-list)
            (setq my/template-list (sort my/template-list #'string<))
            (my/save-templates)
            (message "✅ Template added: %s" path)))
      (message "❌ File or folder does not exist: %s" path))))

;; ------------------------------
;; حذف قالب با شماره و نمایش پایدار
;; ------------------------------
(defun my/remove-template ()
  "Remove a template by selecting its number from the list."
  (interactive)
  (if my/template-list
      (let* ((indexed-list
              (cl-mapcar (lambda (n item)
                           (format "%d) %s" n item))
                         (number-sequence 1 (length my/template-list))
                         my/template-list))
             (preview (mapconcat #'identity indexed-list "\n")))
        ;; نمایش لیست و مکث برای خواندن
        (message "📂 Templates List:\n%s" preview)
        (sit-for 3) ;; مکث ۳ ثانیه‌ای
        ;; گرفتن شماره
        (let* ((num (read-number "Enter template number to remove: "))
               (index (1- num)))
          (if (and (>= index 0) (< index (length my/template-list)))
              (let ((choice (nth index my/template-list)))
                (setq my/template-list (delete choice my/template-list))
                (my/save-templates)
                (message "🗑 Removed: %s" choice))
            (message "❌ Invalid number."))))
    (message "⚠ No templates available to remove.")))

;; ------------------------------
;; انتخاب و کپی قالب با شماره و مکث
;; ------------------------------
(defun my/copy-template-here ()
  "Copy a selected template (by number) into current directory."
  (interactive)
  (if my/template-list
      (let* ((indexed-list
              (cl-mapcar (lambda (n item)
                           (format "%d) %s" n item))
                         (number-sequence 1 (length my/template-list))
                         my/template-list))
             (preview (mapconcat #'identity indexed-list "\n")))
        ;; نمایش لیست و مکث
        (message "📂 Templates List:\n%s" preview)
        (sit-for 3) ;; مکث سه ثانیه‌ای
        ;; گرفتن شماره
        (let* ((num (read-number "Enter template number to copy: "))
               (index (1- num)))
          (if (and (>= index 0) (< index (length my/template-list)))
              (let* ((choice (nth index my/template-list))
                     (target-dir (file-name-directory (or buffer-file-name default-directory)))
                     (dest-path (expand-file-name (file-name-nondirectory choice) target-dir)))
                (if (file-exists-p dest-path)
                    (message "⚠ Destination already exists: %s" dest-path)
                  (if (file-directory-p choice)
                      (copy-directory choice dest-path t t t)
                    (copy-file choice dest-path))
                  (message "✅ Template copied: %s → %s" choice dest-path)))
            (message "❌ Invalid number."))))
    (message "⚠ Template list is empty. Use C-c N to add one.")))

;; ------------------------------
;; مدیریت حالت add/remove
;; ------------------------------
(defun my/manage-templates ()
  "Ask whether to add or remove a template."
  (interactive)
  (let ((action (completing-read "Do you want to (a)dd or (r)emove a template? "
                                 '("add" "remove") nil t)))
    (cond
     ((string= action "add") (my/add-template))
     ((string= action "remove") (my/remove-template)))))

;; ------------------------------
;; Keybindings
;; ------------------------------
(global-set-key (kbd "C-c N") #'my/manage-templates)
(global-set-key (kbd "C-c M") #'my/copy-template-here)

(provide 'template-tools)
;;; template-tools.el ends here

