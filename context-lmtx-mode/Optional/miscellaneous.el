;;; miscellaneous.el --- Miscellaneous small tweaks and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains small, miscellaneous Emacs configurations, extra editing
;; utilities, and minor enhancements. It can be loaded by `extras-loader.el`
;; as part of optional configuration.

;;; Code:

;; ------------------------------------------------------------
;; Backspace Behavior and Word Deletion Functions
;; ------------------------------------------------------------

(defun ryanmarcus/backward-kill-word ()
  "Delete whitespace or a single word backward.
If the point is preceded by whitespace or newlines, delete them continuously;
otherwise, delete a single word backward using `backward-kill-word`."
  (interactive)
  (if (looking-back "[ \n]" 1)
      (progn
        (delete-horizontal-space t)
        (while (looking-back "[ \n]" 1)
          (backward-delete-char 1)))
    (backward-kill-word 1)))

(defun delete-backward-word (arg)
  "Delete ARG words backward without saving them to the kill ring."
  (interactive "p")
  (delete-region (point)
                 (progn (backward-word arg) (point))))

(defun delete-forward-word (arg)
  "Delete ARG words forward without saving them to the kill ring."
  (interactive "p")
  (delete-region (point)
                 (progn (forward-word arg) (point))))

;; Keybindings for deletion functions
(global-set-key (kbd "C-<backspace>") #'delete-backward-word)
(global-set-key (kbd "C-<delete>")    #'delete-forward-word)

;; ------------------------------------------------------------
;; Editing Enhancements
;; ------------------------------------------------------------

;; Automatically insert matching brackets and quotes
(electric-pair-mode 1)
(setq electric-pair-preserve-balance t)

;; ------------------------------------------------------------
;; Miscellaneous Behavior
;; ------------------------------------------------------------

;; Do not ask to save buffers before running `compile`
(setq compilation-ask-about-save nil)

(provide 'miscellaneous)

;;; miscellaneous.el ends here
