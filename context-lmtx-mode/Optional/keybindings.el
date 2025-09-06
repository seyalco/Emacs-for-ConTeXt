;;; keybindings.el --- Custom keybindings -*- lexical-binding: t; -*-

;; جابه‌جایی بین پنجره‌ها
(global-set-key (kbd "C-c b") #'windmove-left)
(global-set-key (kbd "C-c f") #'windmove-right)
(global-set-key (kbd "C-c p") #'windmove-up)
(global-set-key (kbd "C-c n") #'windmove-down)

;; درج کوتیشن دوتایی
(global-set-key (kbd "C-\"") (lambda () (interactive) (insert "\"")))

(provide 'keybindings)
;;; keybindings.el ends here
