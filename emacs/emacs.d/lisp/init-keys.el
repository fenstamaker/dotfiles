;;; init-keys.el --- Load keys configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(bind-key* (kbd "s-=") 'text-scale-increase)
(bind-key* (kbd "s--") 'text-scale-decrease)
(bind-key* (kbd "s-0") (lambda () (interactive) (text-scale-adjust 0)))

(provide 'init-keys)
;;; init-keys.el ends here
