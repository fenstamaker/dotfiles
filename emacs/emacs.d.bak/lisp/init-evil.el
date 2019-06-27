;; init-evil.el --- Load custom evil config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq evil-toggle-key "C-z")
  :config
  ;;; Insert mode is now just emacs mode
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (setcdr evil-normal-state-map nil)
  (setq evil-default-state 'normal)

  (evil-mode 1)
  )

(provide 'init-evil)
;;; init-evil.el ends here
