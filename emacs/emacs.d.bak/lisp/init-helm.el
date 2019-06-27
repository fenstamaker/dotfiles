;;; init-helm.el --- Load helm configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  )

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode t)

  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-<return>") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-b") #'helm-mini)
  (global-set-key (kbd "s-b") #'helm-mini)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-imenu-fuzzy-match t
        helm-find-files-ignore-thing-at-point t)

  (global-set-key (kbd "M-v") #'helm-show-kill-ring)

  ;; (setq helm-display-function #'helm-display-buffer-in-own-frame)
)

(use-package helm-ls-git
  :ensure t
  :config
  (require 'helm-ls-git)
  )

;; (use-package window-purpose
;;   :ensure t
;;   :config
;;   (purpose-mode)
;;   (setq purpose-user-mode-purposes
;;         '((prog-mode . code)
;;           (helm-mode . helm)
;;           ))
;;   (purpose-compile-user-configuration)
;;   )

  ;; (helm-autoresize-mode t)
  ;; (setq helm-autoresize-max-height 60)

  ;; ;; From https://gist.github.com/antifuchs/9238468
  ;; (setq helm-idle-delay 0.0             ; update fast sources immediately (doesn't).
  ;;       helm-input-idle-delay 0.01      ; this actually updates things
  ;;                                       ; reeeelatively quickly.
  ;;       helm-quick-update t
  ;;       helm-M-x-requires-pattern nil
  ;;       helm-ff-skip-boring-files t)

  ;; ;; __Keybindings__

  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; make TAB works in terminal
  ;; (define-key helm-map (kbd "<enter>") 'helm-execute-persistent-action) ; make TAB works in terminal
  ;; (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
  ;; (define-key helm-map (kbd "<C-return>") 'helm-select-action) ; or list actions using ctrl+enter
  ;; (define-key helm-map (kbd "s-s") 'helm-grep-default-recurse-command)

  ;; (setq helm-split-window-in-side-p t)

  ;; (global-unset-key (kbd "C-x c")) ; Remove the default helm key

  ;; (bind-key* (kbd "s-h") 'helm-command-prefix)

  ;; (bind-key* (kbd "M-x") 'helm-M-x)
  ;; (bind-key* (kbd "M-<return>") 'helm-M-x)

  ;; (bind-key* (kbd "M-v") 'helm-show-kill-ring)
  ;; (bind-key* (kbd "s-y") 'helm-show-kill-ring)

  ;; (bind-key* (kbd "C-x b") 'helm-mini)
  ;; (bind-key* (kbd "s-b")   'helm-mini)

  ;; (bind-key* (kbd "C-x C-f") 'helm-find-files)

  ;; (bind-key* (kbd "s-p") 'helm-projectile)



(provide 'init-helm)
;;; init-helm.el ends here
