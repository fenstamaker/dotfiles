;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Lifted liberally from https://github.com/purcell/emacs.d/
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
(setq debug-on-error nil)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(require 'init-package)
(require 'init-theme)
(require 'init-path)
(require 'init-backup)

(require 'init-helm)
(require 'init-flycheck)
(require 'init-autocomplete)

(require 'init-groovy)
(require 'init-javascript)
(require 'init-windows)
(require 'init-keys)

(require 'old-init)

;;; Navigation

(bind-key* (kbd "C-@") 'er/expand-region)
(bind-key* (kbd "s-[") 'windmove-left)
(bind-key* (kbd "s-]") 'windmove-right)
(bind-key* (kbd "s-<up>")      'backward-paragraph)
(bind-key* (kbd "s-<down>")    'forward-paragraph)
(bind-key* (kbd "s-<right>")   'end-of-line)
(bind-key* (kbd "s-<left>")   'beginning-of-line)
(bind-key* (kbd "M-<up>") 'sp-backward-sexp)
(bind-key* (kbd "M-<down>")  'sp-forward-sexp)
(bind-key* (kbd "M-<right>")    'right-word)
(bind-key* (kbd "M-<left>")  'left-word)
(bind-key* (kbd "C-,") 'beginning-of-buffer)
(bind-key* (kbd "C-.") 'end-of-buffer)
(bind-key* (kbd "C-,") 'beginning-of-buffer)
(bind-key* (kbd "C-.") 'end-of-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-prefer-flymake nil)
 '(lsp-ui-doc-border "gray50")
 '(lsp-ui-doc-delay 0.5)
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 50)
 '(lsp-ui-doc-position (quote bottom))
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-flycheck-live-reporting t)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-ui-sideline-show-hover nil)
 '(lsp-ui-sideline-show-symbol t)
 '(lsp-ui-sideline-update-mode (quote line))
 '(package-selected-packages
   (quote
    (flycheck window-purpose company-box lsp-ui yaml-mode web-mode use-package typescript-mode spacemacs-theme smartparens projectile prettier-js multiple-cursors lsp-treemacs inf-clojure highlight-indent-guides helm-swoop helm-lsp helm-ls-git groovy-mode git-gutter exec-path-from-shell diminish csv-mode company-lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background "light green"))))
 '(lsp-ui-sideline-code-action ((t (:background "light green" :foreground "gray30"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "MediumPurple1" :box (:line-width -1 :color "white") :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-global ((t (:background "thistle1"))))
 '(lsp-ui-sideline-symbol ((t (:foreground "gray30" :box (:line-width -1 :color "grey") :height 0.99)))))
