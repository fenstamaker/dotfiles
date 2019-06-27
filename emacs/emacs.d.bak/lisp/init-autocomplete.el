;;; init-autocomplete.el --- Load autocomplete configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook #'yas-minor-mode)
;;   )

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :config
;;   (add-hook 'web-mode-hook #'(lambda () (set (make-local-variable 'yas-extra-modes) 'js-mode)))
;;   )


(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-global-modes '(not org-mode))
  (setq company-global-modes '(not csv-mode))

  ;; Disable Lowercase in certain modes
  (setq company-dabbrev-downcase nil)
  (add-to-list 'company-dabbrev-code-modes 'web-mode)
  (add-to-list 'company-dabbrev-code-modes 'clojure-mode)
  (add-to-list 'company-dabbrev-code-modes 'yaml-mode)
  (add-to-list 'company-dabbrev-code-modes 'json-mode)

  (setq company-idle-delay 0.125)
  (setq company-minimum-prefix-length 1)
  )

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook #'lsp)
  (setq lsp-enable-snippet nil)
  )

(use-package company-lsp
  :ensure t
  :requires (company lsp-mode)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  )

(use-package helm-lsp
  :ensure t
  :requires (helm lsp-mode))


(use-package lsp-treemacs
  :ensure t
  :requires (lsp-mode)
  :commands lsp-treemacs-errors-list)

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
