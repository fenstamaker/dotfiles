;; init-lsp.el --- Load LSP config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (require 'lsp-clients)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package helm-lsp
  :ensure t)

(provide 'init-lsp)
;;; init-lsp.el ends here
