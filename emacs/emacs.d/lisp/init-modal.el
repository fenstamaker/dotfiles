;;; init-modal.el --- Load modal editing configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package boon
  :ensure t
  :config
  (require 'boon-qwerty)
  (boon-mode)
  )

(provide 'init-modal)
;;; init-modal.el ends here
