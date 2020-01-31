;;; init-python.el --- Load python configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
  )

(use-package jupyter
  :ensure t)

(provide 'init-python)
;;; init-python.el ends here
