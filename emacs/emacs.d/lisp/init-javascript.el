;;; init-javascript.el --- Load javascript configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.avsc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[m]?js[x]?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))


  (set-face-foreground 'web-mode-current-column-highlight-face nil)

  (setq web-mode-content-types-alist
        '(("json"    . "\\.json\\'")
          ("json"    . "\\.eslintrc\\'")
          ("json"    . "\\.avsc\\'")
          ("jsx"     . "\\.js[x]?\\'")
          ("json"     . "\\.babelrc\\'")))

  (add-to-list 'interpreter-mode-alist
               '("node" . web-mode))


  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)

  (setq indent-tabs-mode nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-quoting t)

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  )

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\.ts[x]?\'" . typescript-mode))

  )

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)

  (setq prettier-js-args
        '("--bracket-spacing" "true"))
  )

(provide 'init-javascript)
;;; init-javascript.el ends here
