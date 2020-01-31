;;; init-flycheck.el --- Load flycheck configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-to-list 'auto-mode-alist '(".template\\'" . cfn-mode))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


  ;; (flycheck-define-checker cfn-lint
  ;;   "A Cloudformation linter using cfn-python-lint. See URL 'https://github.com/awslabs/cfn-python-lint'."
  ;;   :command ("cfn-lint" "-f" "parseable" source)
  ;;   :error-patterns (
  ;;                    (warning line-start (file-name) ":" line ":" column
  ;;                             ":" (one-or-more digit) ":" (one-or-more digit) ":"
  ;;                             (id "W" (one-or-more digit)) ":" (message) line-end)
  ;;                    (error line-start (file-name) ":" line ":" column
  ;;                           ":" (one-or-more digit) ":" (one-or-more digit) ":"
  ;;                           (id "E" (one-or-more digit)) ":" (message) line-end)
  ;;                    )
  ;;   :modes (cfn-mode))
  ;; (add-to-list 'flycheck-checkers 'cfn-lint)

  (flycheck-add-mode 'javascript-eslint 'web-mode)



  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
