;;; Theming

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-themes.el
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))


(defun apply-themes ()
  (set-face-background 'highlight-indentation-face "#0D3842")
  (set-face-background 'highlight-indentation-current-column-face "#27525C")
  (set-face-background 'web-mode-current-element-highlight-face "#53b3eb")
  (set-face-foreground 'web-mode-current-element-highlight-face "#ffffff")

  (set-face-background 'web-mode-current-column-highlight-face "#fafafa")

  (add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))

  (set-face-attribute 'region nil
                      :foreground nil
                      :background "#fff")

  (set-face-attribute 'show-paren-match nil
                      :foreground nil
                      :background "#024a5b")
  (reapply-themes)
  )


(add-hook 'after-init-hook 'apply-themes)

;; (set-face-attribute 'helm-selection nil
;;                     :foreground "#d30a65"
;;                     :background "#fff")

;; (set-face-attribute 'show-paren-match-face nil
;;         :weight 'bold :underline nil :overline nil :slant 'normal)

;; (set-face-foreground 'show-paren-mismatch-face "#FF0000")
;; (set-face-attribute 'show-paren-mismatch-face nil
;;                     :weight 'bold :underline t :overline nil :slant 'normal)

(provide 'init-themes)
