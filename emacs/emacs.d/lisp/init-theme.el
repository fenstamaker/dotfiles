;;; init-theme.el --- Load theme configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defvar theme/strong-highlight)
(defvar theme/light-highlight)
(defvar theme/highlight)

;; Light
(setq theme/strong-highlight "#fdcce2")
(setq theme/light-highlight "#e8e8e8")
(setq theme/highlight "#d8d8d8")

;; Dark
;; (setq theme/strong-highlight "#644570")
;; (setq theme/light-highlight "#3a3a3a")
;; (setq theme/highlight "#444444")

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-light t))

  ; (let ((strong-highlight "#fdcce2")
  ;       (light-highlight "#e8e8e8")
  ;       (highlight "#d8d8d8"))
  ;   (custom-theme-set-faces
  ;    'spacemacs-light
  ;    `(helm-ls-git-modified-not-staged-face ((t :foreground ,(face-attribute 'default :foreground) :bold nil)))
  ;  ))
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

  (setq highlight-indent-guides-auto-enabled nil)

  (set-face-background 'highlight-indent-guides-odd-face theme/light-highlight)
  (set-face-background 'highlight-indent-guides-even-face theme/highlight)


  (set-face-background 'highlight-indent-guides-top-odd-face theme/strong-highlight)
  (set-face-background 'highlight-indent-guides-top-even-face theme/strong-highlight)

  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
  )


(add-hook
 'emacs-startup-hook
 (lambda nil
   ;; Enable line-numbers
   (setq
    column-number-mode t
    global-display-line-numbers-mode t
    display-line-numbers "%4d \u2502 ")

   ;; Cursor Options
   (setq
    blink-cursor-mode -1
    cursor-type 'box)

   ;; Turn off word wrap
   (setq truncate-lines t)))

(provide 'init-theme)
;;; init-theme.el ends here
