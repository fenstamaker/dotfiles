;;; init-windows.el --- Load windows configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar windows/parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

;; (setq
;;  display-buffer-alist
;;  `(("\\*Buffer List\\*" display-buffer-in-side-window
;;     (side . top) (slot . 0) (window-height . fit-window-to-buffer)
;;     (preserve-size . (nil . t)) ,windows/parameters)

;;    ("\\*Tags List\\*" display-buffer-in-side-window
;;     (side . right) (slot . 0) (window-width . fit-window-to-buffer)
;;     (preserve-size . (t . nil)) ,windows/parameters)

;;    ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
;;     display-buffer-in-side-window
;;     (side . bottom) (slot . -1) (preserve-size . (nil . t))
;;     ,windows/parameters)

;;    ("\\*\\(?:shell\\|compilation\\)\\*" display-buffer-in-side-window
;;     (side . bottom) (slot . 1) (preserve-size . (nil . t))
;;     ,windows/parameters)))

(setq
 display-buffer-alist
 '(("\\*Warnings\\*" display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t))
    ,windows/parameters)

   ("\\*prettier errors\\*" display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t))
    ,windows/parameters)


   ("\\*lsp.*\\*" display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t))
    ,windows/parameters)


   ("\\*helm.*\\*" display-buffer-in-side-window
    (side . bottom) (slot . -1) (preserve-size . (nil . t))
    ,windows/parameters)
   )
 )



(provide 'init-windows)
;;; init-windows.el ends here
