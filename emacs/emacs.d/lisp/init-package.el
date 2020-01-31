;;; init-package.el --- Load package configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;;; Sets up package repositories using SSL when possible
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https"))
       (melpa-url (concat proto "://melpa.org/packages/"))
       (elpa-url (concat proto "://elpa.gnu.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" melpa-url) t)
  (if (< emacs-major-version 24)
      ;; For improtant compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . elpa-url))
    (unless no-ssl
      ;; Forces HTTPS for elpa
      (setcdr (assoc "gnu" package-archives) elpa-url))))


;;; Packages are usually loaded automatically after init but we want
;;; to configure and use the packages during init. So we turn off
;;; automatic package initialization and do it ourselves
(setq package-enable-at-startup nil)
;; (package-initialize)

(unless package-archive-contents
  (package-refresh-contents))


;;; We will use use-package to manage and install packages. Newer
;;; Emacs versions have use-package already installed; this will
;;; install it if not present for older Emacs.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'init-package)
;;; init-package.el ends here
