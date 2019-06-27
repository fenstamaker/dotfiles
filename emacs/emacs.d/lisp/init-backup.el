;;; init-backuptheme.el --- Load backup configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t))
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 create-lockfiles nil
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t) ; use versioned backups

(provide 'init-backup)
;;; init-backup.el ends here
