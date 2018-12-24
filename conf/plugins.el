;;; plugins.el --- emacs plugins configuration


;;; Commentary:
;; This file contains my Emacs plugins configuration

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package evil
  :straight t
  :hook
  ((after-init . evil-mode)))

(use-package counsel
  :straight t
  :config
  (use-package ivy
    :straight t
    :hook
    ((after-init . ivy-mode))))

(use-package magit
  :straight t
  :config
  (use-package evil-magit
    :straight t))

(provide 'plugins)
;;; plugins.el ends here
