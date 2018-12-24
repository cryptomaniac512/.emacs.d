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

(use-package nord-theme
  :straight t
  :hook
  ((after-init . (lambda ()
		   (load-theme 'nord t))))
  :config
  (setq-default nord-comment-brightness 10))

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

(use-package projectile
  :straight t
  :hook
  ((after-init . projectile-mode))
  :init
  (setq projectile-completion-system 'ivy)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :config
  (defun sn-projectile-magit-or-dired ()
    "Open magit or dired in project."
    (if (magit-git-repo-p (projectile-project-root))
	(magit-status)
      (dired-other-window (projectile-project-root))))
  (setq projectile-switch-project-action #'sn-projectile-magit-or-dired))

(use-package magit
  :straight t
  :config
  (use-package evil-magit
    :straight t))

(provide 'plugins)
;;; plugins.el ends here
