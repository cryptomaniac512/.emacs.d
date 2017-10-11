;;; plugins.el --- main plugins configuration        -*- lexical-binding: t; -*-
;; Author: CryptoManiac <cryptomaniac.512@gmail.com>
;; Keywords: lisp
;;; Commentary:
;; This file contains my plugins configuration, requirements and hooks
;;; Code:

(use-package package-utils)

(use-package nord-theme
    :config
  (custom-set-variables
   '(nord-comment-brightness 5))
  (load-theme 'nord t))

(use-package evil
    :config
  (evil-mode t)
  (define-key evil-normal-state-map "/" 'counsel-grep-or-swiper)
  (use-package evil-surround
      :config
    (global-evil-surround-mode t))
  (use-package evil-leader
      :config
    (global-evil-leader-mode t)
    (evil-leader/set-key "i" 'counsel-imenu)
    (evil-leader/set-key "sl" 'sort-lines)
    (evil-leader/set-key-for-mode 'python-mode
	"d" 'anaconda-mode-find-definitions
	"g" 'anaconda-mode-find-assignments
	"k" 'anaconda-mode-show-doc
	"n" 'anaconda-mode-find-references)))

(use-package linum
    :config
  (column-number-mode t)
  (use-package linum-relative
      :config
    (linum-relative-global-mode t)
    (custom-set-variables
     '(linum-relative-current-symbol ""))))

(use-package anaconda-mode
    :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (remove-hook 'anaconda-mode-response-read-fail-hook
	       'anaconda-mode-show-unreadable-response)
  (use-package virtualenvwrapper
      :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location "~/Devel/Envs/")
    (custom-set-variables
     '(python-environment-directory "~/Devel/Envs/")))
  (use-package py-isort
      :config
    (defun cm-py-isort-buffer-or-region ()
      "Call py-isort for region or for buffer."
      (interactive)
      (if (region-active-p)
	  (py-isort-region)
	(py-isort-buffer)))
    :bind
    ("M-p M-i" . cm-py-isort-buffer-or-region)))

(use-package pytest
    :config
  (add-to-list 'pytest-project-root-files "pytest.ini")
  (setq pytest-cmd-flags "-p no:sugar")
  :bind (("C-c C-t C-a" . pytest-all)
	 ("C-c C-t C-m" . pytest-module)
	 ("C-c C-t C-o" . pytest-one)
	 ("C-c C-t C-d" . pytest-directory)
	 ("C-c C-t C-p C-a" . pytest-pdb-all)
	 ("C-c C-t C-p C-m" . pytest-pdb-module)
	 ("C-c C-t C-p C-o" . pytest-pdb-one)))

(use-package vue-mode)

(use-package stylus-mode)

(use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode)

(use-package company
    :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (add-to-list 'company-backends 'company-anaconda)
  (use-package company-quickhelp
      :config
    (company-quickhelp-mode t)
    (setq company-quickhelp-use-propertized-text t)
    (setq company-quickhelp-delay nil)
    :bind
    ("C-k" . company-quickhelp-manual-begin))
  :bind (("C-x C-o" . company-complete)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("M-k" . company-show-doc-buffer)
	 ("C-d" . company-show-location)))

(use-package editorconfig
    :config
  (editorconfig-mode t))

(use-package popwin
    :config
  (popwin-mode t)
  (push '("*Anaconda*" :position bottom :height 20) popwin:special-display-config)
  (push '("*Buffer List*" :position bottom :height 20) popwin:special-display-config)
  (push '("*Flycheck errors*" :position bottom :height 24) popwin:special-display-config)
  (push '("*compilation*" :position bottom :height 24) popwin:special-display-config))

(use-package flycheck
    :config
  (global-flycheck-mode t))

(use-package yasnippet
    :config
  (yas-global-mode t))

(use-package counsel
    :config
  (use-package ivy
      :config
    (ivy-mode t)
    (custom-set-variables
     '(ivy-height 15)
     '(ivy-use-virtual-buffers nil)
     '(ivy-use-selectable-prompt t)
     '(enable-recursive-minibuffers))
    (use-package flx
	:config
      (setq ivy-re-builders-alist
	    '((t . ivy--regex-plus)))
      (setq ivy-initial-inputs-alist nil))
    (use-package ivy-rich
	:config
      (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
      (setq ivy-rich-abbreviate-paths t)))
  (use-package projectile
      :config
    (projectile-mode t)
    (setq projectile-switch-project-action
	  (lambda ()
	    (if (magit-git-repo-p (projectile-project-root))
		(magit-status)
	      (dired-other-window (projectile-project-root)))))
    (setq projectile-completion-system 'ivy)
    (use-package counsel-projectile
	:config
      (counsel-projectile-on))))

(use-package magit
    :config
  (use-package evil-magit)
  (use-package git-messenger
      :config
    (custom-set-variables
     '(git-messenger:show-detail t)
     '(git-messenger:use-magit-popup t))
    :bind (("C-x p v" . git-messenger:popup-message)
	   :map git-messenger-map
	   ("m" . git-messenger:copy-message))))

(provide 'plugins)
;;; plugins.el ends here
