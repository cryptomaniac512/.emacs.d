;;; plugins.el --- main plugins configuration        -*- lexical-binding: t; -*-

;; Author: CryptoManiac <cryptomaniac.512@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file contains my plugins configuration, requirements and hooks

;;; Code:

(use-package package-utils
    :ensure t)

(use-package nord-theme
    :ensure t
    :config
  (custom-set-variables
   '(nord-comment-brightness 10))
  (load-theme 'nord t))

(use-package evil
    :ensure t
    :config
  (evil-mode t)
  (define-key evil-normal-state-map "/" 'counsel-grep-or-swiper)
  (use-package evil-surround
      :ensure t
      :config
    (global-evil-surround-mode t))
  (use-package evil-leader
      :ensure t
      :config
    (global-evil-leader-mode t)
    (evil-leader/set-key "i" 'counsel-imenu)
    (evil-leader/set-key "sl" 'sort-lines)
    (evil-leader/set-key-for-mode 'python-mode
	"D" 'xref-find-definitions-other-window
	"d" 'xref-find-definitions
	"G" 'elpy-goto-assignment-other-window
	"g" 'elpy-goto-assignment
	"k" 'elpy-doc
	"n" 'elpy-occur-definitions
	"r" 'elpy-refactor)
    (evil-leader/set-key-for-mode 'elisp-mode
	"D" 'xref-find-definitions-other-window
	"d" 'xref-find-definitions
	"n" 'xref-find-references)))

(use-package linum
    :config
  (column-number-mode t)
  (use-package linum-relative
      :ensure t
      :config
    (linum-relative-global-mode t)
    (custom-set-variables
     '(linum-relative-current-symbol ""))))

(use-package elpy
    :ensure t
    :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
  (remove-hook 'elpy-modules 'elpy-module-pyvenv)
  (remove-hook 'elpy-modules 'elpy-module-django))

(use-package virtualenvwrapper
    :ensure t
    :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/Devel/Envs/")
  (custom-set-variables
   '(python-environment-directory "~/Devel/Envs/")))

(use-package py-isort
    :ensure t
    :config
  (defun cm-py-isort-buffer-or-region ()
    "Call py-isort for region or for buffer."
    (interactive)
    (if (region-active-p)
	(py-isort-region)
      (py-isort-buffer)))
  :bind
  ("M-p M-i" . cm-py-isort-buffer-or-region))

(use-package pytest
    :ensure t
    :config
  (add-to-list 'pytest-project-root-files "pytest.ini")
  (setq pytest-cmd-flags "-p no:sugar")
  :bind (("C-c t a" . pytest-all)
	 ("C-c t m" . pytest-module)
	 ("C-c t o" . pytest-one)
	 ("C-c t d" . pytest-directory)))

(use-package vue-mode
    :ensure t)

(use-package stylus-mode
    :ensure t)

(use-package emmet-mode
    :ensure t
    :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'vue-html-mode-hook 'emmet-mode))

(use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
	   ("\\.md\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
    :ensure t)

(use-package json-mode
    :ensure t)

(use-package toml-mode
    :ensure t)

(use-package sass-mode
    :ensure t)

(use-package company
    :ensure t
    :config
  (global-company-mode)
  (setq company-idle-delay 0)
  :bind (("C-x C-o" . company-complete)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("M-k" . company-show-doc-buffer)
	 ("C-d" . company-show-location)))

(use-package editorconfig
    :ensure t
    :config
  (editorconfig-mode t))

(use-package popwin
    :ensure t
    :config
  (popwin-mode t)
  (push '("*Buffer List*" :position bottom :height 20) popwin:special-display-config)
  (push '("*Completions*" :position bottom :height 24) popwin:special-display-config)
  (push '("*Flycheck errors*" :position bottom :height 24) popwin:special-display-config)
  (push '("*Help*" :position bottom :height 24) popwin:special-display-config)
  (push '("*compilation*" :position bottom :height 24) popwin:special-display-config))

(use-package flycheck
    :ensure t
    :config
  (global-flycheck-mode t))

(use-package yasnippet
    :ensure t
    :config
    (yas-global-mode t)
    (use-package yasnippet-snippets
	:ensure t))

(use-package counsel
    :ensure t
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
	:ensure t
	:config
      (setq ivy-re-builders-alist
	    '((t . ivy--regex-plus)))
      (setq ivy-initial-inputs-alist nil))
    (use-package ivy-rich
	:ensure t
	:config
      (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
      (setq ivy-rich-abbreviate-paths t)
      (setq ivy-rich-switch-buffer-name-max-length 50)
      (setq ivy-rich-switch-buffer-mode-max-length 20)
      (setq ivy-rich-switch-buffer-project-max-length 30))))

(use-package projectile
    :ensure t
    :config
  (projectile-mode t)
  (setq projectile-switch-project-action
	(lambda ()
	  (if (magit-git-repo-p (projectile-project-root))
	      (magit-status)
	    (dired-other-window (projectile-project-root)))))
  ;; workaround for https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
	'(:eval (format " Projectile[%s]"
		 (projectile-project-name))))
  (setq projectile-completion-system 'ivy)
  (use-package counsel-projectile
      :ensure t
      :after counsel
      :config
      (counsel-projectile-on)))

(use-package magit
    :ensure t
    :config
    (use-package evil-magit
	:ensure t)
  (use-package git-messenger
      :ensure t
      :config
    (custom-set-variables
     '(git-messenger:show-detail t)
     '(git-messenger:use-magit-popup t))
    :bind (("C-x p v" . git-messenger:popup-message)
	   :map git-messenger-map
	   ("m" . git-messenger:copy-message))))

(use-package git-link
    :ensure t)

(use-package flycheck-package
    :ensure t
    :config
    (use-package package-lint
	:ensure t)
    (flycheck-package-setup))

(provide 'plugins)
;;; plugins.el ends here
