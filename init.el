;;; init.el --- my emacs configuration


;;; Commentary:
;; This file contains my Emacs configuration

;;; Code:

(set-frame-font "Roboto Mono-9")

(setq
 inhibit-startup-message t
 inhibit-splash-screen t
 use-dialog-box nil
 ring-bell-function 'ignore
 undo-tree-auto-save-history t
 undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(custom-set-variables
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(electric-pair-mode t))

(setq-default
 display-line-numbers 'visual
 undo-tree-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d/conf"); my cinfiguration dir
(require 'plugins); plugins settings

(provide 'init)
;;; init.el ends here
