;;; init.el --- my emacs configuration               -*- lexical-binding: t; -*-

;; Author: CryptoManiac <cryptomaniac@cmub0>
;; Keywords: lisp

;;; Commentary:

;; This file contains my Emacs configuration

;;; Code:

(set-frame-font "Roboto Mono-9")

(setq inhibit-startup-message t ; hide splash screen and banner
      inhibit-startup-echo-area-message t)

(tool-bar-mode -1) ;; hide toolbar
(menu-bar-mode -1)
(global-set-key (kbd "C-M-m") 'menu-bar-mode)
(setq use-dialog-box nil) ;; no GUI dialog windows

(setq scroll-step 1); scrolling
(setq scroll-margin 5)
(scroll-bar-mode -1); disable scroll bar

(defalias 'yes-or-no-p 'y-or-n-p) ;; Short messages


(setq custom-file "~/.emacs.d/custom-variables.el"); Custom code file
(load custom-file)
(custom-set-variables
 '(user-mail-address "sivakov512@gmail.com")
 '(user-full-name "Nikita Sivakov")
 '(show-paren-style 'expression)
 '(compilation-scroll-output t)
 '(display-line-numbers-type 'visual)
 '(backup-by-copying t))


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")); add mepla to packages list
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(require 'use-package)


(setq large-file-warning-threshold 30000000); up to 30mb is ok


(show-paren-mode t) ;; highlight pair brackets
(electric-pair-mode t) ;; auto-close pair "brackets"
(global-display-line-numbers-mode t)
(column-number-mode t)

(setq lisp-indent-function  'common-lisp-indent-function)

(add-hook 'prog-mode-hook 'outline-minor-mode)


(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(add-to-list 'exec-path (expand-file-name "~/.npm-packages/bin"))


(add-to-list 'load-path "~/.emacs.d/conf/"); my cinfiguration dir
(require 'functions); custom functions
(require 'plugins); plugins settings

(provide 'init)
;;; init.el ends here
