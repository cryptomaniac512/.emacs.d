;;; init.el --- my emacs configuration               -*- lexical-binding: t; -*-

;; Author: CryptoManiac <cryptomaniac@cmub0>
;; Keywords: lisp

;;; Commentary:

;; This file contains my Emacs configuration

;;; Code:

(set-frame-font "Droid Sans Mono Slashed-9")

(setq inhibit-startup-message t ; hide splash screen and banner
      inhibit-startup-echo-area-message t)

(tool-bar-mode -1) ;; hide toolbar
(setq use-dialog-box nil) ;; no GUI dialog windows

(setq scroll-step 1); scrolling
(setq scroll-margin 5)
(scroll-bar-mode -1); disable scroll bar

(defalias 'yes-or-no-p 'y-or-n-p) ;; Short messages


(setq custom-file "~/.emacs.d/custom-variables.el"); Custom code file
(load custom-file)
(custom-set-variables
 '(user-mail-address "cryptomaniac.512@gmail.com")
 '(user-full-name "Nikita <CryptoManiac> Sivakov")
 '(show-paren-style 'expression)
 '(compilation-scroll-output t))


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")); add mepla to packages list
(package-initialize)

(require 'use-package)


(setq large-file-warning-threshold 30000000); up to 30mb is ok


(show-paren-mode t) ;; highlight pair brackets
(electric-pair-mode t) ;; auto-close pair "brackets"

(setq lisp-indent-function  'common-lisp-indent-function)

(add-hook 'prog-mode-hook 'outline-minor-mode)


(add-to-list 'load-path "~/.emacs.d/conf/"); my cinfiguration dir
(require 'functions); custom functions
(require 'plugins); plugins settings

(provide 'init)
;;; init.el ends here
