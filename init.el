;;; init.el --- my emacs configuration               -*- lexical-binding: t; -*-

;; Author: CryptoManiac <cryptomaniac@cmub0>
;; Keywords: lisp

;;; Commentary:

;; This file contains my Emacs configuration

;;; Code:

(custom-set-variables
 '(user-mail-address "cryptomaniac.512@gmail.com")
 '(user-full-name "Nikita <CryptoManiac> Sivakov"))

(set-frame-font "Droid Sans Mono Slashed-9")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")); add mepla to packages list
(package-initialize)

(require 'use-package)

(setq custom-file "~/.emacs.d/custom-variables.el"); Custom code file
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/conf/"); my cinfiguration dir
(require 'functions); custom functions
(require 'plugins); plugins settings

(use-package nord-theme
    :config
  (load-theme 'nord t))

(setq inhibit-startup-message t ; hide splash screen and banner
      inhibit-startup-echo-area-message t)

(tool-bar-mode -1) ;; hide toolbar
(setq use-dialog-box nil) ;; no GUI dialog windows
(setq redisplay-dont-pause t) ;; better buffer 'rendering'

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t) ;; highlight pair brackets
(setq show-paren-style 'expression) ;; highlight text inside brackets

(electric-pair-mode t) ;; auto-close pair "brackets"

(setq large-file-warning-threshold 30000000); up to 30mb is ok

(setq lisp-indent-function  'common-lisp-indent-function)

(setq scroll-step 1); scrolling
(setq scroll-margin 5)
(scroll-bar-mode -1); disable scroll bar

(provide 'init)
;;; init.el ends here
