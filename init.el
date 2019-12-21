
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; The following sequence of commands is required to bootstrap org-mode
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(setq package-load-list '(all))

(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package "diminish"
	:ensure t
	:init
	(require 'diminish)
	)
(require 'bind-key)
(require 'org)
;; (use-package org :ensure t :pin org )
;; Org is now bootstrapped

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Set calendar date-style
(calendar-set-date-style (quote iso))

;; Force babel refresh of main config files
(delete-file "~/.emacs.d/personal.el" nil)
(delete-file "~/.emacs.d/configuration.el" nil)

;; Main config
(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-tag ((t (:background "gray6" :foreground "white smoke" :slant oblique :weight bold :height 0.8)))))
