;;; init.el --- -*- no-byte-compile: t -*-
;; Remove any default org-mode from load-path
(mapcar
 #'(lambda (arg) (delete arg load-path))
 (remove nil
				 (mapcar
					#'(lambda (arg)
							(if (string-match-p "org$" arg) arg nil))
					load-path
					)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; The following sequence of commands is required to bootstrap org-mode
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(setq package-load-list '(all))

;; Install and load org-plus-contrib
(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))
(require 'org)

;; Org is now bootstrapped

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

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Set calendar date-style
(calendar-set-date-style (quote iso))

;; Auto-compile
(use-package auto-compile
	:config (auto-compile-on-load-mode)
	:ensure t)
(setq load-prefer-newer t)

;; Customization group
(defgroup rjh nil
	"Customizations by rjh"
	:tag "RJH Customization")

;; Set rjh config repo location
(setq rjh/local-config-repo "~/.emacs.d/rjh/")

;; Set environment variable to parse for configuration files
(setq rjh/config-env "EMACS_CONFIG")
(setq rjh/config-private-env "EMACS_CONFIG_PRIVATE")

;; Location of private configuration files
(setq rjh/local-private-dir "~/.emacs.d/private")

;; Load config methods
(setq rjh/local-init-dir
			(expand-file-name
			 "init"
			 rjh/local-config-repo ))

(defun rjh/load (dir)
  "Generates rjh/load functions
     dir - load directory"
	(lambda (file)
		(let ((orgfile (expand-file-name (concat orgfile ".org") dir)))
			(if (file-readable-p orgfile)
					(progn
						(org-babel-load-file orgfile t)
						)
				(progn
					(display-warning
					 'rjh
					 (format-message "%s does not exist!" orgfile)
					 :warning
					 ))
				))
		))

;; Functions to load config
(defun rjh/load-init (orgfile)
	"Use org-babel-load-file to load init/orgfile in rjh/local-config-repo"
	(let ((dir rjh/local-init-dir))
		(message "Loading init/%s ..." orgfile)
		(funcall (rjh/load dir) orgfile)))

(defun rjh/load-private (orgfile)
	"Use org-babel-load-file to load private/orgfile"
	(let ((dir rjh/local-private-dir))
		(message "Loading private/%s ..." orgfile)
		(funcall (rjh/load dir) orgfile)))

(defun rjh/load-env ()
	"Loads configuration from environment variable, rjh/config-env"
	(let ((configlist (delete "" (split-string (or (getenv rjh/config-env) ""))))
				(privatelist (delete "" (split-string (or (getenv rjh/config-private-env) ""))))
				)
		;; Load init config
		(dolist (orgfile configlist)
			(rjh/load-init orgfile)
			)

		;; Load private config (for each init) ...
		(dolist (orgfile configlist)
			(rjh/load-private orgfile)
			)

		;; Load private config (overrides) ...
		(dolist (orgfile privatelist)
			(rjh/load-private orgfile)
			)
		))

(rjh/load-env)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left8")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(package-selected-packages
	 (quote
		(crux yasnippet-snippets xah-fly-keys workgroups which-key web-mode use-package undo-tree telephone-line solarized-theme seeing-is-believing scad-preview ruby-test-mode ruby-electric rtags rspec-mode rainbow-mode rainbow-delimiters projectile pinentry pamparam org-plus-contrib org-edna org-caldav org-bullets oauth2 material-theme magit ledger-mode jinja2-mode jdee irony-eldoc inf-ruby htmlize helm graphviz-dot-mode gnuplot git-timemachine flycheck-plantuml flycheck-irony fill-column-indicator f ess elpy ecb diminish diff-hl deft company-jedi company-irony calfw-org calfw-ical calfw-cal calfw bbdb-vcard auto-compile aggressive-indent)))
 '(seeing-is-believing-prefix "C-."))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-tag ((t (:background "gray6" :foreground "white smoke" :slant oblique :weight bold :height 0.8)))))
