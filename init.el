;; Remove any default org-mode from load-path
(mapcar
 '(lambda (arg) (delete arg load-path))
 (remove nil
				 (mapcar
					'(lambda (arg)
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

;; Set rjh config repo location
(setq rjh/local-config-repo "~/.emacs.d/rjh/")

;; Set environment variable to parse for configuration files
(setq rjh/config-env "EMACS_CONFIG")

;; Load config methods
(setq rjh/local-init-dir
			(expand-file-name
			 "init"
			 rjh/local-config-repo ))

(defun rjh/load-init (orgfile)
	"Use org-babel-load-file to load init/orgfile in rjh/local-config-repo"
	;; Force babel refresh of main config files
	;; 	(delete-file "~/.emacs.d/configuration.el" nil)

	(org-babel-load-file
	 (expand-file-name (concat orgfile ".org") rjh/local-init-dir)
	 t
	 ))

(defun rjh/load-env ()
	"Loads configuration from environment variable, rjh/config-env"
	(let ((configlist (delete "" (split-string (or (getenv rjh/config-env) ""))))
				)
		(dolist (orgfile configlist)
			(format-message "Loading %s ..." orgfile)
			(rjh/load-init orgfile)
			)
		))
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
