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

;; Most configurations use use-package 
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Follow symlinks
(require 'vc)
(setq vc-follow-symlinks t)

;; Add rjh repository elisp
(let* ((repo "~/.emacs.d/rjh")
       ;; Prompt if directory does not exist
       (repo (if (file-exists-p repo)
		 repo
	       (read-directory-name "Set location of rjhwelsh/emacs.d repo:")
	       ))
       )
  ;; Load rjh.el  
  (load (expand-file-name "rjh.el" repo))
  )

;; Load customizations from file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Load configuration based on customization values
(rjh/load-custom)

;; Loads configuration based on environment variables
(rjh/load-env)
