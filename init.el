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

;; Ensure customization directory exists
(setq custom-file-directory "~/.emacs.d/custom/")
(unless
    (file-exists-p custom-file-directory)
  (make-directory custom-file-directory)
  )

;; Load customizations from file (in `custom-file-directory')
;; File can be specified on the command-line via environment variable
;; E.g.
;; /usr/bin/env EMACS_CUSTOM="custom.el" /usr/bin/emacs
;; Should start emacs with default custom.el file
;; Otherwise default is used
(setq custom-file
      (expand-file-name
       (or 
	(getenv "EMACS_CUSTOM")
	"default.el")
       custom-file-directory))

;; Ensure file exists before loading
(if (file-exists-p custom-file)
    (load custom-file nil nil nil t))

;; Loads configuration based on environment variables
(rjh/load-env)

;; Load configuration based on customization values
(rjh/load-custom)

