;;; rjh.el --- Loading functions for rjh's org conf files -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 rjhwelsh
;;
;; Author: Roger Welsh <rjhwelsh at gmail dot com>
;; Keywords: init.el org babel
;; Homepage: https://github.com/rjhwelsh/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defgroup rjh nil
  "Customizations by rjh"
  :tag "rjh config customization")

(defcustom rjh/config-env "EMACS_CONFIG"
  "Set environment variable to parse for rjh/init configuration files"
  :type '(string)
  :group 'rjh
  )
(defcustom rjh/config-private-env "EMACS_CONFIG_PRIVATE"
  "Set environment variable to parse for private configuration files"
  :type '(string)
  :group 'rjh
  )

(defun rjh/prompt-for-directory (dir prompt)
  "Prompts for a directory location, if it does not exist."
    ;; Prompt if directory does not exist
    (if (file-exists-p dir)
	dir
      (read-directory-name prompt)
      )
    )

(defcustom rjh/local-config-repo
  ;; Use load-file-name as basis for repo
  ;; else, if it exists, use default location 
  ;; else, ask user to pick a location
  (rjh/prompt-for-directory
   (or (file-name-directory load-file-name)
       "~/.emacs.d/rjh")
   ;; Prompt if directory does not exist
   "Set location of rjhwelsh/emacs.d repo:"
   )
  "Set location of rjhwelsh/emacs.d repo"
  :type '(directory)
  :group 'rjh
  )

(defcustom rjh/local-private-dir 
  (rjh/prompt-for-directory
   "~/.emacs.d/private"
   "Set location of rjh/local-private-dir:"
   )
  "Directory path for private configuration files"
  :type '(directory)
  :group 'rjh
  )

(defcustom rjh/local-init-dir
  (rjh/prompt-for-directory
   (expand-file-name
    "init"
    rjh/local-config-repo )
   "Set location of rjh/local-init-dir:"
   )
  "Directory path for rjh/init configuration files"
  :type '(directory)
  :group 'rjh
  )

(defcustom rjh/config
  '()
  "A list of plists describing rjh configuration files to load"
  :type '(list)
  :group 'rjh
  )

;; Variables
(defvar rjh/config-loaded '()
  "A list of plists describing each loaded configuration")

(defun rjh/load-base (dir)
  "Generates rjh/load functions
     dir - load directory"
  (lambda (base props)
    (let ((orgfile (expand-file-name (concat base ".org") dir)))
      (if (file-readable-p orgfile)
	  (progn
	    (org-babel-load-file orgfile t)
	    (add-to-list 'rjh/config-loaded props t)
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
;; (requires dynamic scoping) 
(defun rjh/load-init (conf)
  "Use org-babel-load-file to load init/conf in rjh/local-config-repo"
  (let* ((dir rjh/local-init-dir)
	 (loadf 'rjh/load-init)
	 (dirsym 'rjh/load-init-dir)
	 (props (list
		 :loadf loadf
		 :dirsym dirsym
		 :dir dir
		 :conf conf))
	 )
    (message "Loading init/%s ..." conf)
    (funcall (rjh/load-base dir) conf props))
  ;; Also load private/conf
  (rjh/load-private conf)
  )

(defun rjh/load-private (conf)
  "Use org-babel-load-file to load private/conf"
  (let* ((dir rjh/local-private-dir)
	 (loadf 'rjh/load-private)
	 (dirsym 'rjh/load-private-dir)
	 (props (list
		 :loadf loadf
		 :dirsym dirsym
		 :dir dir
		 :conf conf))
	 )
    (message "Loading private/%s ..." conf)
    (funcall (rjh/load-base dir) conf props)))

(defun rjh/load-config-plist-list (config-plist-list)
  "Loads configuration from config-plist-list
plist requires the following values, for each entry:
    :loadf The loading function
    :conf  The org config file"
  (dolist (config-plist config-plist-list)
    (funcall
     (plist-get config-plist :loadf)
     (plist-get config-plist :conf)
     )))

(defun rjh/config-plist-list-from-env (env loadf)
  "Reads config-plist-list from environment variable
    env - environment variable name
    loadf - the loading function for each conf"
  (let ((conf-list (delete "" (split-string (or (getenv env) "")))))
    (mapcar
     (lambda (conf) (list :loadf loadf :conf conf)) conf-list)))

(defun rjh/load-env ()
  "Loads configuration from environment variable, rjh/config-env"
  (progn
    (rjh/load-config-plist-list
     (append
      (rjh/config-plist-list-from-env rjh/config-env 'rjh/load-init)
      (rjh/config-plist-list-from-env rjh/config-private-env 'rjh/load-private)))))

(defun rjh/load-custom()
  "Loads configuration from customization variable, rjh/config"
  (rjh/load-config-plist-list rjh/config))

(defun rjh/save-custom()
  "Saves current configuration to customization variable, rjh/config"
  (customize-save-variable 'rjh/config rjh/config-loaded))

