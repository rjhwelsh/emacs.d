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

;; Variables
(defvar rjh/loaded-config-plist-list '()
  "A list of each loaded configuration properties")

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
;; (requires dynamic scoping) 
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
