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
(defvar rjh/local-dir-plist
  (list
   :init rjh/local-init-dir
   :private rjh/local-private-dir
   )
  "A plist of local dir to load configuration from")

(defvar rjh/config-loaded '()
  "A list of plists describing each loaded configuration")

;; Functions
(defun rjh/config-file-path (conf dir)
  "Converts conf to a path, using dir, and base"
  (expand-file-name (concat conf ".org") dir))

(defun rjh/config-exists-p (conf dir)
  "Returns whether config exists in directory"
  (if (file-readable-p
       (rjh/config-file-path conf dir))
      t))

(defun rjh/config-list (dir)
  "Lists available config options in directory"
  (mapcar
   (lambda (string)
     (substring
      string
      (+ (length (expand-file-name dir)) 1)
      (- (length string) 4)))
   (directory-files-recursively dir ".*\.org$")))


(defun rjh/config-load (sym conf)
  "Loads config CONF for sym SYM"
  (let* ((dir (plist-get rjh/local-dir-plist sym))
	 (props (list
		 :sym sym
		 :conf conf))
	 (orgfile (rjh/config-file-path conf dir))
	 )
    (message "Searching for %s%s ..." conf (symbol-name sym))
    (if (rjh/config-exists-p conf dir)
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
  )

(defun rjh/load-config-plist-list (config-plist-list)
  "Loads configuration from config-plist-list
plist requires the following values, for each entry:
    :loadf The loading function
    :conf  The org config file"
  (dolist (config-plist config-plist-list)
    (funcall
     'rjh/config-load
     (plist-get config-plist :sym)
     (plist-get config-plist :conf)
     )))

(defun rjh/config-plist-list-from-env (env sym)
  "Reads config-plist-list from environment variable
    env - environment variable name
    loadf - the loading function for each conf"
  (let ((conf-list (delete "" (split-string (or (getenv env) "")))))
    (mapcar
     (lambda (conf) (list :sym sym :conf conf)) conf-list)))

(defun rjh/load-env ()
  "Loads configuration from environment variable, rjh/config-env"
  (progn
    (rjh/load-config-plist-list
     (append
      (rjh/config-plist-list-from-env rjh/config-env :init)
      (rjh/config-plist-list-from-env rjh/config-env :private)
      (rjh/config-plist-list-from-env rjh/config-private-env :init)))))

(defun rjh/load-custom ()
  "Loads configuration from customization variable, rjh/config"
  (rjh/load-config-plist-list rjh/config))

(defun rjh/save-custom ()
  "Saves current configuration to customization variable, rjh/config"
  (customize-save-variable 'rjh/config rjh/config-loaded))

;; Interactive functions
(defun rjh/load (conf)
  "Load configuration file/s
Will attempt to load configuration file(s) from:
 rjh/local-init-dir
 rjh/local-private-dir"
  (interactive
   (list
    (completing-read
     "Select config: "
     (completion-table-with-cache 'rjh/config-completion-function t)
     nil
     t)))
  (rjh/config-load :init conf)
  (rjh/config-load :private conf)
  )

;; Completion
(defun rjh/config-completion-function (string)
  "Return a list of strings completion table for loading config"
  (append
   (rjh/config-list rjh/local-init-dir)
   (rjh/config-list rjh/local-private-dir)
   )
  )

