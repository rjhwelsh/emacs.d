;;; org-agenda.el patches --- Additional/adjusted code for org-agenda -*- lexical-binding: t; -*-

;; This is an attempt to revive Jesse Johnson's work on priority inheritance in org-mode
;; See https://lists.gnu.org/archive/html/emacs-orgmode/2018-07/msg00067.html

;; Copyright (C) 2020 rjhwelsh
;;
;; Author: Roger Welsh <rjhwelsh at gmail dot com>
;; Keywords: org priority inheritance
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

(defun org-search-view (&optional todo-only string edit-at)
  "Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files
listed in `org-agenda-text-search-extra-files' unless a restriction lock
is active."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq todo-only (car org-agenda-overriding-arguments)
	  string (nth 1 org-agenda-overriding-arguments)
	  edit-at (nth 2 org-agenda-overriding-arguments)))
  (let* ((props (list 'face nil
		      'done-face 'org-agenda-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo (format "mouse-2 or RET jump to location")))
	 (full-words org-agenda-search-view-force-full-words)
	 (org-agenda-text-search-extra-files org-agenda-text-search-extra-files)
	 regexp rtn rtnall files file pos inherited-tags
	 marker category level tags c neg re boolean
	 ee txt beg end words regexps+ regexps- hdl-only buffer beg1 str)
    (unless (and (not edit-at)
		 (stringp string)
		 (string-match "\\S-" string))
      (setq string (read-string
		    (if org-agenda-search-view-always-boolean
			"[+-]Word/{Regexp} ...: "
		      "Phrase or [+-]Word/{Regexp} ...: ")
		    (cond
		     ((integerp edit-at) (cons string edit-at))
		     (edit-at string))
		    'org-agenda-search-history)))
    (catch 'exit
      (when org-agenda-sticky
	(setq org-agenda-buffer-name
	      (if (stringp string)
		  (format "*Org Agenda(%s:%s)*"
			  (or org-keys (or (and todo-only "S") "s")) string)
		(format "*Org Agenda(%s)*" (or (and todo-only "S") "s")))))
      (org-agenda-prepare "SEARCH")
      (org-compile-prefix-format 'search)
      (org-set-sorting-strategy 'search)
      (setq org-agenda-redo-command
	    (list 'org-search-view (if todo-only t nil)
		  (list 'if 'current-prefix-arg nil string)))
      (setq org-agenda-query-string string)
      (if (equal (string-to-char string) ?*)
	  (setq hdl-only t
		words (substring string 1))
	(setq words string))
      (when (equal (string-to-char words) ?!)
	(setq todo-only t
	      words (substring words 1)))
      (when (equal (string-to-char words) ?:)
	(setq full-words t
	      words (substring words 1)))
      (when (or org-agenda-search-view-always-boolean
		(member (string-to-char words) '(?- ?+ ?\{)))
	(setq boolean t))
      (setq words (split-string words))
      (let (www w)
	(while (setq w (pop words))
	  (while (and (string-match "\\\\\\'" w) words)
	    (setq w (concat (substring w 0 -1) " " (pop words))))
	  (push w www))
	(setq words (nreverse www) www nil)
	(while (setq w (pop words))
	  (when (and (string-match "\\`[-+]?{" w)
		     (not (string-match "}\\'" w)))
	    (while (and words (not (string-match "}\\'" (car words))))
	      (setq w (concat w " " (pop words))))
	    (setq w (concat w " " (pop words))))
	  (push w www))
	(setq words (nreverse www)))
      (setq org-agenda-last-search-view-search-was-boolean boolean)
      (when boolean
	(let (wds w)
	  (while (setq w (pop words))
	    (when (or (equal (substring w 0 1) "\"")
		      (and (> (length w) 1)
			   (member (substring w 0 1) '("+" "-"))
			   (equal (substring w 1 2) "\"")))
	      (while (and words (not (equal (substring w -1) "\"")))
		(setq w (concat w " " (pop words)))))
	    (and (string-match "\\`\\([-+]?\\)\"" w)
		 (setq w (replace-match "\\1" nil nil w)))
	    (and (equal (substring w -1) "\"") (setq w (substring w 0 -1)))
	    (push w wds))
	  (setq words (nreverse wds))))
      (if boolean
	  (mapc (lambda (w)
		  (setq c (string-to-char w))
		  (if (equal c ?-)
		      (setq neg t w (substring w 1))
		    (if (equal c ?+)
			(setq neg nil w (substring w 1))
		      (setq neg nil)))
		  (if (string-match "\\`{.*}\\'" w)
		      (setq re (substring w 1 -1))
		    (if full-words
			(setq re (concat "\\<" (regexp-quote (downcase w)) "\\>"))
		      (setq re (regexp-quote (downcase w)))))
		  (if neg (push re regexps-) (push re regexps+)))
		words)
	(push (mapconcat (lambda (w) (regexp-quote w)) words "\\s-+")
	      regexps+))
      (setq regexps+ (sort regexps+ (lambda (a b) (> (length a) (length b)))))
      (if (not regexps+)
	  (setq regexp org-outline-regexp-bol)
	(setq regexp (pop regexps+))
	(when hdl-only (setq regexp (concat org-outline-regexp-bol ".*?"
					    regexp))))
      (setq files (org-agenda-files nil 'ifmode))
      ;; Add `org-agenda-text-search-extra-files' unless there is some
      ;; restriction.
      (when (eq (car org-agenda-text-search-extra-files) 'agenda-archives)
	(pop org-agenda-text-search-extra-files)
	(unless (get 'org-agenda-files 'org-restrict)
	  (setq files (org-add-archive-files files))))
      ;; Uniquify files.  However, let `org-check-agenda-file' handle
      ;; non-existent ones.
      (setq files (cl-remove-duplicates
		   (append files org-agenda-text-search-extra-files)
		   :test (lambda (a b)
			   (and (file-exists-p a)
				(file-exists-p b)
				(file-equal-p a b))))
	    rtnall nil)
      (while (setq file (pop files))
	(setq ee nil)
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (unless buffer
	    ;; If file does not exist, make sure an error message is sent
	    (setq rtn (list (format "ORG-AGENDA-ERROR: No such org-file %s"
				    file))))
	  (with-current-buffer buffer
	    (with-syntax-table (org-search-syntax-table)
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in Org mode" file))
	      (let ((case-fold-search t))
		(save-excursion
		  (save-restriction
		    (if (eq buffer org-agenda-restrict)
			(narrow-to-region org-agenda-restrict-begin
					  org-agenda-restrict-end)
		      (widen))
		    (goto-char (point-min))
		    (unless (or (org-at-heading-p)
				(outline-next-heading))
		      (throw 'nextfile t))
		    (goto-char (max (point-min) (1- (point))))
		    (while (re-search-forward regexp nil t)
		      (org-back-to-heading t)
		      (while (and (not (zerop org-agenda-search-view-max-outline-level))
				  (> (org-reduced-level (org-outline-level))
				     org-agenda-search-view-max-outline-level)
				  (forward-line -1)
				  (org-back-to-heading t)))
		      (skip-chars-forward "* ")
		      (setq beg (point-at-bol)
			    beg1 (point)
			    end (progn
				  (outline-next-heading)
				  (while (and (not (zerop org-agenda-search-view-max-outline-level))
					      (> (org-reduced-level (org-outline-level))
						 org-agenda-search-view-max-outline-level)
					      (forward-line 1)
					      (outline-next-heading)))
				  (point)))

		      (catch :skip
			(goto-char beg)
			(org-agenda-skip)
			(setq str (buffer-substring-no-properties
				   (point-at-bol)
				   (if hdl-only (point-at-eol) end)))
			(mapc (lambda (wr) (when (string-match wr str)
					     (goto-char (1- end))
					     (throw :skip t)))
			      regexps-)
			(mapc (lambda (wr) (unless (string-match wr str)
					     (goto-char (1- end))
					     (throw :skip t)))
			      (if todo-only
				  (cons (concat "^\\*+[ \t]+"
                                                org-not-done-regexp)
					regexps+)
				regexps+))
			(goto-char beg)
			(setq marker (org-agenda-new-marker (point))
			      category (org-get-category)
			      level (make-string (org-reduced-level (org-outline-level)) ? )
			      priority (org-get-priority)
			      inherited-tags
			      (or (eq org-agenda-show-inherited-tags 'always)
				  (and (listp org-agenda-show-inherited-tags)
				       (memq 'todo org-agenda-show-inherited-tags))
				  (and (eq org-agenda-show-inherited-tags t)
				       (or (eq org-agenda-use-tag-inheritance t)
					   (memq 'todo org-agenda-use-tag-inheritance))))
			      tags (org-get-tags nil (not inherited-tags))
			      txt (org-agenda-format-item
				   ""
				   (buffer-substring-no-properties
				    beg1 (point-at-eol))
				   level category priority tags t))
			(org-add-props txt props
			  'org-marker marker 'org-hd-marker marker
			  'org-todo-regexp org-todo-regexp
			  'level level
			  'org-complex-heading-regexp org-complex-heading-regexp
			  'priority priority
			  'type "search")
			(push txt ee)
			(goto-char (1- end))))))))))
	(setq rtn (nreverse ee))
	(setq rtnall (append rtnall rtn)))
      (org-agenda--insert-overriding-header
	(with-temp-buffer
	  (insert "Search words: ")
	  (add-text-properties (point-min) (1- (point))
			       (list 'face 'org-agenda-structure))
	  (setq pos (point))
	  (insert string "\n")
	  (add-text-properties pos (1- (point)) (list 'face 'org-warning))
	  (setq pos (point))
	  (unless org-agenda-multi
	    (insert (substitute-command-keys "\\<org-agenda-mode-map>\
Press `\\[org-agenda-manipulate-query-add]', \
`\\[org-agenda-manipulate-query-subtract]' to add/sub word, \
`\\[org-agenda-manipulate-query-add-re]', \
`\\[org-agenda-manipulate-query-subtract-re]' to add/sub regexp, \
`\\[universal-argument] \\[org-agenda-redo]' for a fresh search\n"))
	    (add-text-properties pos (1- (point))
				 (list 'face 'org-agenda-structure)))
	  (buffer-string)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall 'search) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type search
					     org-last-args (,todo-only ,string ,edit-at)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))


(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.
(defvar diary-list-entries-hook)
(defvar diary-time-regexp)
(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (require 'diary-lib)
  (let* ((diary-fancy-buffer "*temporary-fancy-diary-buffer*")
	 (diary-display-function 'diary-fancy-display)
	 (pop-up-frames nil)
	 (diary-list-entries-hook
	  (cons 'org-diary-default-entry diary-list-entries-hook))
	 (diary-file-name-prefix nil) ; turn this feature off
	 (diary-modify-entry-list-string-function 'org-modify-diary-entry-string)
	 (diary-time-regexp (concat "^" diary-time-regexp))
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
	(funcall (if (fboundp 'diary-list-entries)
		     'diary-list-entries 'list-diary-entries)
		 date 1)))
    (if (not (get-buffer diary-fancy-buffer))
	(setq entries nil)
      (with-current-buffer diary-fancy-buffer
	(setq buffer-read-only nil)
	(if (zerop (buffer-size))
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (zerop (buffer-size))
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))
	    (setq entries
		  (with-temp-buffer
		    (insert entries) (goto-char (point-min))
		    (while (re-search-forward "\n[ \t]+\\(.+\\)$" nil t)
		      (unless (save-match-data (string-match diary-time-regexp (match-string 1)))
			(replace-match (concat "; " (match-string 1)))))
		    (buffer-string)))))
	(set-buffer-modified-p nil)
	(kill-buffer diary-fancy-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-agenda-format-item "" x nil "Diary" nil nil 'time))
	       ;; Extend the text properties to the beginning of the line
	       (org-add-props x (text-properties-at (1- (length x)) x)
		 'type "diary" 'date date 'face 'org-agenda-diary))
	     entries)))))

(defun org-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
		      'done-face 'org-agenda-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (case-fold-search nil)
	 (regexp (format org-heading-keyword-regexp-format
			 (cond
			  ((and org-select-this-todo-keyword
				(equal org-select-this-todo-keyword "*"))
			   org-todo-regexp)
			  (org-select-this-todo-keyword
			   (concat "\\("
				   (mapconcat 'identity
					      (org-split-string
					       org-select-this-todo-keyword
					       "|")
					      "\\|") "\\)"))
			  (t org-not-done-regexp))))
	 marker priority category level tags todo-state
	 ts-date ts-date-type ts-date-pair
	 ee txt beg end inherited-tags todo-state-end-pos)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(save-match-data
	  (beginning-of-line)
	  (org-agenda-skip)
	  (setq beg (point) end (save-excursion (outline-next-heading) (point)))
	  (unless (and (setq todo-state (org-get-todo-state))
		       (setq todo-state-end-pos (match-end 2)))
	    (goto-char end)
	    (throw :skip nil))
	  (when (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item end)
	    (goto-char (1+ beg))
	    (or org-agenda-todo-list-sublevels (org-end-of-subtree 'invisible))
	    (throw :skip nil)))
	(goto-char (match-beginning 2))
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      category (org-get-category)
	      ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
	      ts-date (car ts-date-pair)
	      ts-date-type (cdr ts-date-pair)
	      txt (org-trim (buffer-substring (match-beginning 2) (match-end 0)))
	      priority (1+ (org-get-priority))
	      inherited-tags
	      (or (eq org-agenda-show-inherited-tags 'always)
		  (and (listp org-agenda-show-inherited-tags)
		       (memq 'todo org-agenda-show-inherited-tags))
		  (and (eq org-agenda-show-inherited-tags t)
		       (or (eq org-agenda-use-tag-inheritance t)
			   (memq 'todo org-agenda-use-tag-inheritance))))
	      tags (org-get-tags nil (not inherited-tags))
	      level (make-string (org-reduced-level (org-outline-level)) ? )
	      txt (org-agenda-format-item "" txt level category priority tags t))
	(org-add-props txt props
	  'org-marker marker 'org-hd-marker marker
	  'priority priority
	  'level level
	  'ts-date ts-date
	  'type (concat "todo" ts-date-type) 'todo-state todo-state)
	(push txt ee)
	(if org-agenda-todo-list-sublevels
	    (goto-char todo-state-end-pos)
	  (org-end-of-subtree 'invisible))))
    (nreverse ee)))


(defun org-agenda-get-timestamps (&optional deadlines)
  "Return the date stamp information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view."
  (let* ((props (list 'face 'org-agenda-calendar-event
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (current (calendar-absolute-from-gregorian date))
	 (today (org-today))
	 (deadline-position-alist
	  (mapcar (lambda (d)
		    (let ((m (get-text-property 0 'org-hd-marker d)))
		      (and m (marker-position m))))
		  deadlines))
	 ;; Match time-stamps set to current date, time-stamps with
	 ;; a repeater, and S-exp time-stamps.
	 (regexp
	  (concat
	   (if org-agenda-include-inactive-timestamps "[[<]" "<")
	   (regexp-quote
	    (substring
	     (format-time-string
	      (car org-time-stamp-formats)
	      (encode-time	; DATE bound by calendar
	       0 0 0 (nth 1 date) (car date) (nth 2 date)))
	     1 11))
	   "\\|\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[hdwmy]>\\)"
	   "\\|\\(<%%\\(([^>\n]+)\\)>\\)"))
	 timestamp-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      ;; Skip date ranges, scheduled and deadlines, which are handled
      ;; specially.  Also skip time-stamps before first headline as
      ;; there would be no entry to add to the agenda.  Eventually,
      ;; ignore clock entries.
      (catch :skip
	(save-match-data
	  (when (or (org-at-date-range-p)
		    (org-at-planning-p)
		    (org-before-first-heading-p)
		    (and org-agenda-include-inactive-timestamps
			 (org-at-clock-log-p)))
	    (throw :skip nil))
	  (org-agenda-skip))
	(let* ((pos (match-beginning 0))
	       (repeat (match-string 1))
	       (sexp-entry (match-string 3))
	       (time-stamp (if (or repeat sexp-entry) (match-string 0)
			     (save-excursion
			       (goto-char pos)
			       (looking-at org-ts-regexp-both)
			       (match-string 0))))
	       (todo-state (org-get-todo-state))
	       (warntime (get-text-property (point) 'org-appt-warntime))
	       (done? (member todo-state org-done-keywords)))
	  ;; Possibly skip done tasks.
	  (when (and done? org-agenda-skip-timestamp-if-done)
	    (throw :skip t))
	  ;; S-exp entry doesn't match current day: skip it.
	  (when (and sexp-entry (not (org-diary-sexp-entry sexp-entry "" date)))
	    (throw :skip nil))
	  (when repeat
	    (let* ((past
		    ;; A repeating time stamp is shown at its base
		    ;; date and every repeated date up to TODAY.  If
		    ;; `org-agenda-prefer-last-repeat' is non-nil,
		    ;; however, only the last repeat before today
		    ;; (inclusive) is shown.
		    (org-agenda--timestamp-to-absolute
		     repeat
		     (if (or (> current today)
			     (eq org-agenda-prefer-last-repeat t)
			     (member todo-state org-agenda-prefer-last-repeat))
			 today
		       current)
		     'past (current-buffer) pos))
		   (future
		    ;;  Display every repeated date past TODAY
		    ;;  (exclusive) unless
		    ;;  `org-agenda-show-future-repeats' is nil.  If
		    ;;  this variable is set to `next', only display
		    ;;  the first repeated date after TODAY
		    ;;  (exclusive).
		    (cond
		     ((<= current today) past)
		     ((not org-agenda-show-future-repeats) past)
		     (t
		      (let ((base (if (eq org-agenda-show-future-repeats 'next)
				      (1+ today)
				    current)))
			(org-agenda--timestamp-to-absolute
			 repeat base 'future (current-buffer) pos))))))
	      (when (and (/= current past) (/= current future))
		(throw :skip nil))))
	  (save-excursion
	    (re-search-backward org-outline-regexp-bol nil t)
	    ;; Possibly skip time-stamp when a deadline is set.
	    (when (and org-agenda-skip-timestamp-if-deadline-is-shown
		       (assq (point) deadline-position-alist))
	      (throw :skip nil))
	    (let* ((category (org-get-category pos))
		   (priority (if habit?
				 (org-habit-get-priority (org-habit-parse-todo))
			       (org-get-priority item)))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (consp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags nil (not inherited-tags)))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (and (looking-at "\\*+[ \t]+\\(.*\\)")
			      (match-string 1)))
		   (inactive? (= (char-after pos) ?\[))
		   (habit? (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
		   (item
		    (org-agenda-format-item
		     (and inactive? org-agenda-inactive-leader)
		     head level category priority tags
		     time-stamp org-ts-regexp habit?)))
	      (org-add-props item props
		'priority priority
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker)
		'date date
		'level level
		'ts-date (if repeat (org-agenda--timestamp-to-absolute repeat)
			   current)
		'todo-state todo-state
		'warntime warntime
		'type "timestamp")
	      (push item timestamp-items))))
	(when org-agenda-skip-additional-timestamps-same-entry
	  (outline-next-heading))))
    (nreverse timestamp-items)))

(defun org-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (require 'diary-lib)
  (let* ((props (list 'face 'org-agenda-calendar-sexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp "^&?%%(")
	 marker category extra level ee txt tags entry
	 result beg b sexp sexp-entry todo-state warntime inherited-tags)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq beg (match-beginning 0))
	(goto-char (1- (match-end 0)))
	(setq b (point))
	(forward-sexp 1)
	(setq sexp (buffer-substring b (point)))
	(setq sexp-entry (if (looking-at "[ \t]*\\(\\S-.*\\)")
			     (org-trim (match-string 1))
			   ""))
	(setq result (org-diary-sexp-entry sexp sexp-entry date))
	(when result
	  (setq marker (org-agenda-new-marker beg)
		level (make-string (org-reduced-level (org-outline-level)) ? )
		category (org-get-category beg)
		inherited-tags
		(or (eq org-agenda-show-inherited-tags 'always)
		    (and (listp org-agenda-show-inherited-tags)
			 (memq 'agenda org-agenda-show-inherited-tags))
		    (and (eq org-agenda-show-inherited-tags t)
			 (or (eq org-agenda-use-tag-inheritance t)
			     (memq 'agenda org-agenda-use-tag-inheritance))))
		priority (org-get-priority)
		tags (org-get-tags nil (not inherited-tags))
		todo-state (org-get-todo-state)
		warntime (get-text-property (point) 'org-appt-warntime)
		extra nil)

	  (dolist (r (if (stringp result)
			 (list result)
		       result)) ;; we expect a list here
	    (when (and org-agenda-diary-sexp-prefix
		       (string-match org-agenda-diary-sexp-prefix r))
	      (setq extra (match-string 0 r)
		    r (replace-match "" nil nil r)))
	    (if (string-match "\\S-" r)
		(setq txt r)
	      (setq txt "SEXP entry returned empty string"))
	    (setq txt (org-agenda-format-item extra txt level category priority tags 'time))
	    (org-add-props txt props 'org-marker marker
			   'date date 'todo-state todo-state
			   'level level 'type "sexp" 'warntime warntime)
	    (push txt ee)))))
    (nreverse ee)))

(defun org-agenda-get-progress ()
  "Return the logged TODO entries for agenda display."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (items (if (consp org-agenda-show-log-scoped)
		    org-agenda-show-log-scoped
		  (if (eq org-agenda-show-log-scoped 'clockcheck)
		      '(clock)
		    org-agenda-log-mode-items)))
	 (parts
	  (delq nil
		(list
		 (when (memq 'closed items) (concat "\\<" org-closed-string))
		 (when (memq 'clock items) (concat "\\<" org-clock-string))
		 (when (memq 'state items)
		   (format "- +State \"%s\".*?" org-todo-regexp)))))
	 (parts-re (if parts (mapconcat 'identity parts "\\|")
		     (error "`org-agenda-log-mode-items' is empty")))
	 (regexp (concat
		  "\\(" parts-re "\\)"
		  " *\\["
		  (regexp-quote
		   (substring
		    (format-time-string
		     (car org-time-stamp-formats)
		     (encode-time  ; DATE bound by calendar
		      0 0 0 (nth 1 date) (car date) (nth 2 date)))
		    1 11))))
	 (org-agenda-search-headline-for-time nil)
	 marker hdmarker priority category level tags closedp type
	 statep clockp state ee txt extra timestr rest clocked inherited-tags)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      closedp (equal (match-string 1) org-closed-string)
	      statep (equal (string-to-char (match-string 1)) ?-)
	      clockp (not (or closedp statep))
	      state (and statep (match-string 2))
	      category (org-get-category (match-beginning 0))
	      timestr (buffer-substring (match-beginning 0) (point-at-eol)))
	(when (string-match "\\]" timestr)
	  ;; substring should only run to end of time stamp
	  (setq rest (substring timestr (match-end 0))
		timestr (substring timestr 0 (match-end 0)))
	  (if (and (not closedp) (not statep)
		   (string-match "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\].*?\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
				 rest))
	      (progn (setq timestr (concat (substring timestr 0 -1)
					   "-" (match-string 1 rest) "]"))
		     (setq clocked (match-string 2 rest)))
	    (setq clocked "-")))
	(save-excursion
	  (setq extra
		(cond
		 ((not org-agenda-log-mode-add-notes) nil)
		 (statep
		  (and (looking-at ".*\\\\\n[ \t]*\\([^-\n \t].*?\\)[ \t]*$")
		       (match-string 1)))
		 (clockp
		  (and (looking-at ".*\n[ \t]*-[ \t]+\\([^-\n \t].*?\\)[ \t]*$")
		       (match-string 1)))))
	  (if (not (re-search-backward org-outline-regexp-bol nil t))
	      (throw :skip nil)
	    (goto-char (match-beginning 0))
	    (setq hdmarker (org-agenda-new-marker)
		  inherited-tags
		  (or (eq org-agenda-show-inherited-tags 'always)
		      (and (listp org-agenda-show-inherited-tags)
			   (memq 'todo org-agenda-show-inherited-tags))
		      (and (eq org-agenda-show-inherited-tags t)
			   (or (eq org-agenda-use-tag-inheritance t)
			       (memq 'todo org-agenda-use-tag-inheritance))))
		  tags (org-get-tags nil (not inherited-tags))
		  level (make-string (org-reduced-level (org-outline-level)) ? ))
	    (looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
	    (setq txt (match-string 1))
	    (when extra
	      (if (string-match "\\([ \t]+\\)\\(:[^ \n\t]*?:\\)[ \t]*$" txt)
		  (setq txt (concat (substring txt 0 (match-beginning 1))
				    " - " extra " " (match-string 2 txt)))
		(setq txt (concat txt " - " extra))))
	    (setq txt (org-agenda-format-item
		       (cond
			(closedp "Closed:    ")
			(statep (concat "State:     (" state ")"))
			(t (concat "Clocked:   (" clocked  ")")))
		       txt level category priority tags timestr)))
	  (setq type (cond (closedp "closed")
			   (statep "state")
			   (t "clock")))
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker 'face 'org-agenda-done
	    'priority priority 'level level
	    'type type 'date date
	    'undone-face 'org-warning 'done-face 'org-agenda-done)
	  (push txt ee))
	(goto-char (point-at-eol))))
    (nreverse ee)))

(defun org-agenda-get-deadlines (&optional with-hour)
  "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-deadline-time-hour-regexp
		   org-deadline-time-regexp))
	 (today (org-today))
	 (today? (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
	 deadline-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(unless (save-match-data (org-at-planning-p)) (throw :skip nil))
	(org-agenda-skip)
	(let* ((s (match-string 1))
	       (pos (1- (match-beginning 1)))
	       (todo-state (save-match-data (org-get-todo-state)))
	       (done? (member todo-state org-done-keywords))
               (sexp? (string-prefix-p "%%" s))
	       ;; DEADLINE is the deadline date for the entry.  It is
	       ;; either the base date or the last repeat, according
	       ;; to `org-agenda-prefer-last-repeat'.
	       (deadline
		(cond
		 (sexp? (org-agenda--timestamp-to-absolute s current))
		 ((or (eq org-agenda-prefer-last-repeat t)
		      (member todo-state org-agenda-prefer-last-repeat))
		  (org-agenda--timestamp-to-absolute
		   s today 'past (current-buffer) pos))
		 (t (org-agenda--timestamp-to-absolute s))))
	       ;; REPEAT is the future repeat closest from CURRENT,
	       ;; according to `org-agenda-show-future-repeats'. If
	       ;; the latter is nil, or if the time stamp has no
	       ;; repeat part, default to DEADLINE.
	       (repeat
		(cond
		 (sexp? deadline)
		 ((<= current today) deadline)
		 ((not org-agenda-show-future-repeats) deadline)
		 (t
		  (let ((base (if (eq org-agenda-show-future-repeats 'next)
				  (1+ today)
				current)))
		    (org-agenda--timestamp-to-absolute
		     s base 'future (current-buffer) pos)))))
	       (diff (- deadline current))
	       (suppress-prewarning
		(let ((scheduled
		       (and org-agenda-skip-deadline-prewarning-if-scheduled
			    (org-entry-get nil "SCHEDULED"))))
		  (cond
		   ((not scheduled) nil)
		   ;; The current item has a scheduled date, so
		   ;; evaluate its prewarning lead time.
		   ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		    ;; Use global prewarning-restart lead time.
		    org-agenda-skip-deadline-prewarning-if-scheduled)
		   ((eq org-agenda-skip-deadline-prewarning-if-scheduled
			'pre-scheduled)
		    ;; Set pre-warning to no earlier than SCHEDULED.
		    (min (- deadline
			    (org-agenda--timestamp-to-absolute scheduled))
			 org-deadline-warning-days))
		   ;; Set pre-warning to deadline.
		   (t 0))))
	       (wdays (or suppress-prewarning (org-get-wdays s))))
	  (cond
	   ;; Only display deadlines at their base date, at future
	   ;; repeat occurrences or in today agenda.
	   ((= current deadline) nil)
	   ((= current repeat) nil)
	   ((not today?) (throw :skip nil))
	   ;; Upcoming deadline: display within warning period WDAYS.
	   ((> deadline current) (when (> diff wdays) (throw :skip nil)))
	   ;; Overdue deadline: warn about it for
	   ;; `org-deadline-past-days' duration.
	   (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
	  ;; Possibly skip done tasks.
	  (when (and done?
		     (or org-agenda-skip-deadline-if-done
			 (/= deadline current)))
	    (throw :skip nil))
	  (save-excursion
	    (re-search-backward "^\\*+[ \t]+" nil t)
	    (goto-char (match-end 0))
	    (let* ((category (org-get-category))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (buffer-substring (point) (line-end-position)))
		   (priority
		    ;; Adjust priority to today reminders about deadlines.
		    ;; Overdue deadlines get the highest priority
		    ;; increase, then imminent deadlines and eventually
		    ;; more distant deadlines.
		    (let ((adjust (if today? (- diff) 0)))
		      (+ adjust (org-get-priority))))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (listp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags nil (not inherited-tags)))
		   (time
		    (cond
		     ;; No time of day designation if it is only
		     ;; a reminder.
		     ((and (/= current deadline) (/= current repeat)) nil)
		     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		      (concat (substring s (match-beginning 1)) " "))
		     (t 'time)))
		   (item
		    (org-agenda-format-item
		     ;; Insert appropriate suffixes before deadlines.
		     ;; Those only apply to today agenda.
		     (pcase-let ((`(,now ,future ,past)
				  org-agenda-deadline-leaders))
		       (cond
			((and today? (< deadline today)) (format past (- diff)))
			((and today? (> deadline today)) (format future diff))
			(t now)))
		     head level category priority tags time))
		   (face (org-agenda-deadline-face
			  (- 1 (/ (float diff) (max wdays 1)))))
		   (upcoming? (and today? (> deadline today)))
		   (warntime (get-text-property (point) 'org-appt-warntime)))
	      (org-add-props item props
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		'warntime warntime
		'level level
		'ts-date deadline
		'priority priority
		'todo-state todo-state
		'type (if upcoming? "upcoming-deadline" "deadline")
		'date (if upcoming? date deadline)
		'face (if done? 'org-agenda-done face)
		'undone-face face
		'done-face 'org-agenda-done)
	      (push item deadline-items))))))
    (nreverse deadline-items)))

(defun org-agenda-get-scheduled (&optional deadlines with-hour)
  "Return the scheduled information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view.  When WITH-HOUR is non-nil, only return
scheduled items with an hour specification like [h]h:mm."
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'done-face 'org-agenda-done
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-scheduled-time-hour-regexp
		   org-scheduled-time-regexp))
	 (today (org-today))
	 (todayp (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
	 (deadline-pos
	  (mapcar (lambda (d)
		    (let ((m (get-text-property 0 'org-hd-marker d)))
		      (and m (marker-position m))))
		  deadlines))
	 scheduled-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(unless (save-match-data (org-at-planning-p)) (throw :skip nil))
	(org-agenda-skip)
	(let* ((s (match-string 1))
	       (pos (1- (match-beginning 1)))
	       (todo-state (save-match-data (org-get-todo-state)))
	       (donep (member todo-state org-done-keywords))
	       (sexp? (string-prefix-p "%%" s))
	       ;; SCHEDULE is the scheduled date for the entry.  It is
	       ;; either the bare date or the last repeat, according
	       ;; to `org-agenda-prefer-last-repeat'.
	       (schedule
		(cond
		 (sexp? (org-agenda--timestamp-to-absolute s current))
		 ((or (eq org-agenda-prefer-last-repeat t)
		      (member todo-state org-agenda-prefer-last-repeat))
		  (org-agenda--timestamp-to-absolute
		   s today 'past (current-buffer) pos))
		 (t (org-agenda--timestamp-to-absolute s))))
	       ;; REPEAT is the future repeat closest from CURRENT,
	       ;; according to `org-agenda-show-future-repeats'. If
	       ;; the latter is nil, or if the time stamp has no
	       ;; repeat part, default to SCHEDULE.
	       (repeat
		(cond
		 (sexp? schedule)
		 ((<= current today) schedule)
		 ((not org-agenda-show-future-repeats) schedule)
		 (t
		  (let ((base (if (eq org-agenda-show-future-repeats 'next)
				  (1+ today)
				current)))
		    (org-agenda--timestamp-to-absolute
		     s base 'future (current-buffer) pos)))))
	       (diff (- current schedule))
	       (warntime (get-text-property (point) 'org-appt-warntime))
	       (pastschedp (< schedule today))
	       (futureschedp (> schedule today))
	       (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
	       (suppress-delay
		(let ((deadline (and org-agenda-skip-scheduled-delay-if-deadline
				     (org-entry-get nil "DEADLINE"))))
		  (cond
		   ((not deadline) nil)
		   ;; The current item has a deadline date, so
		   ;; evaluate its delay time.
		   ((integerp org-agenda-skip-scheduled-delay-if-deadline)
		    ;; Use global delay time.
		    (- org-agenda-skip-scheduled-delay-if-deadline))
		   ((eq org-agenda-skip-scheduled-delay-if-deadline
			'post-deadline)
		    ;; Set delay to no later than DEADLINE.
		    (min (- schedule
			    (org-agenda--timestamp-to-absolute deadline))
			 org-scheduled-delay-days))
		   (t 0))))
	       (ddays
		(cond
		 ;; Nullify delay when a repeater triggered already
		 ;; and the delay is of the form --Xd.
		 ((and (string-match-p "--[0-9]+[hdwmy]" s)
		       (> schedule (org-agenda--timestamp-to-absolute s)))
		  0)
		 (suppress-delay
		  (let ((org-scheduled-delay-days suppress-delay))
		    (org-get-wdays s t t)))
		 (t (org-get-wdays s t)))))
	  ;; Display scheduled items at base date (SCHEDULE), today if
	  ;; scheduled before the current date, and at any repeat past
	  ;; today.  However, skip delayed items and items that have
	  ;; been displayed for more than `org-scheduled-past-days'.
	  (unless (and todayp
		       habitp
		       (bound-and-true-p org-habit-show-all-today))
	    (when (or (and (> ddays 0) (< diff ddays))
		      (> diff (or (and habitp org-habit-scheduled-past-days)
				  org-scheduled-past-days))
		      (> schedule current)
		      (and (/= current schedule)
			   (/= current today)
			   (/= current repeat)))
	      (throw :skip nil)))
	  ;; Possibly skip done tasks.
	  (when (and donep
		     (or org-agenda-skip-scheduled-if-done
			 (/= schedule current)))
	    (throw :skip nil))
	  ;; Skip entry if it already appears as a deadline, per
	  ;; `org-agenda-skip-scheduled-if-deadline-is-shown'.  This
	  ;; doesn't apply to habits.
	  (when (pcase org-agenda-skip-scheduled-if-deadline-is-shown
		  ((guard
		    (or (not (memq (line-beginning-position 0) deadline-pos))
			habitp))
		   nil)
		  (`repeated-after-deadline
		   (let ((deadline (time-to-days
				    (org-get-deadline-time (point)))))
		     (and (<= schedule deadline) (> current deadline))))
		  (`not-today pastschedp)
		  (`t t)
		  (_ nil))
	    (throw :skip nil))
	  ;; Skip habits if `org-habit-show-habits' is nil, or if we
	  ;; only show them for today.  Also skip done habits.
	  (when (and habitp
		     (or donep
			 (not (bound-and-true-p org-habit-show-habits))
			 (and (not todayp)
			      (bound-and-true-p
			       org-habit-show-habits-only-for-today))))
	    (throw :skip nil))
	  (save-excursion
	    (re-search-backward "^\\*+[ \t]+" nil t)
	    (goto-char (match-end 0))
	    (let* ((category (org-get-category))
		   (habit (and habitp (org-habit-parse-todo)))
		   (priority (if habit (org-habit-get-priority habit)
			       (+99 diff (org-get-priority))))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (listp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags nil (not inherited-tags)))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (buffer-substring (point) (line-end-position)))
		   (time
		    (cond
		     ;; No time of day designation if it is only a
		     ;; reminder, except for habits, which always show
		     ;; the time of day.  Habits are an exception
		     ;; because if there is a time of day, that is
		     ;; interpreted to mean they should usually happen
		     ;; then, even if doing the habit was missed.
		     ((and
		       (not habitp)
		       (/= current schedule)
		       (/= current repeat))
		      nil)
		     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		      (concat (substring s (match-beginning 1)) " "))
		     (t 'time)))
		   (item
		    (org-agenda-format-item
		     (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
		       ;; Show a reminder of a past scheduled today.
		       (if (and todayp pastschedp)
			   (format past diff)
			 first))
		     head level category priority tags time nil habitp))
		   (face (cond ((and (not habitp) pastschedp)
				'org-scheduled-previously)
			       ((and habitp futureschedp)
				'org-agenda-done)
			       (todayp 'org-scheduled-today)
			       (t 'org-scheduled))))
	      (org-add-props item props
		'undone-face face
		'face (if donep 'org-agenda-done face)
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		'type (if pastschedp "past-scheduled" "scheduled")
		'date (if pastschedp schedule date)
		'ts-date schedule
		'warntime warntime
		'level level
		'priority priority
		'org-habit-p habitp
		'todo-state todo-state)
	      (push item scheduled-items))))))
    (nreverse scheduled-items)))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (let* ((props (list 'face nil
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-tr-regexp)
	 (d0 (calendar-absolute-from-gregorian date))
	 marker hdmarker ee txt d1 d2 s1 s2 category
	 level priority todo-state tags pos head donep inherited-tags)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq pos (point))
	(let ((start-time (match-string 1))
	      (end-time (match-string 2)))
	  (setq s1 (match-string 1)
		s2 (match-string 2)
		d1 (time-to-days
		    (condition-case err
			(org-time-string-to-time s1)
		      (error
		       (error
			"Bad timestamp %S at %d in buffer %S\nError was: %s"
			s1
			pos
			(current-buffer)
			(error-message-string err)))))
		d2 (time-to-days
		    (condition-case err
			(org-time-string-to-time s2)
		      (error
		       (error
			"Bad timestamp %S at %d in buffer %S\nError was: %s"
			s2
			pos
			(current-buffer)
			(error-message-string err))))))
	  (when (and (> (- d0 d1) -1) (> (- d2 d0) -1))
	    ;; Only allow days between the limits, because the normal
	    ;; date stamps will catch the limits.
	    (save-excursion
	      (setq todo-state (org-get-todo-state))
	      (setq donep (member todo-state org-done-keywords))
	      (when (and donep org-agenda-skip-timestamp-if-done)
		(throw :skip t))
	      (setq marker (org-agenda-new-marker (point))
		    category (org-get-category))
	      (if (not (re-search-backward org-outline-regexp-bol nil t))
		  (throw :skip nil)
		(goto-char (match-beginning 0))
		(setq hdmarker (org-agenda-new-marker (point))
		      priority (org-get-priority)
		      inherited-tags
		      (or (eq org-agenda-show-inherited-tags 'always)
			  (and (listp org-agenda-show-inherited-tags)
			       (memq 'agenda org-agenda-show-inherited-tags))
			  (and (eq org-agenda-show-inherited-tags t)
			       (or (eq org-agenda-use-tag-inheritance t)
				   (memq 'agenda org-agenda-use-tag-inheritance))))
		      priority (org-get-priority)
		      tags (org-get-tags nil (not inherited-tags)))
		(setq level (make-string (org-reduced-level (org-outline-level)) ? ))
		(looking-at "\\*+[ \t]+\\(.*\\)")
		(setq head (match-string 1))
		(let ((remove-re
		       (if org-agenda-remove-timeranges-from-blocks
			   (concat
			    "<" (regexp-quote s1) ".*?>"
			    "--"
			    "<" (regexp-quote s2) ".*?>")
			 nil)))
		  (setq txt (org-agenda-format-item
			     (format
			      (nth (if (= d1 d2) 0 1)
				   org-agenda-timerange-leaders)
			      (1+ (- d0 d1)) (1+ (- d2 d1)))
			     head level category priority tags
			     (save-match-data
			       (let ((hhmm1 (and (string-match org-ts-regexp1 s1)
						 (match-string 6 s1)))
				     (hhmm2 (and (string-match org-ts-regexp1 s2)
						 (match-string 6 s2))))
				 (cond ((string= hhmm1 hhmm2)
					(concat "<" start-time ">--<" end-time ">"))
				       ((and (= d1 d0) (= d2 d0))
					(concat "<" start-time ">--<" end-time ">"))
                                       ((= d1 d0)
					(concat "<" start-time ">"))
				       ((= d2 d0)
					(concat "<" end-time ">")))))
			     remove-re))))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker hdmarker
		'type "block" 'date date
		'level level
		'todo-state todo-state
		'priority priority)
	      (push txt ee))))
	(goto-char pos)))
    ;; Sort the entries by expiration date.
    (nreverse ee)))

(defun org-agenda-format-item (extra txt
				     &optional level category priority tags
				     dotime remove-re habitp)
  "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
LEVEL may be a string to replace the `%l' specifier.
CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer
  (let* ((bindings (car org-prefix-format-compiled))
	 (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      ;; Fix the priority part in txt
      (setq txt (org-agenda-fix-displayed-priority txt priority))

      ;; Fix the tags part in txt
      (setq txt (org-agenda-fix-displayed-tags
		 txt tags
		 org-agenda-show-inherited-tags
		 org-agenda-hide-tags-regexp))

      (let* ((category (or category
			   (if buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name))
			     "")))
	     (category-icon (org-agenda-get-category-icon category))
	     (category-icon (if category-icon
				(propertize " " 'display category-icon)
			      ""))
	     (effort (and (not (string= txt ""))
			  (get-text-property 1 'effort txt)))
	     ;; time, tag, effort are needed for the eval of the prefix format
	     (tag (if tags (nth (1- (length tags)) tags) ""))
	     (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
	     time
	     (ts (when dotime (concat
			       (if (stringp dotime) dotime "")
			       (and org-agenda-search-headline-for-time txt))))
	     (time-of-day (and dotime (org-get-time-of-day ts)))
	     stamp plain s0 s1 s2 rtn srp l
	     duration breadcrumbs)
	(and (derived-mode-p 'org-mode) buffer-file-name
	     (add-to-list 'org-agenda-contributing-files buffer-file-name))
	(when (and dotime time-of-day)
	  ;; Extract starting and ending time and move them to prefix
	  (when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		    (setq plain (string-match org-plain-time-of-day-regexp ts)))
	    (setq s0 (match-string 0 ts)
		  srp (and stamp (match-end 3))
		  s1 (match-string (if plain 1 2) ts)
		  s2 (match-string (if plain 8 (if srp 4 6)) ts))

	    ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	    ;; them, we might want to remove them there to avoid duplication.
	    ;; The user can turn this off with a variable.
	    (when (and org-prefix-has-time
		       org-agenda-remove-times-when-in-prefix (or stamp plain)
		       (string-match (concat (regexp-quote s0) " *") txt)
		       (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		       (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			   (= (match-beginning 0) 0)
			 t))
	      (setq txt (replace-match "" nil nil txt))))
	  ;; Normalize the time(s) to 24 hour
	  (when s1 (setq s1 (org-get-time-of-day s1 'string t)))
	  (when s2 (setq s2 (org-get-time-of-day s2 'string t)))

	  ;; Try to set s2 if s1 and
	  ;; `org-agenda-default-appointment-duration' are set
	  (when (and s1 (not s2) org-agenda-default-appointment-duration)
	    (setq s2
		  (org-duration-from-minutes
		   (+ (org-duration-to-minutes s1 t)
		      org-agenda-default-appointment-duration)
		   nil t)))

	  ;; Compute the duration
	  (when s2
	    (setq duration (- (org-duration-to-minutes s2)
			      (org-duration-to-minutes s1)))))

	(when (string-match org-tag-group-re txt)
	  ;; Tags are in the string
	  (if (or (eq org-agenda-remove-tags t)
		  (and org-agenda-remove-tags
		       org-prefix-has-tag))
	      (setq txt (replace-match "" t t txt))
	    (setq txt (replace-match
		       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			       (match-string 1 txt))
		       t t txt))))

	(when remove-re
	  (while (string-match remove-re txt)
	    (setq txt (replace-match "" t t txt))))

	;; Set org-heading property on `txt' to mark the start of the
	;; heading.
	(add-text-properties 0 (length txt) '(org-heading t) txt)

	;; Prepare the variables needed in the eval of the compiled format
	(when org-prefix-has-breadcrumbs
	  (setq breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker)
			      (let ((s (org-format-outline-path (org-get-outline-path)
								(1- (frame-width))
								nil org-agenda-breadcrumbs-separator)))
				(if (eq "" s) "" (concat s org-agenda-breadcrumbs-separator))))))
	(setq time (cond (s2 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      "-" (org-agenda-time-of-day-to-ampm-maybe s2)
			      (when org-agenda-timegrid-use-ampm " ")))
			 (s1 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      (if org-agenda-timegrid-use-ampm
                                  (concat time-grid-trailing-characters " ")
                                time-grid-trailing-characters)))
			 (t ""))
	      extra (or (and (not habitp) extra) "")
	      category (if (symbolp category) (symbol-name category) category)
	      level (or level ""))
	(if (string-match org-link-bracket-re category)
	    (progn
	      (setq l (string-width (or (match-string 2) (match-string 1))))
	      (when (< l (or org-prefix-category-length 0))
		(setq category (copy-sequence category))
		(org-add-props category nil
		  'extra-space (make-string
				(- org-prefix-category-length l 1) ?\ ))))
	  (when (and org-prefix-category-max-length
		     (>= (length category) org-prefix-category-max-length))
	    (setq category (substring category 0 (1- org-prefix-category-max-length)))))
	;; Evaluate the compiled format
	(setq rtn (concat (eval formatter) txt))

	;; And finally add the text properties
	(remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
	(org-add-props rtn nil
	  'org-category category
	  'tags (mapcar 'org-downcase-keep-props tags)
	  'org-priority-highest org-priority-highest
	  'org-priority-lowest org-priority-lowest
	  'time-of-day time-of-day
	  'duration duration
	  'breadcrumbs breadcrumbs
	  'txt txt
	  'level level
	  'time time
	  'extra extra
	  'format org-prefix-format-compiled
	  'dotime dotime)))))

(defun org-agenda-add-time-grid-maybe (list ndays todayp)
  "Add a time-grid for agenda items which need it.

LIST is the list of agenda items formatted by `org-agenda-list'.
NDAYS is the span of the current agenda view.
TODAYP is t when the current agenda view is on today."
  (catch 'exit
    (cond ((not org-agenda-use-time-grid) (throw 'exit list))
	  ((and todayp (member 'today (car org-agenda-time-grid))))
	  ((and (= ndays 1) (member 'daily (car org-agenda-time-grid))))
	  ((member 'weekly (car org-agenda-time-grid)))
	  (t (throw 'exit list)))
    (let* ((have (delq nil (mapcar
			    (lambda (x) (get-text-property 1 'time-of-day x))
			    list)))
	   (string (nth 3 org-agenda-time-grid))
	   (gridtimes (nth 1 org-agenda-time-grid))
	   (req (car org-agenda-time-grid))
	   (remove (member 'remove-match req))
	   new time)
      (when (and (member 'require-timed req) (not have))
	;; don't show empty grid
	(throw 'exit list))
      (while (setq time (pop gridtimes))
	(unless (and remove (member time have))
	  (setq time (replace-regexp-in-string " " "0" (format "%04s" time)))
	  (push (org-agenda-format-item
		 nil string nil "" nil nil
		 (concat (substring time 0 -2) ":" (substring time -2)))
		new)
	  (put-text-property
	   2 (length (car new)) 'face 'org-time-grid (car new))))
      (when (and todayp org-agenda-show-current-time-in-grid)
	(push (org-agenda-format-item
	       nil org-agenda-current-time-string nil "" nil nil
	       (format-time-string "%H:%M "))
	      new)
	(put-text-property
	 2 (length (car new)) 'face 'org-agenda-current-time (car new)))

      (if (member 'time-up org-agenda-sorting-strategy-selected)
	  (append new list)
	(append list new)))))

(defun org-agenda-change-all-lines (newhead hdmarker
					    &optional fixface just-this)
  "Change all lines in the agenda buffer which match HDMARKER.
The new content of the line will be NEWHEAD (as modified by
`org-agenda-format-item').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified according to
the new TODO state.
If JUST-THIS is non-nil, change just the current line, not all.
If FORCE-TAGS is non-nil, the car of it returns the new tags."
  (let* ((inhibit-read-only t)
	 (line (org-current-line))
	 (org-agenda-buffer (current-buffer))
	 (priority (with-current-buffer (marker-buffer hdmarker)
		     (org-with-wide-buffer
		      (goto-char hdmarker)
		      (org-get-priority))))
	 (thetags (with-current-buffer (marker-buffer hdmarker)
		    (org-get-tags hdmarker)))
	 props m pl undone-face done-face finish new dotime level cat tags)
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line 1)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (org-get-at-bol 'org-hd-marker))
		   (or (not just-this) (= (org-current-line) line))
		   (equal m hdmarker))
	  (setq props (text-properties-at (point))
		dotime (org-get-at-bol 'dotime)
		cat (org-agenda-get-category)
		level (org-get-at-bol 'level)
		tags thetags
		new
		(let ((org-prefix-format-compiled
		       (or (get-text-property (min (1- (point-max)) (point)) 'format)
			   org-prefix-format-compiled))
		      (extra (org-get-at-bol 'extra)))
		  (with-current-buffer (marker-buffer hdmarker)
		    (org-with-wide-buffer
		     (org-agenda-format-item extra newhead level cat priority tags dotime))))
		pl (text-property-any (point-at-bol) (point-at-eol) 'org-heading t)
		undone-face (org-get-at-bol 'undone-face)
		done-face (org-get-at-bol 'done-face))
	  (beginning-of-line 1)
	  (cond
	   ((equal new "") (delete-region (point) (line-beginning-position 2)))
	   ((looking-at ".*")
	    ;; When replacing the whole line, preserve bulk mark
	    ;; overlay, if any.
	    (let ((mark (catch :overlay
			  (dolist (o (overlays-in (point) (+ 2 (point))))
			    (when (eq (overlay-get o 'type)
				      'org-marked-entry-overlay)
			      (throw :overlay o))))))
	      (replace-match new t t)
	      (beginning-of-line)
	      (when mark (move-overlay mark (point) (+ 2 (point)))))
	    (add-text-properties (point-at-bol) (point-at-eol) props)
	    (when fixface
	      (add-text-properties
	       (point-at-bol) (point-at-eol)
	       (list 'face
		     (if org-last-todo-state-is-todo
			 undone-face done-face))))
	    (org-agenda-highlight-todo 'line)
	    (beginning-of-line 1))
	   (t (error "Line update did not work")))
	  (save-restriction
	    (narrow-to-region (point-at-bol) (point-at-eol))
	    (org-agenda-finalize)))
	(beginning-of-line 0)))))

(defun org-agenda-add-entry-to-org-agenda-diary-file (type text &optional d1 d2)
  "Add a diary entry with TYPE to `org-agenda-diary-file'.
If TEXT is not empty, it will become the headline of the new entry, and
the resulting entry will not be shown.  When TEXT is empty, switch to
`org-agenda-diary-file' and let the user finish the entry there."
  (let ((cw (current-window-configuration)))
    (org-switch-to-buffer-other-window
     (find-file-noselect org-agenda-diary-file))
    (widen)
    (goto-char (point-min))
    (cl-case type
      (anniversary
       (or (re-search-forward "^\\*[ \t]+Anniversaries" nil t)
	   (progn
	     (or (org-at-heading-p t)
		 (progn
		   (outline-next-heading)
		   (insert "* Anniversaries\n\n")
		   (beginning-of-line -1)))))
       (outline-next-heading)
       (org-back-over-empty-lines)
       (backward-char 1)
       (insert "\n")
       (insert (format "%%%%(org-anniversary %d %2d %2d) %s"
		       (nth 2 d1) (car d1) (nth 1 d1) text)))
      (day
       (let ((org-prefix-has-time t)
	     (org-agenda-time-leading-zero t)
	     fmt time time2)
	 (when org-agenda-insert-diary-extract-time
	   ;; Use org-agenda-format-item to parse text for a time-range and
	   ;; remove it.  FIXME: This is a hack, we should refactor
	   ;; that function to make time extraction available separately
	   (setq fmt (org-agenda-format-item nil text nil nil nil nil t)
		 time (get-text-property 0 'time fmt)
		 time2 (if (> (length time) 0)
			   ;; split-string removes trailing ...... if
			   ;; no end time given.  First space
			   ;; separates time from date.
			   (concat " " (car (split-string time "\\.")))
			 nil)
		 text (get-text-property 0 'txt fmt)))
	 (if (eq org-agenda-insert-diary-strategy 'top-level)
	     (org-agenda-insert-diary-as-top-level text)
	   (require 'org-datetree)
	   (org-datetree-find-date-create d1)
	   (org-agenda-insert-diary-make-new-entry text))
	 (org-insert-time-stamp (org-time-from-absolute
				 (calendar-absolute-from-gregorian d1))
				nil nil nil nil time2))
       (end-of-line 0))
      ((block) ;; Wrap this in (strictly unnecessary) parens because
       ;; otherwise the indentation gets confused by the
       ;; special meaning of 'block
       (when (> (calendar-absolute-from-gregorian d1)
		(calendar-absolute-from-gregorian d2))
	 (setq d1 (prog1 d2 (setq d2 d1))))
       (if (eq org-agenda-insert-diary-strategy 'top-level)
	   (org-agenda-insert-diary-as-top-level text)
	 (require 'org-datetree)
	 (org-datetree-find-date-create d1)
	 (org-agenda-insert-diary-make-new-entry text))
       (org-insert-time-stamp (org-time-from-absolute
			       (calendar-absolute-from-gregorian d1)))
       (insert "--")
       (org-insert-time-stamp (org-time-from-absolute
			       (calendar-absolute-from-gregorian d2)))
       (end-of-line 0)))
    (if (string-match "\\S-" text)
	(progn
	  (set-window-configuration cw)
	  (message "%s entry added to %s"
		   (capitalize (symbol-name type))
		   (abbreviate-file-name org-agenda-diary-file)))
      (org-reveal t)
      (message "Please finish entry here"))))

(defun org-agenda-fix-displayed-priority (txt priority)
  "Modifies TXT to show correct PRIORITY.
Respects `org-use-priority-inheritance' by adding PRIORITY if not
already present. No change is made if `org-get-priority-function'
is non-nil since TXT may be using non-standard priority cookies."
  (when (and priority
	     org-use-priority-inheritance
	     (not (functionp org-get-priority-function))
	     (not (string-match org-priority-regexp txt)))
    (let ((priority-str
	   (char-to-string (org-priority-integer-to-char priority))))
      (setq txt (concat "[#" priority-str "] " txt))))
  txt)
