;;; org.el patches --- Additional/adjusted code for org-agenda -*- lexical-binding: t; -*-

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

(defcustom org-priority-get-priority-function nil
  "Function to extract the priority from current line.
The line is always a headline.

If this is nil Org computes the priority of the headline from a
priority cookie like [#A]. It returns an integer, increasing by
1000 for each priority level (see
`org-priority-char-to-integer').

The user can set a different function here, which should process
the current line and return one of:

- an integer priority
- nil if current line is not a header or otherwise has no
associated priority
- t if the `org-default-priority' should be used or the priority can be
inherited from its parent

Priority can only be inherited if `org-use-priority-inheritance' is
non-nil."
  :group 'org-priorities
  :version "24.1"
  :type '(choice
	  (const nil)
	  (function)))

(defcustom org-use-priority-inheritance nil
  "Whether headline priority is inherited from its parents.

If non-nil then the first explicit priority found when searching
up the headline tree applies.  Thus a child headline can override
its parent's priority.

When nil, explicit priorities only apply to the headline they are
given on.

Regardless of setting, if no explicit priority is found then the
default priority is used."
  :group 'org-priorities
  :type 'boolean)

(defun org-priority-show ()
  "Show the priority of the current item.
This priority is composed of the main priority given with the [#A] cookies,
and by additional input from the age of a schedules or deadline entry."
  (interactive)
  (let ((pri (if (eq major-mode 'org-agenda-mode)
		 (org-get-at-bol 'priority)
	       (org-get-priority))))
    (message "Priority is %d" (if pri pri -1000))))
(defun org-priority-char-to-integer (character)
  "Convert priority CHARACTER to an integer priority."
  (* 1000 (- org-lowest-priority character)))

(defun org-priority-integer-to-char (integer)
  "Convert priority INTEGER to a character priority."
  (- org-lowest-priority (/ integer 1000)))

(defun org-get-priority (&optional pos local)
  "Get integer priority at POS.
POS defaults to point.  If LOCAL is non-nil priority inheritance
is ignored regardless of the value of
`org-use-priority-inheritance'.  Returns nil if no priority can be
determined at POS."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or pos (point)))
      (beginning-of-line)
      (if (not (looking-at org-heading-regexp))
	  (return nil)
	(save-match-data
	  (cl-loop
	   (if (functionp org-get-priority-function)
	       (let ((priority (funcall org-get-priority-function)))
		 (unless (eq priority t)
		   (return priority)))
	     (when (looking-at org-priority-regexp)
	       (return (org-priority-char-to-integer
			(string-to-char (match-string-no-properties 2))))))
	   (unless (and (not local)
			org-use-priority-inheritance
			(org-up-heading-safe))

	     (return (org-priority-char-to-integer org-default-priority)))))))))


(defun org-scan-tags (action matcher todo-only &optional start-level)
  "Scan headline tags with inheritance and produce output ACTION.

ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
or `agenda' to produce an entry list for an agenda view.  It can also be
a Lisp form or a function that should be called at each matched headline, in
this case the return value is a list of all return values from these calls.

MATCHER is a function accepting three arguments, returning
a non-nil value whenever a given set of tags qualifies a headline
for inclusion.  See `org-make-tags-matcher' for more information.
As a special case, it can also be set to t (respectively nil) in
order to match all (respectively none) headline.

When TODO-ONLY is non-nil, only lines with a TODO keyword are
included in the output.

START-LEVEL can be a string with asterisks, reducing the scope to
headlines matching this string."
  (require 'org-agenda)
  (let* ((re (concat "^"
		     (if start-level
			 ;; Get the correct level to match
			 (concat "\\*\\{" (number-to-string start-level) "\\} ")
		       org-outline-regexp)
		     " *\\(" (regexp-opt org-todo-keywords-1 'words) "\\)?"
		     " *\\(.*?\\)\\([ \t]:\\(?:" org-tag-re ":\\)+\\)?[ \t]*$"))
	 (props (list 'face 'default
		      'done-face 'org-agenda-done
		      'undone-face 'default
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %S"
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 (org-map-continue-from nil)
	 priority
         lspos tags tags-list
	 (tags-alist (list (cons 0 org-file-tags)))
	 (llast 0) rtn rtn1 level category i txt
	 todo marker entry priority
	 ts-date ts-date-type ts-date-pair)
    (unless (or (member action '(agenda sparse-tree)) (functionp action))
      (setq action (list 'lambda nil action)))
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
	(org-overview)
	(org-remove-occur-highlights))
      (while (let (case-fold-search)
	       (re-search-forward re nil t))
	(setq org-map-continue-from nil)
	(catch :skip
	  ;; Ignore closing parts of inline tasks.
	  (when (and (fboundp 'org-inlinetask-end-p) (org-inlinetask-end-p))
	    (throw :skip t))
	  (setq todo (and (match-end 1) (match-string-no-properties 1)))
	  (setq tags (and (match-end 4) (org-trim (match-string-no-properties 4))))
	  (goto-char (setq lspos (match-beginning 0)))
	  (setq level (org-reduced-level (org-outline-level))
		category (org-get-category))
          (when (eq action 'agenda)
            (setq ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
		  ts-date (car ts-date-pair)
		  ts-date-type (cdr ts-date-pair)))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the next tags
	  (when tags
	    (setq tags (org-split-string tags ":")
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr (reverse tags-alist)))
		  tags)
		org-scanner-tags tags-list)
	  (when org-use-tag-inheritance
	    (setcdr (car tags-alist)
		    (mapcar (lambda (x)
			      (setq x (copy-sequence x))
			      (org-add-prop-inherited x))
			    (cdar tags-alist))))
	  (when (and tags org-use-tag-inheritance
		     (or (not (eq t org-use-tag-inheritance))
			 org-tags-exclude-from-inheritance))
	    ;; Selective inheritance, remove uninherited ones.
	    (setcdr (car tags-alist)
		    (org-remove-uninherited-tags (cdar tags-alist))))
	  (when (and

		 ;; eval matcher only when the todo condition is OK
		 (and (or (not todo-only) (member todo org-todo-keywords-1))
		      (if (functionp matcher)
			  (let ((case-fold-search t) (org-trust-scanner-tags t))
			    (funcall matcher todo tags-list level))
			matcher))

		 ;; Call the skipper, but return t if it does not
		 ;; skip, so that the `and' form continues evaluating.
		 (progn
		   (unless (eq action 'sparse-tree) (org-agenda-skip))
		   t)

		 ;; Check if timestamps are deselecting this entry
		 (or (not todo-only)
		     (and (member todo org-todo-keywords-1)
			  (or (not org-agenda-tags-todo-honor-ignore-options)
			      (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item))))))

	    ;; select this headline
	    (cond
	     ((eq action 'sparse-tree)
	      (and org-highlight-sparse-tree-matches
		   (org-get-heading) (match-end 0)
		   (org-highlight-new-match
		    (match-beginning 1) (match-end 1)))
	      (org-show-context 'tags-tree))
	     ((eq action 'agenda)
	      (setq priority (org-get-priority))
	      (setq txt (org-agenda-format-item
			 ""
			 (concat
			  (if (eq org-tags-match-list-sublevels 'indented)
			      (make-string (1- level) ?.) "")
			  (org-get-heading))
			 (make-string level ?\s)
			 category
			 priority
			 tags-list))
	      (goto-char lspos)
	      (setq marker (org-agenda-new-marker))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker marker 'org-category category
		'todo-state todo
                'ts-date ts-date
		'priority priority
                'type (concat "tagsmatch" ts-date-type))
	      (push txt rtn))
	     ((functionp action)
	      (setq org-map-continue-from nil)
	      (save-excursion
		(setq rtn1 (funcall action))
		(push rtn1 rtn)))
	     (t (user-error "Invalid action")))

	    ;; if we are to skip sublevels, jump to end of subtree
	    (unless org-tags-match-list-sublevels
	      (org-end-of-subtree t)
	      (backward-char 1))))
	;; Get the correct position from where to continue
	(if org-map-continue-from
	    (goto-char org-map-continue-from)
	  (and (= (point) lspos) (end-of-line 1)))))
    (when (and (eq action 'sparse-tree)
	       (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))

(defun org-entry-properties (&optional pom which)
  "Get all properties of the current entry.

When POM is a buffer position, get all properties from the entry
there instead.

This includes the TODO keyword, the tags, time strings for
deadline, scheduled, and clocking, and any additional properties
defined in the entry.

If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass.  If WHICH is
a string, only get that property.

Return value is an alist.  Keys are properties, as upcased
strings."
  (org-with-point-at pom
    (when (and (derived-mode-p 'org-mode)
	       (org-back-to-heading-or-point-min t))
      (catch 'exit
	(let* ((beg (point))
	       (specific (and (stringp which) (upcase which)))
	       (which (cond ((not specific) which)
			    ((member specific org-special-properties) 'special)
			    (t 'standard)))
	       props)
	  ;; Get the special properties, like TODO and TAGS.
	  (when (memq which '(nil all special))
	    (when (or (not specific) (string= specific "CLOCKSUM"))
	      (let ((clocksum (get-text-property (point) :org-clock-minutes)))
		(when clocksum
		  (push (cons "CLOCKSUM" (org-duration-from-minutes clocksum))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "CLOCKSUM_T"))
	      (let ((clocksumt (get-text-property (point)
						  :org-clock-minutes-today)))
		(when clocksumt
		  (push (cons "CLOCKSUM_T"
			      (org-duration-from-minutes clocksumt))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ITEM"))
	      (let ((case-fold-search nil))
		(when (looking-at org-complex-heading-regexp)
		  (push (cons "ITEM"
			      (let ((title (match-string-no-properties 4)))
				(if (org-string-nw-p title)
				    (org-remove-tabs title)
				  "")))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TODO"))
	      (let ((case-fold-search nil))
		(when (and (looking-at org-todo-line-regexp) (match-end 2))
		  (push (cons "TODO" (match-string-no-properties 2)) props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "PRIORITY"))
	      (push (cons "PRIORITY"
			  (char-to-string
			   (org-priority-integer-to-char (org-get-priority))))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "FILE"))
	      (push (cons "FILE" (buffer-file-name (buffer-base-buffer)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TAGS"))
	      (let ((tags (org-get-tags nil t)))
		(when tags
		  (push (cons "TAGS" (org-make-tag-string tags))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ALLTAGS"))
	      (let ((tags (org-get-tags)))
		(when tags
		  (push (cons "ALLTAGS" (org-make-tag-string tags))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "BLOCKED"))
	      (push (cons "BLOCKED" (if (org-entry-blocked-p) "t" "")) props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("CLOSED" "DEADLINE" "SCHEDULED")))
	      (forward-line)
	      (when (looking-at-p org-planning-line-re)
		(end-of-line)
		(let ((bol (line-beginning-position))
		      ;; Backward compatibility: time keywords used to
		      ;; be configurable (before 8.3).  Make sure we
		      ;; get the correct keyword.
		      (key-assoc `(("CLOSED" . ,org-closed-string)
				   ("DEADLINE" . ,org-deadline-string)
				   ("SCHEDULED" . ,org-scheduled-string))))
		  (dolist (pair (if specific (list (assoc specific key-assoc))
				  key-assoc))
		    (save-excursion
		      (when (search-backward (cdr pair) bol t)
			(goto-char (match-end 0))
			(skip-chars-forward " \t")
			(and (looking-at org-ts-regexp-both)
			     (push (cons (car pair)
					 (match-string-no-properties 0))
				   props)))))))
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("TIMESTAMP" "TIMESTAMP_IA")))
	      (let ((find-ts
		     (lambda (end ts)
		       ;; Fix next time-stamp before END.  TS is the
		       ;; list of time-stamps found so far.
		       (let ((ts ts)
			     (regexp (cond
				      ((string= specific "TIMESTAMP")
				       org-ts-regexp)
				      ((string= specific "TIMESTAMP_IA")
				       org-ts-regexp-inactive)
				      ((assoc "TIMESTAMP_IA" ts)
				       org-ts-regexp)
				      ((assoc "TIMESTAMP" ts)
				       org-ts-regexp-inactive)
				      (t org-ts-regexp-both))))
			 (catch 'next
			   (while (re-search-forward regexp end t)
			     (backward-char)
			     (let ((object (org-element-context)))
			       ;; Accept to match timestamps in node
			       ;; properties, too.
			       (when (memq (org-element-type object)
					   '(node-property timestamp))
				 (let ((type
					(org-element-property :type object)))
				   (cond
				    ((and (memq type '(active active-range))
					  (not (equal specific "TIMESTAMP_IA")))
				     (unless (assoc "TIMESTAMP" ts)
				       (push (cons "TIMESTAMP"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))
				    ((and (memq type '(inactive inactive-range))
					  (not (string= specific "TIMESTAMP")))
				     (unless (assoc "TIMESTAMP_IA" ts)
				       (push (cons "TIMESTAMP_IA"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))))
				 ;; Both timestamp types are found,
				 ;; move to next part.
				 (when (= (length ts) 2) (throw 'next ts)))))
			   ts)))))
		(goto-char beg)
		;; First look for timestamps within headline.
		(let ((ts (funcall find-ts (line-end-position) nil)))
		  (if (= (length ts) 2) (setq props (nconc ts props))
		    ;; Then find timestamps in the section, skipping
		    ;; planning line.
		    (let ((end (save-excursion (outline-next-heading))))
		      (forward-line)
		      (when (looking-at-p org-planning-line-re) (forward-line))
		      (setq props (nconc (funcall find-ts end ts) props))))))))
	  ;; Get the standard properties, like :PROP:.
	  (when (memq which '(nil all standard))
	    ;; If we are looking after a specific property, delegate
	    ;; to `org-entry-get', which is faster.  However, make an
	    ;; exception for "CATEGORY", since it can be also set
	    ;; through keywords (i.e. #+CATEGORY).
	    (if (and specific (not (equal specific "CATEGORY")))
		(let ((value (org-entry-get beg specific nil t)))
		  (throw 'exit (and value (list (cons specific value)))))
	      (let ((range (org-get-property-block beg)))
		(when range
		  (let ((end (cdr range)) seen-base)
		    (goto-char (car range))
		    ;; Unlike to `org--update-property-plist', we
		    ;; handle the case where base values is found
		    ;; after its extension.  We also forbid standard
		    ;; properties to be named as special properties.
		    (while (re-search-forward org-property-re end t)
		      (let* ((key (upcase (match-string-no-properties 2)))
			     (extendp (string-match-p "\\+\\'" key))
			     (key-base (if extendp (substring key 0 -1) key))
			     (value (match-string-no-properties 3)))
			(cond
			 ((member-ignore-case key-base org-special-properties))
			 (extendp
			  (setq props
				(org--update-property-plist key value props)))
			 ((member key seen-base))
			 (t (push key seen-base)
			    (let ((p (assoc-string key props t)))
			      (if p (setcdr p (concat value " " (cdr p)))
				(push (cons key value) props))))))))))))
	  (unless (assoc "CATEGORY" props)
	    (push (cons "CATEGORY" (org-get-category beg)) props)
	    (when (string= specific "CATEGORY") (throw 'exit props)))
	  ;; Return value.
	  props)))))
