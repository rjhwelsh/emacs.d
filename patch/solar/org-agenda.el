;;; org-agenda.el patches --- Additional/adjusted code for org-agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2020 rjhwelsh
;;
;; Author: Roger Welsh <rjhwelsh at gmail dot com>
;; Keywords: org timestamp sexp
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
		   (sexp-time
		    (if sexp?
			(org-time-from-sexp
			 (replace-regexp-in-string "^%%" "" s)
			 (calendar-gregorian-from-absolute current))))
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
		     (sexp-time
		      (concat sexp-time " "))
		     (t 'time)))
		   (item
		    (org-agenda-format-item
		     (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
		       ;; Show a reminder of a past scheduled today.
		       (if (and todayp pastschedp)
			   (format past diff)
			 first))
		     head level category tags time nil habitp))
		   (face (cond ((and (not habitp) pastschedp)
				'org-scheduled-previously)
			       ((and habitp futureschedp)
				'org-agenda-done)
			       (todayp 'org-scheduled-today)
			       (t 'org-scheduled)))
		   (habitp (and habitp (org-habit-parse-todo))))
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
		'priority (if habitp (org-habit-get-priority habitp)
			    (+ 99 diff (org-get-priority item)))
		'org-habit-p habitp
		'todo-state todo-state)
	      (push item scheduled-items))))))
    (nreverse scheduled-items)))


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
	       (sexp-time (if sexp-entry
			      (org-time-from-sexp sexp-entry
						  (calendar-gregorian-from-absolute current))))
	       (time-stamp (if (or repeat sexp-entry)
			       (or (and sexp-time (concat sexp-time " "))
				   (match-string 0))
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
		     head level category tags time-stamp org-ts-regexp habit?)))
	      (org-add-props item props
		'priority (if habit?
			      (org-habit-get-priority (org-habit-parse-todo))
			    (org-get-priority item))
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

