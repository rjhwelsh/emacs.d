;;; ox-icalendar.el patches --- Additional/adjusted code for ox-icalendar -*- lexical-binding: t; -*-

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

(defun org-icalendar-entry (entry contents info)
  "Transcode ENTRY element into iCalendar format.

ENTRY is either a headline or an inlinetask.  CONTENTS is
ignored.  INFO is a plist used as a communication channel.

This function is called on every headline, the section below
it (minus inlinetasks) being its contents.  It tries to create
VEVENT and VTODO components out of scheduled date, deadline date,
plain timestamps, diary sexps.  It also calls itself on every
inlinetask within the section."
  (unless (org-element-property :footnote-section-p entry)
    (let* ((type (org-element-type entry))
	   ;; Determine contents really associated to the entry.  For
	   ;; a headline, limit them to section, if any.  For an
	   ;; inlinetask, this is every element within the task.
	   (inside
	    (if (eq type 'inlinetask)
		(cons 'org-data (cons nil (org-element-contents entry)))
	      (let ((first (car (org-element-contents entry))))
		(and (eq (org-element-type first) 'section)
		     (cons 'org-data
			   (cons nil (org-element-contents first))))))))
      (concat
       (let ((todo-type (org-element-property :todo-type entry))
	     (uid (or (org-element-property :ID entry) (org-id-new)))
	     (summary (org-icalendar-cleanup-string
		       (or (org-element-property :SUMMARY entry)
			   (org-export-data
			    (org-element-property :title entry) info))))
	     (loc (org-icalendar-cleanup-string
		   (org-export-get-node-property
		    :LOCATION entry
		    (org-property-inherit-p "LOCATION"))))
	     (class (org-icalendar-cleanup-string
		     (org-export-get-node-property
		      :CLASS entry
		      (org-property-inherit-p "CLASS"))))
	     ;; Build description of the entry from associated section
	     ;; (headline) or contents (inlinetask).
	     (desc
	      (org-icalendar-cleanup-string
	       (or (org-element-property :DESCRIPTION entry)
		   (let ((contents (org-export-data inside info)))
		     (cond
		      ((not (org-string-nw-p contents)) nil)
		      ((wholenump org-icalendar-include-body)
		       (let ((contents (org-trim contents)))
			 (substring
			  contents 0 (min (length contents)
					  org-icalendar-include-body))))
		      (org-icalendar-include-body (org-trim contents)))))))
	     (cat (org-icalendar-get-categories entry info))
	     (tz (org-export-get-node-property
		  :TIMEZONE entry
		  (org-property-inherit-p "TIMEZONE"))))
	 (concat
	  ;; Events: Delegate to `org-icalendar--vevent' to generate
	  ;; "VEVENT" component from scheduled, deadline, or any
	  ;; timestamp in the entry.
	  (let ((deadline (org-element-property :deadline entry))
		(use-deadline (plist-get info :icalendar-use-deadline)))
	    (and deadline
		 (pcase todo-type
		   (`todo (or (memq 'event-if-todo-not-done use-deadline)
			      (memq 'event-if-todo use-deadline)))
		   (`done (memq 'event-if-todo use-deadline))
		   (_ (memq 'event-if-not-todo use-deadline)))
		 (org-icalendar--vevent
		  entry deadline (concat "DL-" uid)
		  (concat "DL: " summary) loc desc cat tz class)))
	  (let ((scheduled (org-element-property :scheduled entry))
		(use-scheduled (plist-get info :icalendar-use-scheduled)))
	    (and scheduled
		 (pcase todo-type
		   (`todo (or (memq 'event-if-todo-not-done use-scheduled)
			      (memq 'event-if-todo use-scheduled)))
		   (`done (memq 'event-if-todo use-scheduled))
		   (_ (memq 'event-if-not-todo use-scheduled)))
		 (org-icalendar--vevent
		  entry scheduled (concat "SC-" uid)
		  (concat "S: " summary) loc desc cat tz class)))
	  ;; When collecting plain timestamps from a headline and its
	  ;; title, skip inlinetasks since collection will happen once
	  ;; ENTRY is one of them.
	  ;; --------------------------------(also)--------------
	  ;; Diary-sexp: Collect every diary-sexp element within ENTRY
	  ;; and its title, and transcode them.  If ENTRY is
	  ;; a headline, skip inlinetasks: they will be handled
	  ;; separately.
	  (let ((counter 0))
	    (mapconcat
	     #'identity
	     (org-element-map
		 (cons (org-element-property :title entry)
		       (org-element-contents inside))
		 'timestamp
	       (lambda (ts)
		 (when     ;; Regular timestamp conditions
		     (or (let ((type (org-element-property :type ts)))
			   (cl-case (plist-get info :with-timestamps)
			     (active (memq type '(active active-range)))
			     (inactive (memq type '(inactive inactive-range)))
			     ((t) t)))
			 ;; Diary-sexp conditions
			 (and org-icalendar-include-sexps (memq type '(diary)))
			 )
		   (let ((uid (format "TS%d-%s" (cl-incf counter) uid)))
		     (org-icalendar--vevent
		      entry ts uid summary loc desc cat tz class))))
	       info nil (and (eq type 'headline) 'inlinetask))
	     ""))
	  ;; Task: First check if it is appropriate to export it.  If
	  ;; so, call `org-icalendar--vtodo' to transcode it into
	  ;; a "VTODO" component.
	  (when (and todo-type
		     (cl-case (plist-get info :icalendar-include-todo)
		       (all t)
		       (unblocked
			(and (eq type 'headline)
			     (not (org-icalendar-blocked-headline-p
				   entry info))))
		       ((t) (eq todo-type 'todo))))
	    (org-icalendar--vtodo entry uid summary loc desc cat tz class))
	  ))
       ;; If ENTRY is a headline, call current function on every
       ;; inlinetask within it.  In agenda export, this is independent
       ;; from the mark (or lack thereof) on the entry.
       (when (eq type 'headline)
	 (mapconcat #'identity
		    (org-element-map inside 'inlinetask
		      (lambda (task) (org-icalendar-entry task nil info))
		      info) ""))
       ;; Don't forget components from inner entries.
       contents))))

(defun org-icalendar--vevent
    (entry timestamp uid summary location description categories timezone class)
  "Create a VEVENT component.

       ENTRY is either a headline or an inlinetask element.  TIMESTAMP
       is a timestamp object defining the date-time of the event.  UID
       is the unique identifier for the event.  SUMMARY defines a short
       summary or subject for the event.  LOCATION defines the intended
       venue for the event.  DESCRIPTION provides the complete
       description of the event.  CATEGORIES defines the categories the
       event belongs to.  TIMEZONE specifies a time zone for this event
       only.  CLASS contains the visibility attribute.  Three of them
       (\"PUBLIC\", \"CONFIDENTIAL\", and \"PRIVATE\") are predefined, others
       should be treated as \"PRIVATE\" if they are unknown to the iCalendar server.

				      Return VEVENT component as a string."
  (if (eq (org-element-property :type timestamp) 'diary)
      (let ((counter 0))
	(apply 'concat
	       (mapcar (lambda (ts)
			 (let ((uid (format "DS%d-%s" (cl-incf counter) uid)))
			   (org-icalendar--vevent entry ts uid summary location description categories timezone class)
			   ))
		       (org-timestamp-from-sexp
			(substring (org-element-property :raw-value timestamp) 3 -1))
		       )))
    (org-icalendar-fold-string
     (concat "BEGIN:VEVENT\n"
	     (org-icalendar-dtstamp) "\n"
	     "UID:" uid "\n"
	     (org-icalendar-convert-timestamp timestamp "DTSTART" nil timezone) "\n"
	     (org-icalendar-convert-timestamp timestamp "DTEND" t timezone) "\n"
	     ;; RRULE.
	     (when (org-element-property :repeater-type timestamp)
	       (format "RRULE:FREQ=%s;INTERVAL=%d\n"
		       (cl-case (org-element-property :repeater-unit timestamp)
			 (hour "HOURLY") (day "DAILY") (week "WEEKLY")
			 (month "MONTHLY") (year "YEARLY"))
		       (org-element-property :repeater-value timestamp)))
	     "SUMMARY:" summary "\n"
	     (and (org-string-nw-p location) (format "LOCATION:%s\n" location))
	     (and (org-string-nw-p class) (format "CLASS:%s\n" class))
	     (and (org-string-nw-p description)
		  (format "DESCRIPTION:%s\n" description))
	     "CATEGORIES:" categories "\n"
	     ;; VALARM.
	     (org-icalendar--valarm entry timestamp summary)
	     "END:VEVENT"))))
