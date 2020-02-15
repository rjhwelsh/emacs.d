(defun org-icalendar-transcode-diary-sexp (sexp uid summary)
	"Transcode a diary sexp into iCalendar format.
			 SEXP is the diary sexp being transcoded, as a string.  UID is the
			 unique identifier for the entry.  SUMMARY defines a short summary
			 or subject for the event."
	(message (format "Sexp transcode: (%s %s %s)" sexp uid summary))
	(when (require 'icalendar nil t)
		(org-element-normalize-string
		 (with-temp-buffer
			 (let ((sexp (if (not (string-match "\\`<%%" sexp)) sexp
										 (concat (substring sexp 1 -1) " " summary))))
				 (message (format "sexp: %s" sexp))
				 (put-text-property 0 1 'uid uid sexp)
				 (insert sexp "\n"))
			 (org-diary-to-ical-string (current-buffer))))))


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
					(let ((counter 0))
						(mapconcat
						 #'identity
						 (org-element-map (cons (org-element-property :title entry)
																		(org-element-contents inside))
								 'timestamp
							 (lambda (ts)
								 (when (let ((type (org-element-property :type ts)))
												 (cl-case (plist-get info :with-timestamps)
													 (active (memq type '(active active-range)))
													 (inactive (memq type '(inactive inactive-range)))
													 ((t) t)))
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
					;; Diary-sexp: Collect every diary-sexp element within ENTRY
					;; and its title, and transcode them.  If ENTRY is
					;; a headline, skip inlinetasks: they will be handled
					;; separately.
					(when org-icalendar-include-sexps
						(let ((counter 0))
							(mapconcat #'identity
												 (org-element-map
														 (cons (org-element-property :title entry)
																	 (org-element-contents inside))
														 'timestamp
													 (lambda (ts)
														 (when (let ((type (org-element-property :type ts)))
																		 (cl-case (plist-get info :with-timestamps)
																			 (active (memq type '(diary)))
																			 (inactive (memq type '(diary)))
																			 ((t) t)))
															 (let ((uid (format "DS%d-%s" (cl-incf counter) uid)))
																 ;; (org-icalendar--vevent
																 ;; entry ts uid summary loc desc cat tz class)
																 (message (format "%s" (org-element-property :raw-value ts)))
																 (org-icalendar-transcode-diary-sexp
																	(org-element-property :raw-value ts)
																	uid
																	summary))))
													 info nil (and (eq type 'headline) 'inlinetask))
												 "")))))
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
