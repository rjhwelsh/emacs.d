(defun org-diary-to-ical-string (frombuf)
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
  (let* ((tmpdir temporary-file-directory)
				 (tmpfile (make-temp-name
									 (expand-file-name "orgics" tmpdir)))
				 buf rtn b e)
    (with-current-buffer frombuf
      (icalendar-export-region (point-min) (point-max) tmpfile)
      (setq buf (find-buffer-visiting tmpfile))
      (set-buffer buf)
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VEVENT" nil t)
				(setq b (match-beginning 0)))
      (goto-char (point-max))
      (when (re-search-backward "^END:VEVENT" nil t)
				(setq e (match-end 0)))
      (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
    (kill-buffer buf)
    (delete-file tmpfile)
    rtn))

(defun org-icalendar-transcode-diary-sexp (sexp uid summary)
	"Transcode a diary sexp into iCalendar format.
			 SEXP is the diary sexp being transcoded, as a string.  UID is the
			 unique identifier for the entry.  SUMMARY defines a short summary
			 or subject for the event."
	(progn
		(message (format "Sexp transcode: (%s %s %s)" sexp uid summary))
		(when (require 'icalendar nil t)
			(org-element-normalize-string
			 (with-temp-buffer
				 (let ((sexp (if (not (string-match "\\`<%%" sexp)) sexp
											 (concat (substring sexp 1 -1) " " summary))))
					 (message (format "sexp: %s" sexp))
					 (put-text-property 0 1 'uid uid sexp)
					 (insert sexp "\n"))
				 (org-diary-to-ical-string (current-buffer)))))))


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
  (org-icalendar-fold-string
   (if (eq (org-element-property :type timestamp) 'diary)
       (org-icalendar-transcode-diary-sexp
				(org-element-property :raw-value timestamp) uid summary)
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

;; From icalendar.el
(defun icalendar-export-region (min max ical-filename)
  "Export region in diary file to iCalendar format.
All diary entries in the region from MIN to MAX in the current buffer are
converted to iCalendar format.  The result is appended to the file
ICAL-FILENAME.
This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer `*icalendar-errors*'."
  (interactive "r
FExport diary data into iCalendar file: ")
  (let ((result "")
        (start 0)
        (entry-main "")
        (entry-rest "")
				(entry-full "")
        (header "")
        (contents-n-summary)
        (contents)
        (alarm)
        (found-error nil)
        (nonmarker (concat "^" (regexp-quote diary-nonmarking-symbol)
                           "?"))
        (other-elements nil)
        (cns-cons-or-list nil))
    ;; prepare buffer with error messages
    (save-current-buffer
      (set-buffer (get-buffer-create "*icalendar-errors*"))
      (erase-buffer))

    ;; here we go
    (save-excursion
      (goto-char min)
      (while (re-search-forward
              ;; possibly ignore hidden entries beginning with "&"
              (if icalendar-export-hidden-diary-entries
                  "^\\([^ \t\n#].+\\)\\(\\(\n[ \t].*\\)*\\)"
                "^\\([^ \t\n&#].+\\)\\(\\(\n[ \t].*\\)*\\)") max t)
        (setq entry-main (match-string 1))
        (if (match-beginning 2)
            (setq entry-rest (match-string 2))
          (setq entry-rest ""))
				(setq entry-full (concat entry-main entry-rest))

        (condition-case error-val
            (progn
              (setq cns-cons-or-list
                    (icalendar--convert-to-ical nonmarker entry-main))
              (setq other-elements (icalendar--parse-summary-and-rest
																		entry-full))
              (mapc (lambda (contents-n-summary)
                      (setq contents (concat (car contents-n-summary)
                                             "\nSUMMARY:"
                                             (cdr contents-n-summary)))
                      (let ((cla (cdr (assoc 'cla other-elements)))
                            (des (cdr (assoc 'des other-elements)))
                            (loc (cdr (assoc 'loc other-elements)))
                            (org (cdr (assoc 'org other-elements)))
                            (sta (cdr (assoc 'sta other-elements)))
                            (sum (cdr (assoc 'sum other-elements)))
                            (url (cdr (assoc 'url other-elements)))
                            (uid (cdr (assoc 'uid other-elements))))
                        (if cla
                            (setq contents (concat contents "\nCLASS:" cla)))
                        (if des
                            (setq contents (concat contents "\nDESCRIPTION:"
                                                   des)))
                        (if loc
                            (setq contents (concat contents "\nLOCATION:" loc)))
                        (if org
                            (setq contents (concat contents "\nORGANIZER:"
                                                   org)))
                        (if sta
                            (setq contents (concat contents "\nSTATUS:" sta)))
                        ;;(if sum
                        ;;    (setq contents (concat contents "\nSUMMARY:" sum)))
                        (if url
                            (setq contents (concat contents "\nURL:" url)))

                        (setq header (concat "\nBEGIN:VEVENT\nUID:"
                                             (or uid
                                                 (icalendar--create-uid
                                                  entry-full contents))))
                        (setq alarm (icalendar--create-ical-alarm
                                     (cdr contents-n-summary))))
                      (setq result (concat result header contents alarm
                                           "\nEND:VEVENT")))
                    (if (consp (car cns-cons-or-list))
												cns-cons-or-list
											(list cns-cons-or-list)
                      )))
          ;; handle errors
          (error
           (setq found-error t)
           (save-current-buffer
             (set-buffer (get-buffer-create "*icalendar-errors*"))
             (insert (format-message "Error in line %d -- %s: `%s'\n"
                                     (count-lines (point-min) (point))
                                     error-val
                                     entry-main))))))

      ;; we're done, insert everything into the file
      (save-current-buffer
        (let ((coding-system-for-write 'utf-8))
          (set-buffer (find-file ical-filename))
          (goto-char (point-max))
          (insert "BEGIN:VCALENDAR")
          (insert "\nPRODID:-//Emacs//NONSGML icalendar.el//EN")
          (insert "\nVERSION:2.0")
          (insert result)
          (insert "\nEND:VCALENDAR\n")
          ;; save the diary file
          (save-buffer)
          (unless found-error
            (bury-buffer)))))
    found-error))