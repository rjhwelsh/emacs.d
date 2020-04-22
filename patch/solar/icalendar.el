;;; icalendar.el patches --- Additional/adjusted code for icalendar -*- lexical-binding: t; -*-

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
		      (list cns-cons-or-list))))
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
