;;; org.el patches --- Additional/adjusted code for org -*- lexical-binding: t; -*-

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

(defun org-time-from-sexp (s d)
  "Convert Org timestamp S, as a sexp-string, into a time string for date D.
Return nil if S is no valid time string found"
  (require 'diary-lib)
  (let* ((result (diary-sexp-entry s "" d))
	 (txt
	  (cond
	   ((and (stringp result)
		 (not (string-blank-p result)))
	    result)
	   ((and (consp result)
		 (stringp (cdr result))
		 (not (string-blank-p (cdr result))))
	    (cdr result))
	   (t nil)))
	 (ts
	  (if txt
	      (format "%04d"
		      (diary-entry-time txt)))))
    (if ts
	(progn
	  (string-match "\\(..\\)\\(..\\)" ts)
	  (concat
	   (match-string 1 ts)
	   ":"
	   (match-string 2 ts))
	  )
      )))


(defun org-timestamp-from-sexp ( sexp &optional start )
  "Convert SEXP to an org timestamp. 
Enumerates sexp for `icalendar-export-sexp-enumeration-days'.

SEXP is a generic diary sexp expression

Optional argument START determines the first day of the enumeration, given
as a time value, in the same format as returned by `current-time'

See `icalendar--convert-sexp-to-ical'"

  (progn
    (require 'icalendar)
    (let ((now (or start (current-time))))
      (delete
       nil
       (mapcar
	(lambda (offset)
	  (let*
	      ((day (decode-time (time-add now
					   (seconds-to-time
					    (* offset 60 60 24)))))
	       (d (nth 3 day))
	       (m (nth 4 day))
	       (y (nth 5 day))
	       (se (diary-sexp-entry sexp "" (list m d y)))
	       (see (cond ((stringp se) se)
			  ((consp se) (cdr se))
			  (t nil))))
	    (cond
	     ((null see)
	      nil)
	     ((stringp see)
	      (let*
		  ((ts (diary-entry-time see))
		   (hhmm (if (>= ts 0) (format "%04d" ts)))
		   (org-ts
		    (if hhmm
			(progn
			  (string-match "\\(..\\)\\(..\\)" hhmm)
			  (let* ((sec 0)
				 (min (string-to-number (match-string 2 hhmm)))
				 (hour (string-to-number (match-string 1 hhmm)))
				 (next (encode-time sec min hour d m y)))
			    (org-timestamp-from-time next t)))
		      (let ((next (encode-time 0 0 0 d m y)))
			(org-timestamp-from-time next)))))
		(let
		    ((p-alist '((:minute-start . :minute-end)
				(:hour-start . :hour-end)
				(:day-start . :day-end)
				(:month-start . :month-end)
				(:year-start . :year-end)
				)))
		  ;; Copy start times over to end times
		  (dolist (p-cell p-alist)
		    (org-element-put-property org-ts
					      (cdr p-cell)
					      (org-element-property
					       (car p-cell) org-ts)))
		  org-ts
		  ))) ;; END OF LET* ((ts (appears to match with (cond
	     ((error "Unsupported Sexp-entry: %s"
		     sexp)))))
	(number-sequence
	 0 (- icalendar-export-sexp-enumeration-days 1))))
      )))
