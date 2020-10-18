;;; org-habit.el patches --- Additional/adjusted code for org-habit -*- lexical-binding: t; -*-

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

(defsubst org-habit-get-priority (habit &optional moment)
  "Determine the relative priority of a habit.
This must take into account not just urgency, but consistency as well."
  (let ((pri 1000)
	(now (if moment (time-to-days moment) (org-today)))
	(scheduled (org-habit-scheduled habit))
	(deadline (org-habit-deadline habit)))
    ;; add 10 for every day past the scheduled date, and subtract for every
    ;; day before it
    (setq pri (+ pri (* (- now scheduled) 10)))
    ;; add 50 if the deadline is today
    (if (and (/= scheduled deadline)
	     (= now deadline))
	(setq pri (+ pri 50)))
    ;; add 100 for every day beyond the deadline date, and subtract 10 for
    ;; every day before it
    (let ((slip (- now (1- deadline))))
      (if (> slip 0)
	  (setq pri (+ pri (* slip 100)))
	(setq pri (+ pri (* slip 10)))))
    pri))
