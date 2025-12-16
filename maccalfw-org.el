;;; maccalfw-org.el --- Calendar view for Mac Calendars -- Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Al Haji-Ali, Frederick Morlock

;; Author: Frederick Morlock <me@freddy.us>
;; Created: 2025
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (maccalfw "0.2"))
;; Homepage: https://github.com/FrederickGeek8/org-maccalfw
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bridge from Mac Calendar to calfw.

;;; Installation:

;; Here is a minimum sample code:
;; (advice-add 'org-agenda-files :filter-return (lambda (orig) (maccalfw-org-write-file) (push (expand-file-name maccalfw-org-output-file) orig)))


;;; Code:
(require 'org)
(require 'maccalfw)

(defcustom maccalfw-org-output-file "~/.cache/calendar.org"
  "Default file to output \='org-mode' maccalfw events to."
  :type 'file
  :group 'maccalfw-org)

(defcustom maccalfw-org-default-calendars '()
  "List of calendars to fetch by default when writing to \='org-mode' file."
  :type '(repeat string)
  :group 'maccalfw-org)

(defcustom maccalfw-org-default-start-date-offset -7
  "Number of days in the past to fetch date from to write to \='org-mode' file."
  :type 'integer
  :group 'maccalf-org)

(defcustom maccalfw-org-default-end-date-offset 30
  "Number of days in the past to fetch date from to write to \='org-mode' file."
  :type 'integer
  :group 'maccalf-org)


(defun maccalfw-org--get-date-offset (days)
  "Return date as (MM DD YYYY) offset by DAYS from today."
  (let* ((time (time-add (current-time) (days-to-time days)))
         (decoded (decode-time time)))
    (list (nth 4 decoded)    ; month
          (nth 3 decoded)    ; day
          (nth 5 decoded)))) ; year

(defun maccalfw-org--encode-calfw-date-time (date time)
  "Parse calfw DATE and TIME from event and return \='encode-time' encoded value."
  (let* ((date (list (nth 1 date) (nth 0 date) (nth 2 date)))
         (time (list 0 (nth 1 time) (nth 0 time)))
         (date-time (append time date))
         (encoded (encode-time date-time)))
    encoded))

(defun maccalfw-org--encode-start-time (event)
  "Return an Emacs-encoded form of a calfw EVENT start time."
  (maccalfw-org--encode-calfw-date-time (calfw-event-start-date event) (calfw-event-start-time event)))

(defun maccalfw-org--encode-end-time (event)
  "Return an Emacs-encoded form of a calfw EVENT end time."
  (maccalfw-org--encode-calfw-date-time (calfw-event-end-date event) (calfw-event-end-time event)))

(defun maccalfw-org--event-to-org-entry (event &optional level)
  "Create a org entry for calfw EVENT with optional LEVEL."
  (let* ((title (calfw-event-title event))
         (level (if level level 1))
         (header (make-string level ?*)))
    (format "%s %s\n<%s>-<%s>\n"
            header
            title
            (format-time-string
             "%Y-%m-%d %a %H:%M"
             (maccalfw-org--encode-start-time event)
             nil)
            (format-time-string
             "%Y-%m-%d %a %H:%M"
             (maccalfw-org--encode-end-time event)
             nil))))

(defun maccalfw-org--generate-org-for-events (event-seq &optional level)
  "Create multiple org entries for EVENT-SEQ calfw entries with optional LEVEL."
  (mapcar (lambda (event) (maccalfw-org--event-to-org-entry event level)) (cdr event-seq)))

(defun maccalfw-org--generate-org-for-cals (calendars start-date end-date)
  "Create multiple org entries for \='maccalfw' CALENDARS containing events in START-DATE to END-DATE range."
  (mapcar
   (lambda (calendar)
     (let* (
            (id (plist-get calendar :id))
            (name (plist-get calendar :title))
            (events (maccalfw--get-calendar-events id start-date end-date))
            (org-events (maccalfw-org--generate-org-for-events events 2)))
       (format "* %s \n%s" name (string-join org-events "\n"))))
   calendars))

(defun maccalfw-org-for-cal-names (calendar-names start-date end-date)
  "Create multiple org entries for \='maccalf' CALENDAR-NAMES containing events in START-DATE to END-DATE range."
  (maccalfw--load-module)
  (string-join (maccalfw-org--generate-org-for-cals (maccalfw-get-calendars-by-name calendar-names) start-date end-date) "\n\n"))

(defun maccalfw-org-write-file (&optional calendar-names start-date end-date)
  "Write an org file to \='maccalfw-org-output-file' containing entries for events in CALENDAR-NAMES in the START-DATE to END-DATE range."
  (let* ((calendar-names (or calendar-names maccalfw-org-default-calendars))
         (start-date (or start-date (maccalfw-org--get-date-offset maccalfw-org-default-start-date-offset)))
         (end-date (or end-date (maccalfw-org--get-date-offset maccalfw-org-default-end-date-offset)))
         (org-contents (maccalfw-org-for-cal-names calendar-names start-date end-date))
         (org-contents (concat "#    -*- mode: org -*-\n" org-contents)))
    ;; Now write to file
    (write-region org-contents nil (expand-file-name maccalfw-org-output-file) nil 'nomsg)))

(provide 'maccalfw-org)
;;; maccalfw-org.el ends here
