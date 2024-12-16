;;; repeat-todo.el --- Special repeaters for org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: December 16, 2024
;; Modified: December 16, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashweaver/repeat-todo
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defgroup repeat-todo nil
  "Special repeaters for `org-mode' TODOs.")

(defvar repeat-todo-mode nil
  "Non-nil if repeat-todo is enabled.")

(defun repeat-todo-mode-enable ()
  "Enable repeat-todo mode."
  (interactive)
  (setq repeat-todo-mode t)
  (add-hook 'org-after-todo-state-change 'repeat-todo--reschedule))

(defun repeat-todo-mode-disable ()
  "Enable repeat-todo mode."
  (interactive)
  (setq repeat-todo-mode nil)
  (remove-hook 'org-after-todo-state-change 'repeat-todo--reschedule))

(defun repeat-todo-mode-toggle ()
  "Enable repeat-todo mode."
  (interactive)
  (if repeat-todo-mode
      (repeat-todo-mode-disable)
    (repeat-todo-mode-enable)))

(defcustom repeat-todo--property "REPEAT"
  "Property name to indicate how to handle DONE event repeated weekly.

One of:

- \"weekdays\": Mon, Tues, Wed, Thurs, Fri, Sat, Sun

- \"weekends\": Sat, Sun

- One or more of the days of the week;

  For example: (1) \"M\" or \"Mon\" \"Monday\"; (2) \"M T W\"

- One or more of the days of the week as numbers; sunday=0, monday=1, etc:

  For example: \"1 3 5\" for Mon, Wed, Fri "
  :group 'repeat-todo
  :type 'string)

(defun repeat-todo--parse-property (prop-value)
  "Return repeating days for PROP-VALUE as list of day numbers (sun=0, mon=1, etc)."
  (let ((case-fold-search nil))
    (cond
     ((string-match "weekday\\(s\\)?" prop-value)
      '(0 1 2 3 4))
     ((string-match "weekend\\(s\\)?" prop-value)
      '(5 6))
     (t
      (let ((repeat-days '())
            (prop-values (string-split " " prop-value 'omit-nulls)))
        (dolist (value prop-values)
          ;; su, sun, sunday
          ((when (string-match "^su\\(n\\(day\\)?\\)?")
             (push 0 repeat-days)))
          ;; m, mon, monday
          ((string-match "^m\\(on\\(day\\)?\\)?") (push 1 repeat-days))
          ;; t, tue, tues, tuesday
          ((when (string-match "^t\\(ue\\(s\\(day\\)?\\)?")
             (push 2 repeat-days)))
          ;; w, wed, wednesday
          ((when (string-match "^w\\(ed\\(nesday\\)?\\)?")
             (push 3 repeat-days)))
          ;; r, thu, thur, thurs, thursday
          ((when (or (string= "r")
                     (string= "R")
                     (string-match "thu\\(r\\(s\\(day\\)?\\)?\\)?"))
             (push 4 repeat-days)))
          ;; f, fri, friday
          ((when (string-match "^f\\(ri\\(day\\)?\\)?")
             (push 5 repeat-days)))
          ;; sa, sat, saturday
          ((when (string-match "^sa\\(t\\(urday\\)?\\)?")
             (push 6 repeat-days))))
        (reverse repeat-days))))))

(defun repeat-todo--p (pom)
  "Return non-nil if the heading at POM is configured to repeat on weekdays."
  (let ((valid-repeaters '("++1d" ".+1d")))
    (and (-contains-p valid-repeaters (org-get-repeat))
         (org-entry-get pom repeat-todo--property))))

(defun repeat-todo--next-scheduled-time (current-scheduled-time weekdays)
  "Return the next valid, by WEEKDAYS, time after CURRENT-SCHEDULED-TIME.

WEEKDAYS: See `repeat-todo--weekdays'."
  (when weekdays
    (let ((new-scheduled-time
           (time-add current-scheduled-time (days-to-time 1))))
      (while (not
              (member
               (string-to-number
                (format-time-string "%u" new-scheduled-time))
               weekdays))
        (setq new-scheduled-time
              (time-add new-scheduled-time (days-to-time 1))))
      new-scheduled-time)))

(defun repeat-todo--reschedule (point-or-marker)
  "Reschedule heading at POINT-OR-MARKER to the next appropriate weekday."
  (if (and repeat-todo-mode
           (org-entry-is-done-p)
           (repeat-todo--p point-or-marker))
      (when-let* ((weekdays
                   (repeat-todo--parse-property
                    (or (org-entry-get point-or-marker repeat-todo--property)
                        "")))
                  (scheduled-time (org-get-scheduled-time point-or-marker))
                  (next-scheduled-time
                   ;; Schedule to the day before the next schedule time because
                   ;; it'll get moved forward one day past when we schedule it
                   (time-subtract
                    (repeat-todo--next-scheduled-time
                     scheduled-time weekdays)
                    (days-to-time 1)))
                  (hh-mm (format-time-string "%H:%M" next-scheduled-time))
                  (format-string
                   (if (string= hh-mm "00:00")
                       "%F"
                     "%F %H:%M")))
        (org-schedule
         nil (format-time-string format-string next-scheduled-time)))))

(provide 'repeat-todo)
;;; repeat-todo.el ends here
