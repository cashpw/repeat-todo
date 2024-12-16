(require 'repeat-todo)
(require 'ert)

(defvar repeat-todo--test-weekdays '(1 2 3 4 5))
(defvar repeat-todo--test-weekends '(0 6))

(defvar repeat-todo--test-monday (date-to-time "2000-01-03T08:00:00-0700"))
(defvar repeat-todo--test-tuesday (date-to-time "2000-01-04T08:00:00-0700"))
(defvar repeat-todo--test-wednesday (date-to-time "2000-01-05T08:00:00-0700"))
(defvar repeat-todo--test-thursday (date-to-time "2000-01-06T08:00:00-0700"))
(defvar repeat-todo--test-friday (date-to-time "2000-01-07T08:00:00-0700"))
(defvar repeat-todo--test-saturday (date-to-time "2000-01-08T08:00:00-0700"))
(defvar repeat-todo--test-sunday (date-to-time "2000-01-09T08:00:00-0700"))
(defvar repeat-todo--test-next-monday (date-to-time "2000-01-10T08:00:00-0700"))
(defvar repeat-todo--test-next-tuesday (date-to-time "2000-01-11T08:00:00-0700"))
(defvar repeat-todo--test-next-wednesday (date-to-time "2000-01-12T08:00:00-0700"))
(defvar repeat-todo--test-next-thursday (date-to-time "2000-01-13T08:00:00-0700"))
(defvar repeat-todo--test-next-friday (date-to-time "2000-01-14T08:00:00-0700"))
(defvar repeat-todo--test-next-saturday (date-to-time "2000-01-15T08:00:00-0700"))
(defvar repeat-todo--test-next-sunday (date-to-time "2000-01-16T08:00:00-0700"))

(defvar repeat-todo--current-time-override-time nil)

(defun repeat-todo--current-time-override ()
  repeat-todo--current-time-override-time)

(defmacro with-current-time (new-current-time &rest body)
  "Run body with `current-time' returning NEW-CURRENT-TIME."
  `(letf (((symbol-function 'current-time)
           (lambda ()
             ,new-current-time)))
     ,@body))

(ert-deftest repeat-todo--next-scheduled-time ()
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-monday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-monday repeat-todo--test-weekdays)))
    (format-time-string "%FT%T%z" repeat-todo--test-tuesday)))
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-tuesday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-tuesday repeat-todo--test-weekdays)))
    (format-time-string "%FT%T%z" repeat-todo--test-wednesday)))
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-wednesday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-wednesday repeat-todo--test-weekdays)))
    (format-time-string "%FT%T%z" repeat-todo--test-thursday)))
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-thursday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-thursday repeat-todo--test-weekdays)))
    (format-time-string "%FT%T%z" repeat-todo--test-friday)))
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-friday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-friday repeat-todo--test-weekdays)))
    (format-time-string "%FT%T%z" repeat-todo--test-next-monday)))

  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-saturday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-saturday repeat-todo--test-weekends)))
    (format-time-string "%FT%T%z" repeat-todo--test-sunday)))
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-sunday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-sunday repeat-todo--test-weekends)))
    (format-time-string "%FT%T%z" repeat-todo--test-next-saturday)))

  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-monday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-monday '(1 3 5))))
    (format-time-string "%FT%T%z" repeat-todo--test-wednesday)))
  (should
   (string=
    (format-time-string
     "%FT%T%z"
     (with-current-time repeat-todo--test-wednesday
                        (repeat-todo--next-scheduled-time
                         repeat-todo--test-wednesday '(1 3 5))))
    (format-time-string "%FT%T%z" repeat-todo--test-friday))))
