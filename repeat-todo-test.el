(require 'repeat-todo)
(require 'ert)

(defvar repeat-todo--test-weekdays '(1 2 3 4 5))
(defvar repeat-todo--test-weekends '(6 7))

(defvar repeat-todo--test-monday (date-to-time "2000-01-03T08:00:00-0700"))
(defvar repeat-todo--test-tuesday (date-to-time "2000-01-04T08:00:00-0700"))
(defvar repeat-todo--test-wednesday (date-to-time "2000-01-05T08:00:00-0700"))
(defvar repeat-todo--test-thursday (date-to-time "2000-01-06T08:00:00-0700"))
(defvar repeat-todo--test-friday (date-to-time "2000-01-07T08:00:00-0700"))
(defvar repeat-todo--test-saturday (date-to-time "2000-01-08T08:00:00-0700"))
(defvar repeat-todo--test-sunday (date-to-time "2000-01-09T08:00:00-0700"))
(defvar repeat-todo--test-next-monday (date-to-time "2000-01-10T08:00:00-0700"))
(defvar repeat-todo--test-next-tuesday
  (date-to-time "2000-01-11T08:00:00-0700"))
(defvar repeat-todo--test-next-wednesday
  (date-to-time "2000-01-12T08:00:00-0700"))
(defvar repeat-todo--test-next-thursday
  (date-to-time "2000-01-13T08:00:00-0700"))
(defvar repeat-todo--test-next-friday (date-to-time "2000-01-14T08:00:00-0700"))
(defvar repeat-todo--test-next-saturday
  (date-to-time "2000-01-15T08:00:00-0700"))
(defvar repeat-todo--test-next-sunday (date-to-time "2000-01-16T08:00:00-0700"))

(defvar repeat-todo--current-time-override-time nil)

(defun repeat-todo--current-time-override ()
  repeat-todo--current-time-override-time)

(defmacro with-current-time (new-current-time &rest body)
  "Run body with `current-time' returning NEW-CURRENT-TIME."
  `(letf
       (((symbol-function 'current-time) (lambda () ,new-current-time))) ,@body))

(defmacro time-string= (time-a time-b)
  "Compare human-readable TIME-A and TIME-B."
  `(string=
    (format-time-string "%FT%T%z" ,time-a)
    (format-time-string "%FT%T%z" ,time-b)))

(ert-deftest repeat-todo--next-scheduled-time--weekdays ()
  ;; Weekdays
  (should
   (time-string=
    (with-current-time repeat-todo--test-monday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-monday repeat-todo--test-weekdays))
    repeat-todo--test-tuesday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-tuesday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-tuesday repeat-todo--test-weekdays))
    repeat-todo--test-wednesday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-wednesday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-wednesday repeat-todo--test-weekdays))
    repeat-todo--test-thursday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-thursday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-thursday repeat-todo--test-weekdays))
    repeat-todo--test-friday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-friday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-friday repeat-todo--test-weekdays))
    repeat-todo--test-next-monday)))

(ert-deftest repeat-todo--next-scheduled-time--weekends ()
  (should
   (time-string=
    (with-current-time repeat-todo--test-saturday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-saturday repeat-todo--test-weekends))
    repeat-todo--test-sunday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-sunday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-sunday repeat-todo--test-weekends))
    repeat-todo--test-next-saturday)))

(ert-deftest repeat-todo--next-scheduled-time--custom-numbers ()
  (should
   (time-string=
    (with-current-time repeat-todo--test-monday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-monday '(1 3 5)))
    repeat-todo--test-wednesday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-wednesday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-wednesday '(1 3 5)))
    repeat-todo--test-friday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-monday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-monday '(7)))
    repeat-todo--test-sunday))
  (should
   (time-string=
    (with-current-time repeat-todo--test-sunday
                       (repeat-todo--next-scheduled-time
                        repeat-todo--test-sunday '(7)))
    repeat-todo--test-next-sunday)))

(ert-deftest repeat-todo--parse-property--weekdays ()
  (should
   (equal (repeat-todo--parse-property "weekday") repeat-todo--test-weekdays))
  (should
   (equal (repeat-todo--parse-property "weekdays") repeat-todo--test-weekdays)))

(ert-deftest repeat-todo--parse-property--weekends ()
  (should
   (equal (repeat-todo--parse-property "weekend") repeat-todo--test-weekends))
  (should
   (equal (repeat-todo--parse-property "weekends") repeat-todo--test-weekends)))

(ert-deftest repeat-todo--parse-property--numbers ()
  (should (equal (repeat-todo--parse-property "1 3 5") '(1 3 5)))
  (should (equal (repeat-todo--parse-property "7") '(7)))
  (should (equal (repeat-todo--parse-property "0") '(7))))

(ert-deftest repeat-todo--parse-property--name-monday ()
  (should (equal (repeat-todo--parse-property "m") '(1)))
  (should (equal (repeat-todo--parse-property "mon") '(1)))
  (should (equal (repeat-todo--parse-property "monday") '(1)))
  (should (equal (repeat-todo--parse-property "M") '(1)))
  (should (equal (repeat-todo--parse-property "Mon") '(1)))
  (should (equal (repeat-todo--parse-property "Monday") '(1))))

(ert-deftest repeat-todo--parse-property--name-tuesday ()
  (should (equal (repeat-todo--parse-property "t") '(2)))
  (should (equal (repeat-todo--parse-property "tue") '(2)))
  (should (equal (repeat-todo--parse-property "tues") '(2)))
  (should (equal (repeat-todo--parse-property "tuesday") '(2)))
  (should (equal (repeat-todo--parse-property "T") '(2)))
  (should (equal (repeat-todo--parse-property "Tue") '(2)))
  (should (equal (repeat-todo--parse-property "Tues") '(2)))
  (should (equal (repeat-todo--parse-property "Tuesday") '(2))))

(ert-deftest repeat-todo--parse-property--name-wednesday ()
  (should (equal (repeat-todo--parse-property "w") '(3)))
  (should (equal (repeat-todo--parse-property "wed") '(3)))
  (should (equal (repeat-todo--parse-property "wednesday") '(3)))
  (should (equal (repeat-todo--parse-property "W") '(3)))
  (should (equal (repeat-todo--parse-property "Wed") '(3)))
  (should (equal (repeat-todo--parse-property "Wednesday") '(3))))

(ert-deftest repeat-todo--parse-property--name-thursday ()
  (should (equal (repeat-todo--parse-property "r") '(4)))
  (should (equal (repeat-todo--parse-property "R") '(4)))

  (should (equal (repeat-todo--parse-property "thu") '(4)))
  (should (equal (repeat-todo--parse-property "thur") '(4)))
  (should (equal (repeat-todo--parse-property "thurs") '(4)))
  (should (equal (repeat-todo--parse-property "thursday") '(4)))
  (should (equal (repeat-todo--parse-property "Thu") '(4)))
  (should (equal (repeat-todo--parse-property "Thur") '(4)))
  (should (equal (repeat-todo--parse-property "Thurs") '(4)))
  (should (equal (repeat-todo--parse-property "Thursday") '(4))))

(ert-deftest repeat-todo--parse-property--name-friday ()
  (should (equal (repeat-todo--parse-property "f") '(5)))
  (should (equal (repeat-todo--parse-property "fri") '(5)))
  (should (equal (repeat-todo--parse-property "friday") '(5)))
  (should (equal (repeat-todo--parse-property "F") '(5)))
  (should (equal (repeat-todo--parse-property "Fri") '(5)))
  (should (equal (repeat-todo--parse-property "Friday") '(5))))

(ert-deftest repeat-todo--parse-property--name-saturday ()
  (should (equal (repeat-todo--parse-property "sa") '(6)))
  (should (equal (repeat-todo--parse-property "sat") '(6)))
  (should (equal (repeat-todo--parse-property "saturday") '(6)))
  (should (equal (repeat-todo--parse-property "Sa") '(6)))
  (should (equal (repeat-todo--parse-property "Sat") '(6)))
  (should (equal (repeat-todo--parse-property "Saturday") '(6))))

(ert-deftest repeat-todo--parse-property--name-sunday ()
  (should (equal (repeat-todo--parse-property "su") '(7)))
  (should (equal (repeat-todo--parse-property "sun") '(7)))
  (should (equal (repeat-todo--parse-property "sunday") '(7)))
  (should (equal (repeat-todo--parse-property "Su") '(7)))
  (should (equal (repeat-todo--parse-property "Sun") '(7)))
  (should (equal (repeat-todo--parse-property "Sunday") '(7))))

(ert-deftest repeat-todo--parse-property--name ()
  (should
   (equal (repeat-todo--parse-property "m t w r f sa su") '(1 2 3 4 5 6 7)))
  ;; Mixed cases are supported
  (should (equal (repeat-todo--parse-property "M f Sa") '(1 5 6)))
  ;; They don't have to be in order
  (should
   (equal (repeat-todo--parse-property "Sunday Tuesday Monday") '(7 2 1))))
