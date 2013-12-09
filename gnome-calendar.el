;;; GnomeShell calendar integration
(require 'dbus)

(defvar gsc-gnome-calendar-dbus-object nil)
(defvar gsc-get-items-function nil "function to be called to retrieve items")

(defun gnome-shell-calendar-register-service (function)
  "Register to the GnomeShell calendar service.
FUNCTION is called to fill the Gnome calendar with items."
  (setq gsc-get-items-function function)
  (dbus-register-service :session
			 "org.gnome.Shell.CalendarServer"
			 :replace-existing)
  (setq gsc-gnome-calendar-dbus-object 
	(dbus-register-method :session
			      "org.gnome.Shell.CalendarServer"
			      "/org/gnome/Shell/CalendarServer"
			      "org.gnome.Shell.CalendarServer"
			      "GetEvents"
			      'gsc-select-items)))

(defun gnome-shell-calendar-unregister-service ()
  "Unregister from the DBus service"
  (when gsc-gnome-calendar-dbus-object
    (dbus-unregister-object gsc-gnome-calendar-dbus-object)
    (dbus-unregister-service :session "org.gnome.Shell.CalendarServer")
    (setq gsc-gnome-calendar-dbus-object nil)))

(defun gsc-select-items (since until force-reload)
  (let ((day-since (floor (time-to-number-of-days (seconds-to-time since))))
	(day-until (floor (time-to-number-of-days (seconds-to-time until))))
	(items (funcall gsc-get-items-function))
	selected-items)
    (dolist (item items)
      (let ((day (floor (time-to-number-of-days (cdr item)))))
	(when (and (>= day day-since)
		   (<= day day-until))
	  (add-to-list 'selected-items item))))
    (list :array (gsc-items-to-dbus-entries selected-items))))

(defun gsc-items-to-dbus-entries (items)
  (mapcar (lambda (item)
	    (list :struct
		  ""
		  (car item)
		  ""
		  :boolean (not (gsc-item-has-time-p item))
		  :int64 (floor (time-to-seconds (cdr item)))
		  :int64 (+ 1 (floor (time-to-seconds (cdr item))))
		  (list :array :signature "{sv}")))
	  items))

(defun gsc-item-has-time-p (item)
  (let ((time (decode-time (cdr item))))
    (or (not (= 0 (nth 0 time)))
	(not (= 0 (nth 1 time)))
	(not (= 0 (nth 2 time))))))

(provide 'gnome-shell-calendar)
	    
