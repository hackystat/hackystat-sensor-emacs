;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bufftrans-sensor.el -- 
;; Author          : Philip M. Johnson
;; Created On      : Sun Jun 24 10:53:11 2001
;; Last Modified By: 
;; Last Modified On: Mon Jul 16 13:59:28 2007
;; RCS: $Id: bufftrans-sensor.el,v 1.1.1.1 2005/10/20 23:56:57 johnson Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 2003 Philip M. Johnson
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; See http://csdl.ics.hawaii.edu/FAQ/Lisp/lisp-naming-conventions.html
;; for info on why these functions are named this way.

(require 'cl) 

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Variables
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar hackystat*bufftrans!timer nil
  "The timer that runs once every bufftrans-timer-interval to check
the active buffer and record a buffer transition (if necessary) and send off the 
data (if necessary).")

(defvar hackystat*bufftrans*timer-interval 5
  "The interval in seconds between wakeups of the buffer transition timer.")

(defvar hackystat*bufftrans*display-p nil
  "Global variable controlling whether or not to display buffer transitions in the minibuffer.")

(defvar hackystat*bufftrans!last-file-name nil
  "Saves the name of the last file.")

(defvar hackystat*bufftrans!last-file-modified-p nil
  "Indicates whether the buffer corresponding to the last file was found to be modified at any point
during this transition.")

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Public functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*bufftrans*set-timer-interval (new-interval)
  "Resets the interval between wakeups of the bufftrans timer to NEW-INTERVAL.
NEW-INTERVAL should be a number.
Returns the new interval."
  (when hackystat*bufftrans!timer
    (cancel-timer hackystat*bufftrans!timer))
  (setq hackystat*bufftrans*timer-interval new-interval)
  (setq hackystat*bufftrans!timer
	(run-with-timer hackystat*bufftrans*timer-interval
			hackystat*bufftrans*timer-interval
			#'hackystat*bufftrans!check-for-transition))
  new-interval)


(defun hackystat*bufftrans*set-enabled-p (enabled)
  "Enables buffer transition processing if ENABLED is non-nil.
Returns t if enabled, nil otherwise."
  (cond (enabled
	 (hackystat*bufftrans*set-timer-interval hackystat*bufftrans*timer-interval)
	 t)
	(t
	 (when hackystat*bufftrans!timer
	   (cancel-timer hackystat*bufftrans!timer))
	 nil)))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Private functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*bufftrans!check-for-transition ()
  "Runs once every bufftrans timer interval seconds and creates a buffer transition if needed.
If hackystat*bufftrans*display-p is non-nil, then the minibuffer traces the sensor."
  ;; capture the file name so it can't change during processing.
  (when hackystat*bufftrans*display-p
    (message (concat (substring (current-time-string) 11 19) " BuffTrans...")))
  (let ((file-name (buffer-file-name)))
    (when (file-name)
      ;; when same file as last time, then set modified to t if modified. 
      (when (and (equal file-name hackystat*bufftrans!last-file-name)
		 (buffer-modified-p))
	(setq hackystat*bufftrans!last-file-modified-p t))
      ;; when last-file currently nil, set it to the current file name
      (when (equal hackystat*bufftrans!last-file-name nil)
	(setq hackystat*bufftrans!last-file-name file-name))
      ;; when the last file is different from the current one, then record a transition.
      (when (not (equal file-name hackystat*bufftrans!last-file-name))
	(when hackystat*bufftrans*display-p
	  (message (concat (substring (current-time-string) 11 19)
			   " BuffTrans...recording transition to " (file-name-nondirectory file-name))))
	(hackystat*sensorshell*send-command
	 (concat "add#"
		 "SensorDataType=BuffTrans#"
		 "Resource=file://" file-name "#"
		 "Tool=Emacs#"
		 "fromFile=" hackystat*bufftrans!last-file-name "#"
		 "toFile=" file-name "#"
		 "mod=" (if hackystat*bufftrans!last-file-modified-p "true" "false")))
	;; Do a DevEvent here. 
	(hackystat*devevent*add-event "Edit" file-name "BufferTransition")
	(setq hackystat*bufftrans!last-file-name file-name)))))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Start it off at load-time if enabled. 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(hackystat*bufftrans*set-enabled-p 't)

(provide 'bufftrans-sensor)










