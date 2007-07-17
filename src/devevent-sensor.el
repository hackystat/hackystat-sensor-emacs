;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; devevent-sensor.el -- 
;; Author          : Philip M. Johnson
;; Created On      : Sun Jun 24 10:53:11 2001
;; Last Modified By: 
;; Last Modified On: Tue Jul 17 11:49:50 2007
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

(defvar hackystat*devevent*display-p nil
  "Global variable controlling whether or not to display devevents in the minibuffer.")

(defvar hackystat*devevent!edit-timer nil
  "The timer that runs once every state-change-interval to check
the active buffer and record a state change activity type (if necessary) and send off the 
data (if necessary).")

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Public API
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*devevent*init ()
  "Initializes the devevent sensor code.
Sends some initialization stuff to the SensorShell which should already be running."
  (hackystat*devevent*msg "Initializing")
  (unless hackystat*devevent!edit-timer
    (setq hackystat*devevent!edit-timer 
	  (run-with-timer (hackystat*props*state-change-interval)
			  (hackystat*props*state-change-interval)
			  #'(lambda ()
			      (hackystat*devevent*statechange))))))


(defun hackystat*devevent*msg (msg)
  "If hackystat*devevent*display-p, then print MSG to the minibuffer."
  (message (concat (substring (current-time-string) 11 19) " DevEvent: " msg)))


(defun hackystat*devevent*add-event (subtype path &optional properties)
  "Adds the passed DevEvent data."
  (when hackystat*devevent*display-p
    (message (concat (substring (current-time-string) 11 19) " DevEvent: " type " " path)))
  (hackystat*sensorshell*send-command
   (concat "add#"
	   "SensorDataType=DevEvent#"
	   "Resource=file://" path "#"
	   "Tool=Emacs#"
	   "Type=Edit#"
	   "Subtype=" subtype
	   (if (null properties) "" (concat "#" properties)))))

(defun hackystat*devevent*statechange ()
  "Runs once every hackystat*devevent*edit-timer-interval seconds and if
the current buffer is associated with a file, sends the file name and the buffer
size to the SensorShell to determine whether a DevEvent edit event should be posted."
  (when (buffer-file-name)
    (hackystat*sensorshell*send-command
     (concat "statechange#"
	     (format "%d" (buffer-size)) "#"
	     "SensorDataType=DevEvent#"
	     "Resource=file://" (buffer-file-name) "#"
	     "Tool=Emacs#"
	     "Type=Edit#"
             "Subtype=StateChange"))))

(provide 'devevent-sensor)







