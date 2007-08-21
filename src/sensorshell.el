;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sensorshell.el -- 
;; Author          : Philip M. Johnson
;; Created On      : Mon Jul 22 09:21:50 2002
;; Last Modified By: 
;; Last Modified On: Mon Aug 13 15:34:52 2007
;; RCS: $Id: sensorshell.el,v 1.1.1.1 2005/10/20 23:56:57 johnson Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 2002 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; See http://csdl.ics.hawaii.edu/FAQ/Lisp/lisp-naming-conventions.html
;; for info on why these functions are named this way.

(require 'cl) 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Variables
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar hackystat*sensorshell!buffer-name "*hackystat-v8-shell*"
  "The name of the buffer in which the SensorShell process is running.")

(defvar hackystat*sensorshell*start-hooks nil
  "A list of functions to be run after the sensorshell is started up.
Users can use this to send initialization information to the SensorShell such
as FileMetric class path information.")

(defvar hackystat*sensorshell*max-buffer-size 500000
  "The maximum number of characters to allow in the hackystat sensorshell before
erasing the buffer. Can be changed by the user from the default. A nil value means
never erase the buffer.")

(defvar hackystat*sensorshell*file-name "sensorshell.jar"
  "The name of the sensorshell jar file.")

(defvar hackystat*sensorshell*file-path (concat hackystat*user-home ".hackystat/emacs/")
  "The path to the sensorshell jar file. Defaults home/.hackystat/emacs/
Override this value if you want to keep your sensorshell.jar file someplace else..")


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*sensorshell*start ()
  "Sets up the SensorShell subprocess.
Locally binds process-connection-type to nil during process buffer creation to ensure
that pipes rather than PTYs are used for the Hackystat shell process. This appears to
cure a bug in XEmacs that prevents graceful exit."
  (interactive)
  (let ((shell-jar (concat hackystat*sensorshell*file-path hackystat*sensorshell*file-name))
	(process-connection-type nil))
    (setq comint-scroll-to-bottom-on-output t)
    (hackystat*make-comint-in-buffer "hackystat-shell" hackystat*sensorshell!buffer-name
				     "java" nil "-jar" shell-jar "emacs"))
  (process-kill-without-query (get-buffer-process hackystat*sensorshell!buffer-name))
  (run-hooks 'hackystat*sensorshell*start-hooks))


(defun hackystat*sensorshell*shutdown ()
  "Shutdown the SensorShell subprocess."
  (interactive)
  (when (hackystat*sensorshell*alive-p)
    (save-excursion
      (set-buffer hackystat*sensorshell!buffer-name)
      (goto-char (point-max))
      (insert "quit")
      (comint-send-input)
      (let ((dots "."))
	(while (and (hackystat*sensorshell*alive-p)
		    (< (length dots) 20))
	  (sit-for 1)
	  (setq dots (concat dots "."))
	  (message (concat "Shutting down hackystat shell" dots)))))
    (message "Shutting down hackystat shell.. done.")))


(defun hackystat*sensorshell*send-command (command)
  "Sends COMMAND to the sensor shell."
  (when (hackystat*sensorshell*alive-p)
    (save-excursion
      (set-buffer hackystat*sensorshell!buffer-name)
      (goto-char (point-max))
      (insert command)
      (comint-send-input)
      (hackystat*sensorshell!clear-buffer-if-needed))))

(defun hackystat*sensorshell!clear-buffer-if-needed()
  "Clears the sensorshell buffer if hackystat*sensorshell*max-buffer-size
is non-nil and the buffer has exceeded that value. Writes a message to the minibuffer."
  (when hackystat*sensorshell*max-buffer-size
    (save-excursion
      (set-buffer hackystat*sensorshell!buffer-name)
      (when (> (buffer-size) hackystat*sensorshell*max-buffer-size)
	(erase-buffer)
	(message (format "%s %s %s %s" (current-time-string)
			 " Hackystat shell buffer erased (exceeded "
			 hackystat*sensorshell*max-buffer-size
			 " characters)."))))))
	

(defun hackystat*sensorshell*alive-p ()
  "Returns non-nil if the sensor shell process is still alive."
  (comint-check-proc hackystat*sensorshell!buffer-name))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Utility function (XEmacs compatibility)
;;; make-comint-in-buffer is defined in GNU but not XEmacs, so I provide it here.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*make-comint-in-buffer (name buffer program &optional startfile &rest switches)
  "Make a comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-process', or a cons pair of the form (HOST . SERVICE) denoting a TCP
connection to be opened via `open-network-stream'.  If there is already a
running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM."
  (or (fboundp 'start-process)
      (error "Multi-processing is not supported for this system"))
  (setq buffer (get-buffer-create (or buffer (concat "*" name "*"))))
  ;; If no process, or nuked process, crank up a new one and put buffer in
  ;; comint mode.  Otherwise, leave buffer and existing process alone.
  (unless (comint-check-proc buffer)
    (with-current-buffer buffer
      (comint-mode)) ; Install local vars, mode, keymap, ...
    (comint-exec buffer name program startfile switches))
  buffer)

(provide 'sensorshell)





