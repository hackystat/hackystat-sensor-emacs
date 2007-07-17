;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sensor-properties.el -- 
;; Author          : Philip Johnson
;; Created On      : Sun Jun 24 10:53:11 2001
;; Last Modified By: 
;; Last Modified On: Mon Jul 16 12:59:38 2007
;; RCS: $Id: sensor-properties.el,v 1.1.1.1 2005/10/20 23:56:57 johnson Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 2001 Philip Johnson
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; See http://csdl.ics.hawaii.edu/FAQ/Lisp/lisp-naming-conventions.html
;; for info on why these functions are named this way.

(require 'cl) 

;; This file will not load correctly if hackystat*user-home is not set.

;; Functions to read in and parse the sensor.properties file.
;;
;; (hackystat*props*make <home directory>) ;; initializes the system.
;; (hackystat*props*enabled-p)             ;; if the Emacs sensor is turned on
;; (hackystat*props*file-available-p)      ;; if sensor.properties is found
;; (hackystat*props*host)                  ;; hackystat host
;; (hackystat*props*user-email)            ;; user email
;; (hackystat*props*state-change-interval) ;; interval between wakeups to check state.

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Variables
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar hackystat*props!file ""
  "The file in which the sensor properties should be found.")

(defvar hackystat*props!table (make-hash-table :test #'equal)
  "A table storing the key value pairs read from the sensor properties file.")

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Constructor
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*props*make (user-home-directory) 
  "Enables access to sensor properties values."
  (setq hackystat*props!table (make-hash-table :test #'equal))
  (setq hackystat*props!file (concat user-home-directory ".hackystat/v8.sensor.properties"))

  (cond ((file-readable-p hackystat*props!file)
         (save-excursion
           (find-file hackystat*props!file)
           (goto-char (point-min))
           (while (> (- (point-max) (point)) 1)
             ;; only process lines that aren't commented.
             (cond ((hackystat*props!noncommentline)
                    (let ((key (hackystat*props!get-key))
                          (value (hackystat*props!get-value)))
                      (if (and key value)
                          (setf (gethash key hackystat*props!table) value))
                      ;;(message (concat "Got: '" key "' and '" value "'"))
                      ;;(sit-for 1)
                      )))
             (forward-line 1))
           (kill-buffer (current-buffer))))

        (t
         (message "Hackystat sensor properties file not found."))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Property Retrieval Functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*props*get(key)
  "Returns the value associated with key, or nil if not found."
  (gethash key hackystat*props!table))

(defun hackystat*props*file-available-p ()
  "Returns non-nil if the properties file could be found."
  (file-readable-p hackystat*props!file))

(defun hackystat*props*host()
  "Returns the hackystat host or http://www.hackystat.org:8080/ if none found."
  (or (hackystat*props!guarantee-suffix (gethash "HACKYSTAT_SENSORBASE_HOST" hackystat*props!table) "/")
      "http://www.hackystat.org:8080/"))

(defun hackystat*props*email()
  "Returns the user email or 'unknownkey' if none found."
  (or (gethash "HACKYSTAT_EMAIL" hackystat*props!table)
      "unknown_email"))

(defun hackystat*props*password()
  "Returns the user email or 'unknownkey' if none found."
  (or (gethash "HACKYSTAT_PASSWORD" hackystat*props!table)
      "unknown_password"))

(defun hackystat*props*state-change-interval ()
  "Returns the state change interval number or 60 (1 minute) if not found."
  ;; string-to-number returns 0
  (let ((interval (gethash "HACKYSTAT_STATE_CHANGE_INTERVAL" hackystat*props!table)))
    (cond (interval
           (let ((num (string-to-number interval)))
             (if (= num 0)
                 60
               num)))
          (t
           60))))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Utility functions to parse the line and extract keys and values
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*props!guarantee-suffix (str suffix)
  "Returns STR with SUFFIX appended if it is not already on STR."
  (if (equal (substring str (- (length str) (length suffix))) suffix)
      str
    (concat str suffix)))


(defun hackystat*props!noncommentline ()
  "Returns non-nil if the point is currently on a line that 
contains a key-value pair. In other words, it is not a comment or empty."
  (let ((line-string (buffer-substring (progn (beginning-of-line)
                                              (point))
                                       (progn (end-of-line)
                                              (point)))))

    (and (not (string-equal line-string ""))
         (not (string-equal line-string ""))
         (not (string-equal (substring line-string 0 1) "#")))))

(defun hackystat*props!get-key ()
  "Returns the key string on the current non-comment line or nil if none found."
  (let ((start (progn 
                 (beginning-of-line)
                 (point)))
        (equal-sign (search-forward "=" (point-at-eol) t)))
    (if equal-sign
        (hackystat*props!trim-string
         (buffer-substring start (1- equal-sign))))))


(defun hackystat*props!get-value ()
  "Returns the value string on the current non-comment line or nil if none found."
  (let ((start (progn 
                 (beginning-of-line)
                 (search-forward "=" (point-at-eol) t))))
    (if start
        (hackystat*props!trim-string 
         (buffer-substring start (point-at-eol))))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; String utilities
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*props!trim-string (str)
  "Returns the passed string with leading and trailing spaces and ^M's removed."
  (hackystat*props!string-remove-regexp search-whitespace-regexp str))


;; This is recursive; ought to be iterative.  But won't get too deep -- at
;; most a few for ordinary uses of this function.

(defun hackystat*props!string-remove-regexp (regexp string)
  "Return a string with (nonoverlapping) matches for REGEXP removed from STRING."
  (if (string-match regexp string)
      (concat (substring string 0 (match-beginning 0))
              (hackystat*props!string-remove-regexp regexp (substring string (match-end 0))))
    string))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Set up the sensor properties
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(setq hackystat*user-home (hackystat*props!guarantee-suffix hackystat*user-home "/"))
(hackystat*props*make hackystat*user-home)

(provide 'sensor-properties)


