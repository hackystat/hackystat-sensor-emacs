;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sensor-hooks.el -- 
;; Author          : Philip Johnson
;; Created On      : Sun Jun 24 10:53:11 2001
;; Last Modified By: 
;; Last Modified On: Sat Sep 08 11:31:09 2007
;; RCS: $Id: sensor-hooks.el,v 1.1.1.1 2005/10/20 23:56:57 johnson Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 2001 Philip Johnson
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; See http://csdl.ics.hawaii.edu/FAQ/Lisp/lisp-naming-conventions.html
;; for info on why these functions are named this way.

;; This file contains the hook function code for installing the sensors.

(require 'cl)
(require 'comint)
;; Load remainder of system.
(require 'sensor-properties)
(require 'sensor-utils)
(require 'sensorshell)
(require 'devevent-sensor)

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Hook definition functions
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*hook*init-sensors ()
  "Initializes all of the currently installed sensors."
  ;; SensorShell must be started before anything else can be init'd.
  (hackystat*sensorshell*start)
  (hackystat*devevent*init))

(defun hackystat*hook!add-info (subtype)
  "Records the passed subtype regarding the current buffer if it is bound to a file."
  (let ((file (buffer-file-name)))
    (when file
      (hackystat*devevent*add-event subtype file))))

(defun hackystat*hook*open-file ()
  "Records that the current file was just visited if not recorded already."
  (hackystat*hook!add-info "OpenFile"))

(defun hackystat*hook*save-file ()
  "Records that the current file was saved."
  (hackystat*hook!add-info "SaveFile"))

(defun hackystat*hook*close-file ()
  "Records when the current file is closed."
  (hackystat*hook!add-info "CloseFile"))

(defun hackystat*hook*shell-command (command)
  "Records that a shell program has been invoked. Does not record hackystat shell stuff, of course."
  (unless (equal (buffer-name) hackystat*sensorshell!buffer-name)
    (hackystat*devevent*add-event "RunProgram" (buffer-file-name) (concat "Command=" command))))

(defun hackystat*hook*kill-emacs ()
  "Send activity and session data whenever Emacs exits."
  (hackystat*devevent*add-event "Exit" (buffer-file-name))
  (hackystat*sensorshell*shutdown))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Install hooks
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(add-hook 'after-init-hook               'hackystat*hook*init-sensors)
(add-hook 'find-file-hooks               'hackystat*hook*open-file)
(add-hook 'after-save-hook               'hackystat*hook*save-file)
(add-hook 'kill-buffer-hook              'hackystat*hook*close-file)
(add-hook 'comint-input-filter-functions 'hackystat*hook*shell-command)
(add-hook 'kill-emacs-hook               'hackystat*hook*kill-emacs)

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Stop hackystat processing.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*uninstall ()
  "Disable hackystat hook functions in case problems occur in installation."
  (interactive)
  (remove-hook 'after-init-hook               'hackystat*hook*init-sensors)
  (remove-hook 'find-file-hooks               'hackystat*hook*open-file)
  (remove-hook 'after-save-hook               'hackystat*hook*save-file)
  (remove-hook 'kill-buffer-hook              'hackystat*hook*close-file)
  (remove-hook 'comint-input-filter-functions 'hackystat*hook*shell-command)
  (remove-hook 'kill-emacs-hook               'hackystat*hook*kill-emacs))
  

(provide 'sensor-hooks)
(provide 'sensor-package)





