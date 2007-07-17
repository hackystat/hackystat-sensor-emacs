;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sensor-utils.el -- 
;; Author          : Philip Johnson
;; Created On      : Fri Sep 28 10:11:24 2001
;; Last Modified By: 
;; Last Modified On: Mon Jul 16 13:21:22 2007
;; RCS: $Id: sensor-utils.el,v 1.1.1.1 2005/10/20 23:56:57 johnson Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (C) 2001 Philip Johnson
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; See http://csdl.ics.hawaii.edu/FAQ/Lisp/lisp-naming-conventions.html
;; for info on why these functions are named this way.

(require 'cl)

;;;  define point-at-eol if not already defined, this should help sensor work with
;;;  old versions of emacs prior to 21.7.
(unless (functionp 'point-at-eol)
  (fset 'point-at-eol 'line-end-position))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Version info
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar hackystat-version "@hackystat.version@"
  "Token that is replaced by the actual hackystat release number by the Ant task.")

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Time (millisecond) utilities
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun hackystat*util*milliseconds-string (milliseconds)
  "Return the string representation of MILLISECONDS, which is in (current-time) format ."
  (format "%0.f" (* 1000 (+ (* (float (car milliseconds)) 65536) 
                            (cadr milliseconds)))))


(defun hackystat*util*diff-milliseconds (first second)
  "Returns (- FIRST SECOND) as a float in seconds.
FIRST and SECOND are  milliseconds in the format returned by (current-time)."
  (let ((upper (- (car first) (car second)))
        (lower (- (cadr first) (cadr second))))
    (+ (* (float upper) 65536) lower)))


(provide 'sensor-utils)
  
