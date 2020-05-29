;;; keylog-mode.el --- Keystroke logging

;; Copyright (c) 2020 Michael Piotrowski

;; Author:    Michael Piotrowski <mxp@dynalabs.de>
;; Created:   2020-05-17
;; Version:   0.1
;; Keywords:  keystroke logging, writing research
;; Stability: unstable

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for logging keystrokes for replay and analysis.

;;; Code:

;; A global counter is probably not ideal: the actual logs may not
;; necessarily start with 1 and if logging in several buffers, the IDs
;; in the individual logs won't be contiguous.  But they will be
;; increasing, and they don't really have any importance anyway.
;; [FIXME] use a buffer local variable.
(defvar keylog-event-id 0 "Counter for `log-keys'.")

(define-minor-mode keylog-mode
  "Toggle Keylog mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Keylog mode is enabled, ..."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " ⌨︎"
  :group 'keylog
  ;; Body
  (defvar keylog-event-id 0 "Counter for `log-keys'.")
  (if (bound-and-true-p keylog-mode)
      (keylog-mode-start)
    (keylog-mode-stop)))

(defun keylog-mode-start ()
  (let ((timestamp (format-time-string "%FT%T")))
    ;; Save a copy of the current buffer (as starting point for replaying)
    (write-region (point-min) (point-max)
		  (concat (buffer-file-name) "--" timestamp ".point")
		  nil nil nil 'excl)
    ;; Buffer for logging
    (set (make-local-variable 'keylog-logfile)
	 (find-file-noselect
	  (concat (buffer-file-name) "--" timestamp ".keylog")))

    ;;
    (with-current-buffer (get-buffer-create keylog-logfile)
      (insert ";;; keylog-mode log file " timestamp "\n(")
      )

    )
 
  (add-hook 'pre-command-hook 'keylog-log-keys nil t)
  (message "Starting keystroke logging..."))

(defun keylog-mode-stop ()
  (message "Stopping keystroke logging...")
  (setq this-command 'keylog-mode-stop)
  
  (remove-hook 'pre-command-hook 'keylog-log-keys t)
  ;; `keylog-post-command' will run one last time and then remove itself
  ;; from `post-command-hook'
  )

(defun keylog-log-keys ()
  (interactive)
  ;; Add it here rather than in `keylog-mode-stop' to avoid creating
  ;; an entry for the activation itself
  (add-hook 'post-command-hook 'keylog-post-command nil t)
  
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (let ((major-mode-name major-mode)
            (tt (format-time-string "%T:%N"))
	    (pos (point))
	    (len (buffer-size))
	    (buf (buffer-name)))
        (with-current-buffer (get-buffer-create keylog-logfile)
          (goto-char (point-max))
	  (insert (format "\n(%d \"%s\" %d %d \"" keylog-event-id tt pos len)
	  	  (key-description (this-command-keys)) "\"")
	  
	  (setf keylog-event-id (1+ keylog-event-id)))))))

(defun keylog-post-command ()
  "Log buffer length after command execution."
  (interactive)
  
  (let ((len (buffer-size)))
    (with-current-buffer (get-buffer-create keylog-logfile)
      (goto-char (point-max))
      (insert (format " %S %d)" this-command len))

      (when (eq this-command 'keylog-mode-stop)
	(insert "\n)\n") ; insert closing paren
	(save-buffer)))

    (when (eq this-command 'keylog-mode-stop)
      (remove-hook 'post-command-hook 'keylog-post-command t))))


(provide 'keylog-mode)

;;; keylog-mode.el ends here
