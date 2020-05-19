;;; Log only in a particular buffer---minor mode?
;;; Save copy of current buffer as reference for later replay.

;;; Document length *after* event is only available in post-command-hook
;;; What needs to be logged to generate S-notation?
;;; What does InputLog's XML look like?

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
	  (concat (buffer-file-name) "--" timestamp ".keylog"))))
  
  (add-hook 'pre-command-hook 'keylog-log-keys nil t)
  (message "Starting keystroke logging..."))

(defun keylog-mode-stop ()
  (message "Stopping keystroke logging...")
  (with-current-buffer (get-buffer keylog-logfile)
    (save-buffer))
  (remove-hook 'pre-command-hook 'keylog-log-keys t))

(defun keylog-log-keys ()
  (interactive)
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (let ((major-mode-name major-mode)
            (tt (format-time-string "%T:%N") ;(format-time-string "%s" (current-time)
		)
	    (pos (point))
	    (len (buffer-size))
	    (buf (buffer-name)))
        (with-current-buffer (get-buffer-create keylog-logfile)
          (goto-char (point-max))
          (if (eql this-command 'self-insert-command)
              (let ((desc (key-description (this-command-keys))))
                (if (= 1 (length desc))
                    (insert (format "\n%d %s buf: %s %S pos: %d len: %d "
				    keylog-event-id tt buf major-mode-name pos len)
			    desc)
                  (insert (format "\n%d %s buf: %s %S pos: %d len: %d "
				  keylog-event-id tt buf major-mode-name
				  pos len) " " desc " ")))
	    (insert (format "\n%d %s buf: %s %S pos: %d len: %d "
			    keylog-event-id tt buf major-mode-name pos len)
		    (key-description (this-command-keys))))
	  (setf keylog-event-id (1+ keylog-event-id)))))))


(provide 'keylog-mode)

;;; keylog-mode.el ends here
