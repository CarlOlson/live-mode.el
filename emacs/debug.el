(make-variable-buffer-local
 (defvar live/debug-buffer-state ""
   "Expected state of current buffer in `live-test-mode'."))

(define-minor-mode live-test-mode
    "Checks that live-mode works as intended without sending any
data to a server."
  :init-value nil
  :lighter " LiveTest"
  :keymap nil
  (cond
    (live-test-mode
     (live/log "startup")
     (setq-local live/send-hook '(live/check-events))
     (live/startup))
    (t
     (live/log "shutdown")
     (kill-local-variable 'live/send-hook)
     (live/shutdown))))

(defun live/log (string &rest objects)
  (let ((name (buffer-name))
	(inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*live-mode-debug*")
      (read-only-mode 1)
      (save-excursion
	(goto-char (point-max))
	(insert (format "[%s] " name))
	(insert (apply 'format string objects))
	(newline)))))

(defun live/check-events (events)
  (mapc 'live/check-event events))

(defun live/check-event (event)
  (cond
    ((live/set-event? event)
     (live/debug-apply-set event))
    ((live/update-event? event)
     (live/debug-apply-update event))
    (t
     (live/log "Unknown event %S" event)))
  (live/check-state))

(defun live/check-state ()
  (unless (live/valid-state?)
    (live/log "%s, %s, bad state" last-command this-command)
    (live/reset-state)))

(defun live/valid-state? ()
  (string=
   live/debug-buffer-state
   (buffer-substring-no-properties (point-min) (point-max))))

(defun live/reset-state ()
  (setq live/debug-buffer-state
	(buffer-substring-no-properties (point-min) (point-max))))

(defun live/set-event? (event)
  (equal (live/get-event 'event event) 'set))

(defun live/update-event? (event)
  (equal (live/get-event 'event event) 'update))

(defun live/get-event (key event)
  (cdr (assoc key event)))

(defun live/debug-apply-set (event)
  (setq live/debug-buffer-state
	(live/get-event 'text event)))

(defun live/debug-apply-update (event)
  (let ((start  (live/get-event 'start  event))
	(length (live/get-event 'length event))
	(text   (live/get-event 'text   event))
	(state  live/debug-buffer-state))
    (setq live/debug-buffer-state
	  (concat (subseq state 0 (1- start))
		  text
		  (subseq state (+ (1- start) length))))))
