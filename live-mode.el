
(require 'cl-lib)
(require 'json)

(defvar live/port 3000)

(defvar live/url-format "http://localhost:%s/%s")

(defvar live/process-with-undo-commands
  '(newline indent-for-tab-command))

(defvar live/mode-alist '(("\\.el$"  . "lisp")
			  ("\\.rb$"  . "ruby")
			  ("\\.erb$" . "html")
			  ("\\.pl$"  . "prolog")))

(make-local-variable
 (defvar live/previous-undo-list nil))

(defvar live/event-queue nil)

(defun live/get-highlight-mode ()
  (let* ((filename (buffer-file-name))
	 (pair (cl-find-if (lambda (regex-mode)
			     (string-match-p (car regex-mode)
					     filename))
			   live/mode-alist))
	 (mode (cdr pair)))
    (or mode "")))

(defun live/recent-undos ()
  (unless (eq live/previous-undo-list
	      buffer-undo-list)
    (cl-loop
       for (undo . rest) on buffer-undo-list
       collect undo
       until (eq rest live/previous-undo-list))))

(defun live/send-queued-events ()
  (when live/event-queue
    (let ((queued (nreverse live/event-queue)))
      (setq live/event-queue nil)
      ;; TODO: only use synchronously during development
      (call-process "curl" nil nil nil
		    "-H" "Content-Type: application/json"
		    "--connect-timeout" "1"
		    "--data"
		    (json-encode queued)
		    (format live/url-format
			    live/port
			    (buffer-name))))))

(defun live/queue-json (json)
  (push json live/event-queue)
  (run-at-time 0 nil 'live/send-queued-events))

(defun live/queue-set-event (text)
  (live/queue-json `((event . set)
		     (text  . ,text)
		     (mode  . ,(live/get-highlight-mode)))))

(defun live/queue-update-event (start length text)
  (live/queue-json `((event  . update)
		     (start  . ,start)
		     (length . ,length)
		     (text   . ,text))))

(defun live/reverse-undo-list (undos)
  "Applys undo information and returns param lists for
`live/queue-update-event'."
  (cl-labels ((rec (undo)
		(when (listp undo)
		  (let ((head (car undo))
			(tail (cdr undo)))
		    (cond
		      ((and (integerp head)
			    (integerp tail))
		       (prog1 (list head 0 (buffer-substring head tail))
			 (delete-region head tail)))
		      ((and (stringp head)
			    (integerp tail))
		       (prog1 (list (abs tail) (length head) "")
			 (goto-char (abs tail))
			 (insert head))))))))
    (reverse
     (delete nil
	     (mapcar (lambda (undo)
		       (rec undo))
		     undos)))))

(defun live/queue-recent-undos ()
  (let* ((text      (buffer-string))
	 (undo-copy (copy-list buffer-undo-list))
	 (recent    (live/recent-undos))
	 (undo-list (with-temp-buffer
		      (insert text)
		      (live/reverse-undo-list recent))))
    (dolist (args undo-list)
      (apply 'live/queue-update-event args))))

(defun live/requires-undo-p ()
  (member this-command live/process-with-undo-commands))

(defun live/after-change-fn (start end prev-length)
  (unless (live/requires-undo-p)
    (live/queue-update-event start
			     prev-length
			     (buffer-substring start end))))

(defun live/pre-command-fn ()
  (when (live/requires-undo-p)
    (setq live/previous-undo-list buffer-undo-list)))

(defun live/post-command-fn ()
  (when (and (live/requires-undo-p)
	     live/previous-undo-list)
    (run-with-timer 0 nil
		    (lambda ()
		      (live/queue-recent-undos)
		      (setq live/previous-undo-list nil)))))

(defun live/setup ()
  (if live-mode
      (progn
	(add-hook 'after-change-functions 'live/after-change-fn nil t)
	(add-hook 'pre-command-hook       'live/pre-command-fn nil t)
	(add-hook 'post-command-hook      'live/post-command-fn nil t)
	(live/queue-set-event (buffer-string)))
    (remove-hook 'after-change-functions 'live/after-change-fn t)
    (remove-hook 'pre-command-hook       'live/pre-command-fn t)
    (remove-hook 'post-command-hook      'live/post-command-fn t)))

(define-minor-mode live-mode
    ""
  :init-value nil
  :lighter " Live"
  :keymap nil)

(add-hook 'live-mode-hook 'live/setup)
