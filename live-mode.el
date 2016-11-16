
(require 'cl-lib)
(require 'json)

(defvar live/port 3000)

(defvar live/url-format "http://localhost:%s/%s")

(defvar live/process-with-undo-commands
  '(newline indent-for-tab-command))

(defvar live/mode-alist '(("\\.rb$"  . "ruby")
			  ("\\.erb$" . "html")
			  ("\\.pl"   . "prolog")))

(defvar live/previous-undo-list nil)

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

(defun live/send-json (json)
  (start-process "*live/post*" nil
		 "curl"
		 "-H" "Content-Type: application/json"
		 "--data"
		 (json-encode json)
		 (format live/url-format
			 live/port
			 (buffer-name))))

(defun live/send-set-event (text)
  (live/send-json `((event . set)
		    (text  . ,text)
		    (mode  . ,(live/get-highlight-mode)))))

(defun live/send-update-event (start length text)
  (live/send-json `((event  . update)
		    (start  . ,start)
		    (length . ,length)
		    (text   . ,text))))

(defun live/reverse-undo-list (undos)
  "Applys undo information and returns param lists for
`live/send-update-event'."
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

(defun live/send-recent-undos ()
  (let* ((text      (buffer-string))
	 (undo-copy (copy-list buffer-undo-list))
	 (recent    (live/recent-undos))
	 (undo-list (with-temp-buffer
		      (insert text)
		      (live/reverse-undo-list recent))))
    (dolist (args undo-list)
      (apply 'live/send-update-event args))))

(defun live/requires-undo-p ()
  (member this-command live/process-with-undo-commands))

(defun live/after-change-fn (start end prev-length)
  (unless (live/requires-undo-p)
    (live/send-update-event start
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
		      (run-with-timer 0 nil
				      (lambda ()
					(live/send-recent-undos)
					(setq live/previous-undo-list nil)))))))

(defun live/setup ()
  (if live-mode
      (progn
	(add-hook 'after-change-functions 'live/after-change-fn nil t)
	(add-hook 'pre-command-hook       'live/pre-command-fn nil t)
	(add-hook 'post-command-hook      'live/post-command-fn nil t)
	(live/send-set-event (buffer-string)))
    (remove-hook 'after-change-functions 'live/after-change-fn t)
    (remove-hook 'pre-command-hook       'live/pre-command-fn t)
    (remove-hook 'post-command-hook      'live/post-command-fn t)))

(define-minor-mode live-mode
    ""
  :init-value nil
  :lighter " Live"
  :keymap nil)

(add-hook 'live-mode-hook 'live/setup)
