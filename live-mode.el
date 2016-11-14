
(require 'cl-lib)
(require 'json)

(defvar live/port 3000)

(defvar live/url-format "http://localhost:%s/%s")

(defvar live/indent-commands '(newline indent-for-tab-command))

(defvar live/mode-alist '(("\\.rb$"  . "ruby")
			  ("\\.erb$" . "html")))

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
  (start-process "*live/post*" "*live/post*"
		 "curl"
		 "-H" "Content-Type: application/json"
		 ;; "--data-urlencode"
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

(defun live/send-undo (undo)
  (let ((head (car undo))
	(tail (cdr undo)))
    (cond
      ((and (integerp head)
	    (integerp tail))
       (live/send-update-event head
			       0
			       (buffer-substring head tail)))
      ((and (stringp head)
	    (integerp tail))
       (live/send-update-event tail
			       (length head)
			       "")))))

(defun live/after-change-fn (start end prev-length)
  (unless (member this-command live/indent-commands)
    (live/send-update-event start
			    prev-length
			    (buffer-substring start end))))

(defun live/pre-indent-fn ()
  (when (member this-command live/indent-commands)
    (setq live/previous-undo-list buffer-undo-list)))

(defun live/post-indent-fn ()
  (when (and (member this-command live/indent-commands)
	     live/previous-undo-list)
    (dolist (undo (reverse (live/recent-undos)))
      (live/send-undo undo))
    (setq live/previous-undo-list nil)))

(defun live/setup ()
  (if live-mode
      (progn
	(add-hook 'after-change-functions 'live/after-change-fn nil t)
	(add-hook 'pre-command-hook       'live/pre-indent-fn nil t)
	(add-hook 'post-command-hook      'live/post-indent-fn nil t)
	(live/send-set-event (buffer-string)))
    (remove-hook 'after-change-functions 'live/after-change-fn t)
    (remove-hook 'pre-command-hook       'live/pre-indent-fn t)
    (remove-hook 'post-command-hook      'live/post-indent-fn t)))

(define-minor-mode live-mode
    ""
  :init-value nil
  :lighter " Live"
  :keymap nil)

(add-hook 'live-mode-hook 'live/setup)
