
(require 'cl-lib)
(require 'json)

(defvar live/port 3000)

(defvar live/url-format "http://127.0.0.1:%s/%s")

(defvar live/process-with-undo-commands
  '(newline indent-for-tab-command))

(defvar live/mode-alist '(("\\.el$"  . "lisp")
			  ("\\.rb$"  . "ruby")
			  ("\\.erb$" . "html")
			  ("\\.pl$"  . "prolog")))

(make-local-variable
 (defvar live/previous-undo-list nil))

(make-local-variable
 (defvar live/event-queue nil))
(make-local-variable
 (defvar live/undo-event-stack nil))
(make-local-variable
 (defvar live/change-event-stack nil))

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

(defun live/merge-lists (a b)
  ;; TODO: improve by finding equal sublists
  (cond
    ((and (null a) (null b))
     nil)
    ((equal (car a) (car b))
     (cons (car a)
	   (live/merge-lists (cdr a)
			     (cdr b))))
    ((>= (length a) (length b))
     (cons (car a)
	   (live/merge-lists (cdr a) b)))
    (t (live/merge-lists b a))))

(defun live/simplify-stacks (updates)
  ;; TODO: try to simplify
  (cl-labels ((join (a b)
		(cl-destructuring-bind (a-start a-len a-text) a
		  (cl-destructuring-bind (b-start b-len b-text) b
		    (cond
		      ((and (= a-len b-len 0)
			    (= a-start (+ b-start (length b-text))))
		       (list b-start 0 (concat b-text a-text)))))))
	      (rec (updates)
		(cond
		  ((null (cdr updates)) updates)
		  ((join (car updates) (cadr updates))
		   (rec (cons (join (car updates)
				    (cadr updates))
			      (cddr updates))))
		  (t (cons (car updates)
			   (rec (cdr updates)))))))
    (let ((len (length updates))
	  (rtn (rec updates)))
      (while (/= len (length rtn))
	(setq len (length rtn)
	      rtn (rec rtn)))
      rtn)))

(defun live/execute-queue ()
  (cond
    ((or (null live/undo-event-stack)
	 (null live/change-event-stack)
	 (equal live/change-event-stack
		live/undo-event-stack))
     (dolist (event (or live/undo-event-stack
			live/change-event-stack))
       (apply 'live/queue-update-event event)))
    (t
     (dolist (event (live/merge-lists
		     (live/simplify-stacks
		      live/undo-event-stack)
		     (live/simplify-stacks
		      live/change-event-stack)))
       (apply 'live/queue-update-event event))))
  (setq live/undo-event-stack   nil
	live/change-event-stack nil))

(defun live/send-queued-events ()
  (when live/event-queue
    (let ((queued live/event-queue))
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
      (push args live/undo-event-stack))))

(defun live/requires-undo-p ()
  (member this-command live/process-with-undo-commands))

(defun live/after-change-fn (start end prev-length)
  (push (list start prev-length (buffer-substring start end))
	live/change-event-stack))

(defun live/pre-command-fn ()
  (setq live/previous-undo-list buffer-undo-list))

(defun live/post-command-fn ()
  (run-with-timer 0 nil
		  (if (and (live/requires-undo-p)
			   live/previous-undo-list)
		      (lambda ()
			(live/queue-recent-undos)
			(setq live/previous-undo-list nil)
			(live/execute-queue))
		    (lambda ()
		      (setq live/previous-undo-list nil)
		      (live/execute-queue)))))

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
