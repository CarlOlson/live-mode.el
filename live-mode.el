
(require 'json)

(defvar live/port "3000")
(defvar live/url (concat "http://localhost:" live/port))

(defun live/post-set-event (text)
  (let ((data (json-encode `((event . set)
			     (text  . ,text)))))
    (async-start-process "live/post" "curl" nil
			 "--data" data live/url)))

(defun live/post-update-event (start length text)
  (let ((data (json-encode `((event  . update)
			     (start  . ,start)
			     (length . ,length)
			     (text   . ,text)))))
    (async-start-process "live/post" "curl" nil
			 "--data" data live/url)))

(defun live/after-change-fn (start end prev-length)
  (live/post-update-event start
			  prev-length
			  (buffer-substring start end)))

(defun live/setup ()
  (if live-mode
      (progn
	(add-hook 'after-change-functions 'live/after-change-fn nil t)
	(live/post-set-event (buffer-string)))
    (remove-hook 'after-change-functions 'live/after-change-fn t)))

(define-minor-mode live-mode
    ""
  :init-value nil
  :lighter " Live"
  :keymap nil)

(add-hook 'live-mode-hook 'live/setup)
