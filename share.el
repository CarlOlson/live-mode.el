
;; (start-process "discovery"
;; 	       "*discovery*"
;; 	       "ruby" "simple_peer_discovery"
;; 	       "emacs" NAME PORT))

(define-minor-mode buffer-streaming-mode
    ""
  :init-value nil
  :lighter "Streaming"
  :keymap nil)

(define-minor-mode buffer-watch-mode
    ""
  :init-value nil
  :lighter "Watching"
  :keymap nil)

(add-hook 'buffer-watch-mode #'read-only-mode)

(defun watch-shared-buffer ()
  (interactive)
  )

(defun stream-buffer ()
  (interactive)
  )
