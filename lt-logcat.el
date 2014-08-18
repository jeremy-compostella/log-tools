(require 'debug-tools)

(defun lt-logcat-filter (buffer proc string)
  (lt-insert-string-in-log-buffer buffer (replace-regexp-in-string "\r" "" string)))

(defun lt-logcat-init ()
  (interactive)
  (setq lt-faces '((".*\\(error\\|fail\\).*$"	.	'error)
		   ("  E/.*$"			.	'error)
		   ("  W/.*$"			.	'warning)))
  (start-process "logcat" (current-buffer) "adb" "logcat")
  (set-process-filter (get-buffer-process (current-buffer))
		      (curry 'lt-logcat-filter (current-buffer))))

(lt-register-backend (make-lt-backend :name "logcat"
				      :init 'lt-logcat-init))

(provide 'dt-logcat)
