(require 'log-tools)

(defun lt-logcat-filter (buffer proc string)
  (lt-insert-string-in-log-buffer buffer (replace-regexp-in-string "\r" "" string)))

(defun lt-logcat-start-process ()
  (start-file-process "logcat" (current-buffer) "adb" "logcat")
  (set-process-filter (get-buffer-process (current-buffer))
		      (curry 'lt-logcat-filter (current-buffer))))

(defun lt-logcat-init ()
  (interactive)
  (setq page-delimiter "--------- beginning of")
  (setq lt-faces '((".*\\(error\\|fail\\).*$"	.	'error)
		   ("  E/.*$"			.	'error)
		   ("  W/.*$"			.	'warning)))
  (lt-logcat-start-process))

(lt-register-backend (make-lt-backend :name "logcat"
				      :init 'lt-logcat-init
				      :restart 'lt-logcat-start-process))

(provide 'lt-logcat)
