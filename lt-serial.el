(require 'log-tools)

(defvar lt-serial-default-port nil) 
(defvar lt-serial-default-speed 115200) 
(defvar lt-serial-controlkeys '(("up"		.	"\e[A")
				("down"		.	"\e[B")
				("f2"		.	"\e[12~")
				("f4"		.	"\e[14~")
				("return"	.	"\r")
				("escape"	.	"\e")))

(defvar-local lt-serial-port nil)
(defvar-local lt-serial-speed nil)
(defvar-local lt-serial-clean-regexp '("[\r\r\x]" "\e\\[[0-9]*m" "\e\\[[0-9]+;[0-9]+H?l?"))

(defun lt-serial-filter (buffer proc string)
  (mapc (lambda (x) (setq string (replace-regexp-in-string x "" string))) lt-serial-clean-regexp)
  (lt-insert-string-in-log-buffer buffer string))

(defun lt-serial-bind-controlkeys ()
  (dolist (key2value lt-serial-controlkeys)
    (local-set-key (kbd (format "<C-%s>" (car key2value)))
		   (icurry 'lt-send-string (cdr key2value)))))

(defun lt-serial-start-process ()
  (light-save-excursion-if-not-at-point-max (current-buffer)
    (goto-char (point-max))
    (when (get-buffer-process (current-buffer))
      (delete-process (get-buffer-process (current-buffer))))
    (make-serial-process :port lt-serial-port
			 :buffer (current-buffer)
			 :speed lt-serial-speed
			 :filter (curry 'lt-serial-filter (current-buffer)))))

(defun lt-serial-init (&optional serial-port serial-speed)
  (interactive (list (read-file-name "Serial port: " "/dev" lt-serial-default-port t)
		     (read-number "Serial speed: " lt-serial-default-speed)))
  (setq lt-serial-port serial-port
	lt-serial-speed serial-speed)
  (lt-serial-start-process)
  (lt-serial-bind-controlkeys)
  (lt-append-to-buffer-name (file-name-nondirectory serial-port)))

(lt-register-backend (make-lt-backend :name "serial"
				      :init 'lt-serial-init
				      :restart 'lt-serial-start-process))

(provide 'lt-serial)
