(require 'log-tools)

(defvar lt-serial-default-port nil) 
(defvar lt-serial-default-speed 115200) 
(defvar lt-serial-controlkeys '(("up"		.	"\e[A")
				("down"		.	"\e[B")
				("f2"		.	"\e[12~")
				("f4"		.	"\e[14~")
				("return"	.	"\r")
				("escape"	.	"\e")))

(defcustom lt-serial-socat-port 5900
  "TCP port to use with socat."
  :group 'lt-serial)

(defvar-local lt-serial-socat-processes '())
(defvar-local lt-serial-real-port nil)
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

(defun lt-serial-over-socat ()
  (with-parsed-tramp-file-name lt-serial-port remote
    (let ((buf (get-buffer-create "*socat*")))
      (let ((default-directory (file-name-directory lt-serial-port)))
	(setq lt-serial-socat-processes
	      (list (start-file-process "socat" buf "socat"
					(concat "FILE:" remote-localname)
					(format "TCP-LISTEN:%d" lt-serial-socat-port)))))
      (let ((tmp (make-temp-name "/tmp/tty")))
	(add-to-list 'lt-serial-socat-processes
		     (start-process "socat" buf "socat" (concat "PTY,link=" tmp)
				    (format "TCP:%s:%d" remote-host lt-serial-socat-port)))
	(sleep-for 1)		; Give time to socat to create the file
	(setq lt-serial-real-port tmp)))))

(defun lt-serial-clear-buffer ()
  (dolist (p lt-serial-socat-processes)
    (kill-process p))
  (setq lt-serial-socat-processes '()))

(defun lt-serial-start-process ()
  (lt-serial-clear-buffer)
  (when (tramp-tramp-file-p lt-serial-port)
    (lt-serial-over-socat))
  (light-save-excursion-if-not-at-point-max (current-buffer)
    (goto-char (point-max))
    (when (get-buffer-process (current-buffer))
      (delete-process (get-buffer-process (current-buffer))))
    (make-serial-process :port lt-serial-real-port
			 :buffer (current-buffer)
			 :speed lt-serial-speed
			 :filter (curry 'lt-serial-filter (current-buffer)))))


(defun lt-serial-init (&optional serial-port serial-speed)
  (interactive (list (read-file-name "Serial port: " "/dev" lt-serial-default-port t)
		     (read-number "Serial speed: " lt-serial-default-speed)))
  (setq lt-serial-port serial-port
	lt-serial-real-port serial-port
	lt-serial-speed serial-speed)
  (lt-serial-start-process)
  (lt-serial-bind-controlkeys)
  (lt-append-to-buffer-name (file-name-nondirectory serial-port)))

(lt-register-backend (make-lt-backend :name "serial"
				      :init 'lt-serial-init
				      :restart 'lt-serial-start-process))

(add-hook 'kill-buffer-hook 'lt-serial-clear-buffer)

(provide 'lt-serial)
