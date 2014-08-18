(require 'lt-serial)

(defun lt-serial-kernel-init ()
  (interactive)
  (call-interactively 'lt-serial-init)
  (setq page-delimiter "Initializing cgroup subsys cpuset")
  (setq lt-faces '((".*\\(error\\|fail\\).*$"			.	'error)
		   ("[[:alnum:]]+@[[:alnum:]]+:/.* [\$#] $"	.       'success)
		   ("# $"					.	'success)
		   ("shell> $"					.	'success)
		   ("\\\[<[0-9a-f]+>\\\]"			.	'warning))))

(lt-register-backend (make-lt-backend :name "serial-kernel"
				      :init 'lt-serial-kernel-init
				      :restart 'lt-serial-start-process))

(provide 'lt-serial-kernel)

