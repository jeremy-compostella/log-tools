;;; lt-logat.el --- Log Tools serial kernel backend

;; Copyright (C) 2014 2014 Free Software Foundation, Inc.

;; Author: Jeremy Compostella <jeremy.compostella@gmail.com>
;; Keywords: comm, processes
;; Package: log-tools

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'lt-serial)

(defun lt-serial-kernel-init (&rest args)
  (interactive)
  (apply 'lt-serial-init args)
  (setq page-delimiter "Initializing cgroup subsys cpuset")
  (setq lt-faces `((".*\\(error\\|fail\\).*$"								.	error)
		   ("\\\([[:alnum:]]\\\|_\\\|-\\\)+@\\\([[:alnum:]]\\\|_\\\|-\\\)+ :/.* [\$#] $"	.       success)
		   ("# $"										.	success)
		   ("shell> $"										.	success)
		   ("\\\[<[0-9a-f]+>\\\]"								.	warning))))

(lt-register-backend (make-lt-backend :name "serial-kernel"
				      :init 'lt-serial-kernel-init
				      :restart 'lt-serial-start-process))

(provide 'lt-serial-kernel)

