;;; lt-logat.el --- Log Tools logcat backend

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
