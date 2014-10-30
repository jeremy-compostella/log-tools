;;; log-tools.el --- Log Tools

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

(require 'cl)

(defgroup log-tools nil
  "Log tools group."
  :group 'tools)

(defvar lt-backends '())

(defcustom lt-time-fmt "%Y/%m/%d  %H:%M:%S  "
  "Format for timestamp prefix displayed at the beginning of each
log line."
  :group 'log-tools)

(defcustom lt-max-line-nb 100000
  "Maximum number of line of the log buffer."
  :group 'log-tools)

(defcustom lt-delete-line-nb 1000
  "Number of line to delete at once to maintain the maximum
number of line, see `lt-max-line-nb'."
  :group 'log-tools)

(defcustom lt-propertize-line-max-length 200
  "Maximum line length allowed to be propertized.  If the a line
length is larger than this value it won't be propertized."
  :group 'log-tools)

(defcustom lt-buf-fmt "*lt:%s*"
  "Log tools buffer name format."
  :group 'log-tools)

(defvar-local lt-faces
  '((".*\\(error\\|fail\\).*$"	.	'error)))

(defvar-local lt-backend nil)

(defface lt-hi-red
  '((((background dark)) (:background "red" :foreground "white"))
    (t (:background "red")))
  "Face for log-tools mode.")

(defface lt-hi-pink
  '((((background dark)) (:background "pink" :foreground "black"))
    (t (:background "pink")))
  "Face for log-tools mode.")

(defface lt-hi-green
  '((((background dark)) (:background "green" :foreground "black"))
    (t (:background "green")))
  "Face for log-tools mode.")

(defface lt-hi-yellow
  '((((background dark)) (:background "yellow" :foreground "black"))
    (t (:background "yellow")))
  "Face for log-tools mode.")

(defface lt-hi-orange
  '((((background dark)) (:background "orange" :foreground "black"))
    (t (:background "orange")))
  "Face for log-tools mode.")

(defface lt-hi-blue
  '((((background dark)) (:background "blue" :foreground "black"))
    (t (:background "blue")))
  "Face for log-tools mode.")

(defvar-local lt-hi-faces
  '(("pink"	.	'lt-hi-pink)
    ("yellow"	.	'lt-hi-yellow)
    ("green"	.	'lt-hi-green)
    ("red"	.	'lt-hi-red)
    ("orange"	.	'lt-hi-orange)
    ("blue"	.	'lt-hi-blue)))

(defvar-local lt-hi-list '())

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst icurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (interactive) (apply function (append arguments more)))))

(defmacro light-save-excursion (&rest body)
  (declare (indent 1))
  `(let ((save-pos (make-symbol "save-pos")))
     (setq save-pos (point))
     ,@body
     (goto-char save-pos)))

(defmacro light-save-excursion-if-not-at-point-max (buf &rest body)
  (declare (indent 1))
  `(if (= (point-max) (point))
       (progn ,@body
              (when (get-buffer-window ,buf t)
		(with-selected-window (get-buffer-window ,buf t)
		  (goto-char (point-max)))))
     (light-save-excursion (progn ,@body))))

(defun lt-buffer-auto-shrink ()
  (light-save-excursion
   (goto-char (point-max))
    (when (> (line-number-at-pos) lt-max-line-nb)
      (delete-region (point-min)
		     (progn (goto-char (point-min))
			    (forward-line lt-delete-line-nb)
			    (point))))))

(defsubst lt-insert-current-date-string ()
  (insert (propertize (format-time-string lt-time-fmt (current-time))
		      'face 'font-lock-comment-face)))

(defun lt-insert-string-in-log-buffer (buf string)
  (when (and (> (length string) 0) (get-buffer buf))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(light-save-excursion-if-not-at-point-max buf
	  (goto-char (point-max))
	  (when (= (point) (line-beginning-position))
	    (lt-insert-current-date-string))
	  (light-save-excursion
	    (insert (lt-add-time-prefix string)))
	  (lt-propertize-lines))
	(lt-buffer-auto-shrink)))))

(defun lt-propertize-line ()
  (when (< (length (buffer-substring (line-beginning-position) (line-end-position)))
           lt-propertize-line-max-length)
    (goto-char (line-beginning-position))
    (dolist (f (append lt-faces lt-hi-list))
      (when (re-search-forward (car f) (line-end-position) t)
        (replace-match (propertize (match-string 0) 'face (cdr f)) nil t)))))

(defun lt-propertize-lines ()
  (light-save-excursion
      (lt-propertize-line)
      (while (and (= 0 (forward-line 1)) (not (= (point) (point-max))))
        (lt-propertize-line))))

(defun lt-add-time-prefix (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (= 0 (forward-line 1)) (not (= (point) (point-max))))
      (unless (= (line-beginning-position) (line-end-position))
	(forward-line 0)
	(lt-insert-current-date-string)))
    (buffer-string)))

(defun lt-clear-log-buffer ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to erase this log buffer ?")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(defun lt-send-string (str)
  (let ((process (get-buffer-process (current-buffer))))
    (if (> (length str) 10)
	(mapc (lambda (c) (progn (comint-send-string process (string c))
				 (sleep-for 0.01))) (string-to-list str))
      (comint-send-string process str))))

(defvar lt-command-history '())

(defun lt-send-command (cmd)
  (interactive (list (read-string "Command: " nil 'lt-command-history)))
  (lt-send-string (concat cmd "\r")))

(defun lt-append-to-buffer-name (string)
  (let ((name (buffer-name)))
    (rename-buffer (concat (substring name 0 (1- (length name))) "-" string "*"))))

(defvar lt-highlight-history '())

(defun lt-highlight (regexp facename)
  (interactive (list (read-regexp "Regexp" (list (word-at-point))
				  'lt-highlight-history)
		     (ido-completing-read "Face: " (mapcar (lambda (x) (propertize (car x) 'face (cdr x)))
							   lt-hi-faces))))
  (let ((face (assoc-default facename lt-hi-faces))
	(inhibit-read-only t)
	(save-point (point-max)))
    (if lt-hi-list
	(add-to-list 'lt-hi-list (cons regexp face)
		     nil (lambda (x y) (string= (car x) (car y))))
      (setq lt-hi-list (list (cons regexp face))))
    (light-save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match (propertize (match-string 0) 'face face) nil t)))))

(defun lt-clean-highlight (regexp)
  (light-save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
    (while (re-search-forward regexp nil t)
      (replace-match (propertize (match-string 0) 'face nil) nil t)))))

(defun lt-unhighlight (regexp)
  (interactive (list (ido-completing-read "Regexp: " (mapcar 'car lt-hi-list) nil t)))
  (setq lt-hi-list (delete-if (curry 'string= regexp) lt-hi-list
			      :key 'car))
  (lt-clean-highlight regexp))

(defun lt-unhighlight-all ()
  (interactive)
  (let ((save lt-hi-list))
    (setq lt-hi-list nil)
    (dolist (cur save)
      (lt-clean-highlight (car cur)))))

(defun lt-restart ()
  (interactive)
  (let ((backend (find lt-backend lt-backends :key 'lt-backend-name :test 'string=)))
    (funcall (lt-backend-restart backend))))

(defun lt-quit ()
  (interactive)
  (when (or (get-buffer-process (current-buffer))
	    (yes-or-no-p "Are you sure you want to kill this log buffer ?"))
    (kill-buffer)))

(define-derived-mode lt-mode fundamental-mode
  "log"
  (make-local-variable 'page-delimiter)
  (local-set-key (kbd "M-n") 'forward-page)
  (local-set-key (kbd "M-p") 'backward-page)
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
  (local-set-key (kbd "l") 'recenter-top-bottom)
  (local-set-key (kbd "c") 'lt-clear-log-buffer)
  (local-set-key (kbd "e") 'lt-send-command)
  (local-set-key (kbd "h") 'lt-highlight)
  (local-set-key (kbd "U") 'lt-unhighlight-all)
  (local-set-key (kbd "u") 'lt-unhighlight)
  (local-set-key (kbd "r") 'lt-restart)
  (local-set-key (kbd "q") 'lt-quit)
  (toggle-read-only t))

(defun log-tools (backend-name)
  (interactive (list (ido-completing-read "Log backend: "
					  (mapcar 'lt-backend-name lt-backends) nil t)))
  (let ((backend (find backend-name lt-backends :key 'lt-backend-name :test 'string=)))
    (with-current-buffer (get-buffer-create (format lt-buf-fmt backend-name))
      (lt-mode)
      (setq lt-backend backend-name)
      (call-interactively (lt-backend-init backend))
      (pop-to-buffer-same-window (current-buffer)))))

(defstruct lt-backend
  name
  (init 'ignore)
  (restart 'ignore))

(defun lt-register-backend (backend)
  (add-to-list 'lt-backends backend nil
	       (lambda (x y) (string= (lt-backend-name x)
				      (lt-backend-name y)))))

(provide 'log-tools)
