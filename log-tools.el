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

(defcustom lt-time-fmt "%H:%M:%S.%3N"
  "Format for timestamp prefix displayed at the beginning of each
log line."
  :type '(string))

(defcustom lt-max-line-nb 100000
  "Maximum number of line of the log buffer."
  :type '(integer))

(defcustom lt-delete-line-nb 1000
  "Number of line to delete at once to maintain the maximum
number of line, see `lt-max-line-nb'."
  :type '(integer))

(defcustom lt-propertize-line-max-length 200
  "Maximum line length allowed to be propertized.  If the a line
length is larger than this value it won't be propertized."
  :type '(integer))

(defcustom lt-buf-fmt "*lt:%s*"
  "Log tools buffer name format."
  :type '(string))

(defvar-local lt-faces
  '((".*\\(error\\|fail\\|denied\\).*$"	. 'error)))

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
(defvar-local lt-trigger-list '())
(defvar-local lt-input-marker nil)
(defvar-local lt-separator-marker nil)

(defcustom lt-separator "\n"
  "String separating the logs and the input field."
  :type '(string))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst icurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (interactive) (apply function (append arguments more)))))

(defmacro light-save-excursion (&rest body)
  (declare (indent 0))
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
     (let ((end (progn (goto-char (point-min))
		       (forward-line lt-delete-line-nb)
		       (point))))
      (mapcar 'delete-overlay (overlays-in (point-min) end))
      (delete-region (point-min) end)))))

(defun lt-setup-window-margin ()
  (set-window-margins (get-buffer-window (current-buffer)) (length (format-time-string lt-time-fmt (current-time))) (cdr (window-margins (get-buffer-window (current-buffer))))))

(defvar-local lt-initial-timestamp '(0 0 0 0))

(defun lt-set-initial-timestamp ()
  (interactive)
  (setq lt-initial-timestamp
	(overlay-get (car (overlays-in (line-beginning-position) (line-end-position)))
		     'time))
  (lt-update-overlay lt-time-fmt))

(defun lt-update-overlay (time-format)
  (interactive (list (read-string "Time Format: " lt-time-fmt)))
  (setq lt-time-fmt time-format)
  (set-window-margins (get-buffer-window (current-buffer)) (length (format-time-string lt-time-fmt (current-time))) (cdr (window-margins (get-buffer-window (current-buffer)))))
  (dolist (ov (overlays-in (point-min) (marker-position lt-separator-marker)))
    (let ((str (propertize " " 'display
			   `((margin left-margin)
			     ,(format-time-string lt-time-fmt
						  (time-subtract (overlay-get ov 'time)
								 lt-initial-timestamp)))
			   'face 'font-lock-comment-face)))
      (overlay-put ov 'before-string str))))

(defsubst lt-insert-current-date-string (time)
  (let ((time))
    (unless time
      (setf time (current-time)))
    (unless (get-text-property (line-beginning-position) 'time)
      (let ((props (list 'time time
			 'line-prefix (propertize " "
						  'display
						  `((margin left-margin)
						    ,(propertize (format-time-string lt-time-fmt time) 'face 'font-lock-comment-face))))))
	(add-text-properties (line-beginning-position) (line-end-position) props)))))

(defun lt-insert-string-in-log-buffer (buf string)
  (let (time (current-time))
    (when (and (> (length string) 0) (get-buffer buf))
      (with-current-buffer buf
	(let ((inhibit-read-only t))
	  (light-save-excursion-if-not-at-point-max buf
	    (goto-char (1- (marker-position lt-separator-marker)))
	    (when (= (point) (line-beginning-position))
	      (lt-insert-current-date-string time))
	    (light-save-excursion
	      (lt-add-time-prefix string time))
	    (lt-trigger-if-match)
	    (lt-propertize-lines)))))))
	;; (lt-buffer-auto-shrink)))))

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
      (while (and (= 0 (forward-line 1)) (< (point) (marker-position lt-separator-marker)))
        (lt-propertize-line))))

(defun lt-trigger-if-match ()
  (dolist (trigger lt-trigger-list)
    (light-save-excursion
    (goto-char (line-beginning-position))
      (when (re-search-forward (car trigger) nil t)
	(funcall (cdr trigger))))))

(defun lt-add-time-prefix (string time)
  (let ((start (point)))
    (insert-before-markers (propertize string 'read-only t 'front-sticky t 'rear-nonsticky t))
    (goto-char start)
    (while (and (< (point) (marker-position lt-separator-marker)))
      (unless (= (line-beginning-position) (line-end-position))
	(forward-line 0)
	(lt-insert-current-date-string time))
      (forward-line 1))))

(defun lt-clear-log-buffer ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to erase this log buffer ?")
    (mapcar 'delete-overlay (overlays-in (point-min)
					 (marker-position lt-separator-marker)))
    (let ((inhibit-read-only t))
      (delete-region (point-min) (1- (marker-position lt-separator-marker))))))

(defun lt-send-string (str)
  (let ((process (get-buffer-process (current-buffer))))
    (if (> (length str) 1)
	(dolist (c (string-to-list str))
	  (comint-send-string process (string c))
	  ;; (shell-command "sleep .05")
	  )
      (comint-send-string process str))))

(defvar lt-command-history '())
(defvar-local lt-command-history-cur 0)
(defvar-local lt-command-current nil)

(defun lt-send-command (cmd)
  (interactive (list (read-string "Command: " nil 'lt-command-history)))
  (unless (get-buffer-process (current-buffer))
    ;; (when (yes-or-no-p "No process, do you want to restart ?")
    (lt-restart))
  (lt-send-string (concat cmd "\r")))

(defun lt-new-send-command ()
  (interactive)
  (let ((win (split-window-below -4)))
    (set-window-buffer win (get-buffer-create "*Command*"))
    (select-window win)))

(defun lt-append-to-buffer-name (string)
  (let ((name (buffer-name)))
    (rename-buffer (concat (substring name 0 (1- (length name))) "-" string "*"))))

(defvar lt-highlight-history '())

(defun lt-highlight (regexp facename)
  (interactive (list (read-regexp "Regexp" (list (word-at-point))
				  'lt-highlight-history)
		     (completing-read "Face: " (mapcar (lambda (x) (propertize (car x) 'face (cdr x)))
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

(defvar lt-trigger-regexp-history '())
(defvar lt-trigger-symbol-history '())
(defun lt-trigger (regexp fun-name)
  (interactive (list (read-regexp "Regexp" (list (word-at-point))
				  'lt-trigger-history)
		     (read-string "Symbol: " nil 'lt-trigger-symbol-history)))
  (if lt-trigger-list
      (add-to-list 'lt-trigger-list (cons regexp (intern fun-name)))
    (setq lt-trigger-list (list (cons regexp (intern fun-name))))))

(defun lt-clean-highlight (regexp)
  (light-save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
    (while (re-search-forward regexp nil t)
      (replace-match (propertize (match-string 0) 'face nil) nil t)))))

(defun lt-unhighlight (regexp)
  (interactive (list (completing-read "Regexp: " (mapcar 'car lt-hi-list) nil t)))
  (setq lt-hi-list (delete-if (curry 'string= regexp) lt-hi-list
			      :key 'car))
  (lt-clean-highlight regexp))

(defun lt-unhighlight-all ()
  (interactive)
  (let ((save lt-hi-list))
    (setq lt-hi-list nil)
    (dolist (cur save)
      (lt-clean-highlight (car cur)))))

(defun lt-untrigger ()
  (interactive)
  (setq lt-trigger-list nil))

(defun lt-restart ()
  (interactive)
  (let ((backend (find lt-backend lt-backends :key 'lt-backend-name :test 'string=)))
    (funcall (lt-backend-restart backend))))

(defun lt-quit ()
  (interactive)
  (when (or (get-buffer-process (current-buffer))
	    (yes-or-no-p "Are you sure you want to kill this log buffer ?"))
    (kill-buffer)))

(defun lt-interactive-send-command ()
  (interactive)
  (insert (ido-completing-read "Command: " lt-command-history)))

(define-derived-mode lt-mode fundamental-mode
  "log"
  (make-local-variable 'page-delimiter)
  (local-set-key (kbd "M-n") 'lt-buffer-next-msg)
  (local-set-key (kbd "M-p") 'lt-buffer-prev-msg)
  (local-set-key (kbd "RET") 'lt-send-input)
  (local-set-key (kbd "M-r") 'lt-interactive-send-command))

(defun lt-send-input ()
  (interactive)
  (let ((cmd (buffer-substring (marker-position lt-input-marker)
			       (point-max))))
    (add-to-history 'lt-command-history cmd)
    (lt-send-command cmd)
    (setf lt-command-history-cur 0)
    (delete-region (marker-position lt-input-marker) (point-max))))

(defun log-tools (backend-name)
  (interactive (list (completing-read "Log backend: "
					  (mapcar 'lt-backend-name lt-backends) nil t)))
  (let ((backend (find backend-name lt-backends :key 'lt-backend-name :test 'string=)))
    (with-current-buffer (get-buffer-create (format lt-buf-fmt backend-name))
      (lt-mode)
      (insert "\n")
      (setq lt-separator-marker (point-marker))
      (insert (propertize "▶ "
			  'read-only t
			  'inhibit-line-move-field-capture t
			  'field 'output
			  'rear-nonsticky t
			  'front-sticky '(field inhibit-line-move-field-capture)))
      (setq lt-input-marker (point-marker))
      (setq lt-backend backend-name)
      (call-interactively (lt-backend-init backend))
      (pop-to-buffer-same-window (current-buffer))
      (setq-local window-configuration-change-hook '(lt-my-update))
      (lt-update-overlay lt-time-fmt))))

(defstruct lt-backend
  name
  (init 'ignore)
  (restart 'ignore))

(defun lt-register-backend (backend)
  (add-to-list 'lt-backends backend nil
	       (lambda (x y) (string= (lt-backend-name x)
				      (lt-backend-name y)))))

(defun lt-measure-region (beg end)
  (interactive "r")
  (setf beg (save-excursion (goto-char beg) (line-beginning-position)))
  (setf end (save-excursion (goto-char end) (line-beginning-position)))
  (let* ((delta (time-subtract (get-text-property end 'time)
			       (get-text-property beg 'time))))
      (message (format-time-string "%H:%M:%S.%6N" delta t))))

(remove-hook 'activate-mark-hook 'lt-activate-mark-hook)
(remove-hook 'deactivate-mark-hook 'lt-activate-mark-hook)

(defun lt-activate-mark-hook ()
  (when (eq major-mode 'lt-mode)
    (lt-measure-region (region-beginning) (region-end))))

(defun lt-my-update ()
  (lt-update-overlay "%k:%M:%S.%6N"))

(defun lt-buffer-replace-command (msg)
  (delete-region (marker-position lt-input-marker) (point-max))
  (goto-char (marker-position lt-input-marker))
  (insert msg))

(defun lt-history-move (n)
  (when lt-command-history
    (let ((cur (+ lt-command-history-cur n)))
      (when (and (>= cur 0) (<= cur (length lt-command-history)))
	(when (zerop lt-command-history-cur)
	  (setq lt-command-current (delete-and-extract-region (marker-position lt-input-marker) (point-max))))
	(lt-buffer-replace-command (if (zerop cur)
				       lt-command-current
				     (nth (1- cur) lt-command-history)))
	(incf lt-command-history-cur n)))))

(defun lt-buffer-prev-msg ()
  (interactive)
  (lt-history-move +1))

(defun lt-buffer-next-msg ()
  (interactive)
  (lt-history-move -1))

(provide 'log-tools)
