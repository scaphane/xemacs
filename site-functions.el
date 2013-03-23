
;;; Define some useful functions

(defun kill-buffers (regexp) 
  (interactive "sKill buffers: ")
  "Kill buffers matching REGEXP."
  (mapcar
   (function
    (lambda (buffer) 
      (let ((name (buffer-file-name buffer)))
        (if (and name (string-match regexp name))
            (kill-buffer buffer)))))
   (buffer-list)))

(defun backward-buffer () (interactive)
  "Switch to previously selected buffer."
  (let* ((list (cdr (buffer-list)))
         (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
        (setq list (cdr list))
        (setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

(defun forward-buffer () (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
         (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
        (setq list (cdr list))
        (setq buffer (car list))))
    (switch-to-buffer buffer)))

(defun split-window-switch-buffer () (interactive)
  "Split current window and display the two last buffers used."
  (split-window)
  (switch-to-buffer (other-buffer (current-buffer)))
  )

(defun hsplit-window-switch-buffer () (interactive)
  "Split current window horizontally and display the two last buffers used."
  (split-window-horizontally)
  (switch-to-buffer (other-buffer (current-buffer)))
  )

(defun kill-scratch-buffer ()
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  nil)

(defun strip-trailing-whitespace() (interactive)
  "Strip trailing whitespace from the end of everyline."
  (replace-regexp "\ +$" "")

)