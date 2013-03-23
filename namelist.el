

;; Simple mode for Fortran Namelists

(defun namelist-mode ()

  (text-mode)

  (progn (setq
	  font-lock-keywords
	  (list '("^#.*$"                         . font-lock-comment-face)
		'("$.*"                           . font-lock-function-name-face)
		'("="                             . font-lock-keyword-face)
		'("[\"\']*[A-Za-z0-9\.]+[\"\']*$" . font-lock-string-face)))

	 (font-lock-mode)))
