
;; Simple mode for SIMTEC ADF files

(defun simtec-adf-mode ()

  (text-mode)

  (progn (setq
      font-lock-keywords
      (list '("^\\[.*?\\]$"                  . font-lock-comment-face)
            '("[\"\'][A-Za-z0-9\.]+[\"\']"   . font-lock-string-face)
            '("^OBJECT:"                     . green)
            '("^END_OBJECT:"                 . green)
;           '("\\b[A-Z][a-z0-9_]+\\b"        . red)
            '("^\\s-+[A-Za-z_]+:"            . blue)

            ))

     (font-lock-mode)))
