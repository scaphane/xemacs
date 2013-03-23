;;
;; $Id: wave-mode-hook.el,v 1.1 2005/09/30 09:15:11 scaphane Exp $
;; Example of wave-mode-hook function.
;;
(setq wave-mode-hook
      (function (lambda ()
		  ;; indenting for blocks, continuation lines
		  (setq wave-block-indent 3  
			wave-continuation-indent 2
			wave-minimum-statement-indent 0
			wave-newline-and-indent t
			wave-surround-by-blank t
			wave-upcase-procedure-name t
			wave-upcase-common-name t))))
