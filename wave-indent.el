;;
;; $Id: wave-indent.el,v 1.1 2005/09/30 09:15:11 scaphane Exp $
;; Template for batch modification using wave-mode
;;
;; Usage:  emacs -batch <file.pro> -l wave-indent.el
;;
(load "wave.el")
(wave-mode)
(wave-indent-buffer)
(save-buffer)


