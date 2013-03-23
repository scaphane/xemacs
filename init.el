
;; Early load the custom file

(add-to-list 'load-path (expand-file-name "~/.xemacs/"))

(load (expand-file-name "~/.xemacs/custom.el"))

(setq my-cache (expand-file-name "~/.xemacs/"))


;; Auto-save
;; =========

(setq auto-save-directory (concat my-cache "autosave/")
      auto-save-directory-fallback auto-save-directory
      auto-save-hash-p nil
      efs-auto-save t
      efs-auto-save-remotely nil
      auto-save-interval 2000
      )

;; Font locks
;; ==========

(require 'font-lock)

;; lazy-lock

(if (fboundp 'turn-on-lazy-lock)
    (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock))

(setq lazy-lock-stealth-time nil)


;; Set some variables
;; ==================

(setq initial-scratch-message nil)

(setq minibuffer-max-depth nil)

(setq scroll-step 1)

(setq column-number-start-at-one nil)

(setq frame-title-format
       '((buffer-file-name "%f")))

(setq enable-local-eval t)

;; Vertical bar mode
;; =================

(require 'vvb-mode)
(setq-default vvb-column 80
          vvb-sticky-p nil
          vvb-permanent-p t)

;; Mode hooks
;; ==========

;; C, C++

(setq c-offsets-alist
      '((innamespace . 0)       ;; don't indent for namespace
        (case-label . +)        ;; make each case line indent from switch
        ;(comment-intro . +)    ;; indent
        (inline-open . 0)       ;; don't indent inclass inline methods.
        ))

;; Set stroustrup as the default style for C/C++ code
(setq c-default-style "stroustrup")

(defun my-c-mode-common-hook ()
  (auto-fill-mode)
  ;(c-set-style "stroustrup")
  ;(c-set-offset 'substatement-open 0)
  ;(c-toggle-auto-hungry-state 1)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Set up C++ mode hook
(defun my-c++-mode-hook ()
  ;; Tell cc-mode not to check for old-style (K&R) function declarations.
  ;; This speeds up indenting a lot.
  (setq c-recognize-knr-p nil)

  ;; Automatically indent after a newline (like vi)
  (local-set-key '[(return)] 'newline-and-indent)

  ;; Tab sanity: match the C indent, but don't insert new tabs (use spaces)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)

  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)


;; Perl

(defun my-cperl-hook ()
  (auto-fill-mode))

(setq cperl-mode-hook 'my-cperl-hook)


;; Miscellaneous
;; =============

(load-default-sounds)

(require 'recent-files)
(recent-files-initialize)

(display-time)

(if (require 'scroll-in-place)
    (turn-on-scroll-in-place))

(put 'narrow-to-region 'disabled nil)

(blink-cursor-mode 1)

;; Modeline
;; ========

;; Don't need to see the buffer name - its already at the top and it obscures
;; the line and column numbers anyway.

(setq default-modeline-format
      (list
       (purecopy "")
       (purecopy "   ")
       'global-mode-string
       (cons modeline-modified-extent "%1*%1+")
       (purecopy "   %[(")
       (cons modeline-minor-mode-extent (list "" 'mode-name 'minor-mode-alist))
       (cons modeline-narrowed-extent "%n")
       'modeline-process
       (purecopy ")%] ")
       (purecopy '(line-number-mode "L%l "))
       (purecopy '(column-number-mode "C%c "))))


;; Additional Menu Buttons
;; =======================

(defun force-wrap()
  (interactive)
  (setq truncate-partial-width-windows nil)
  (toggle-truncate-lines))

;(add-menu-button nil ["Last" switch-to-other-buffer t ] "Help")
;(add-menu-button nil ["Version" vc-next-action t ] "Help")
(add-menu-button nil ["Strip" strip-trailing-whitespace t ] "Help")
(add-menu-button nil ["Revert" revert-buffer t ] "Help")
(add-menu-button nil ["Kill" kill-this-buffer t ] "Help")
(add-menu-button nil ["Wrap" force-wrap t ] "Help")

;; Mouse-wheel
;; ===========

(mwheel-install)
(setq mwheel-scroll-amount '(2 . 6))
(setq mwheel-follow-mouse t)

;; Key-bindings
;; ============

(global-set-key [f2] 'switch-to-other-buffer)
(global-set-key [f5] 'manual-entry)
(global-set-key [f9]  'auto-fill-mode)
(global-set-key [f10] 'follow-delete-other-windows-and-split)
(global-set-key [f11] 'vvb-mode)
(global-set-key [f12] 'whitespace-visual-mode)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [(control c) (control r)] 'comment-region)
(global-set-key [(control x) \2] 'split-window-switch-buffer)
(global-set-key [(control x) \3] 'hsplit-window-switch-buffer)
(global-set-key [(control return)] 'forward-buffer)
(global-set-key [(shift return)]  'backward-buffer)
(global-set-key [insert]
  (function
   (lambda () (interactive)
     (message "Overwrite mode disabled."))))



;; Make *scratch* unkillable
;; =========================

(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

;;; `default.el' automatically loaded by XEmacs after custom.el and init.el.

(load "site-functions")

(line-number-mode 1)
(column-number-mode 1)

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)


;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.
;;;
(require 'completer)


;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;; 
(setq crypt-encryption-type 'pgp   ; default encryption mechanism
      crypt-confirm-password t     ; make sure new passwords are correct
      ;crypt-never-ever-decrypt t  ; if you don't encrypt anything, set this to
                   ; tell it not to assume that "binary" files
                   ; are encrypted and require a password.
      )
(require 'crypt)


;;; Filladapt is a syntax-highlighting package.  When it is enabled it
;;; makes filling (e.g. using M-q) much much smarter about paragraphs
;;; that are indented and/or are set off with semicolons, dashes, etc.

(require 'filladapt)

(setq-default filladapt-mode t)
(when (fboundp 'turn-off-filladapt-mode)
  (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
  (add-hook 'outline-mode-hook 'turn-off-filladapt-mode))


(require 'font-lock)

;;; resize-minibuffer-mode makes the minibuffer automatically
;;; resize as necessary when it's too small to hold its contents.

(when (fboundp 'resize-minibuffer-mode)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil))

;; Load Wave Mode
;(autoload 'wave-mode "wave" "Wave Mode" t)

;(setq wave-surround-by-blank nil)

;; Load Matlab Mode
;(autoload 'matlab-mode "matlab" "Matlab mode" t)
;(autoload 'matlab-shell "matlab" "Interactive Matlab mode" t)

;; Load my Fortran Namelists Mode
;(autoload 'namelist-mode "namelist" "Fortran Namelist mode" t)

;; Load my Simtec ADF Mode
;(autoload 'simtec-adf-mode "simtec-adf" "Simtec Adf mode" t)

;; Get rid of modeline information taking up too much space -- in
;; particular, minor modes that are always enabled.
(setq pending-delete-modeline-string "")
(setq filladapt-mode-line-string "")
;; lazy-lock doesn't have a variable for its modeline name, so we have
;; to do a bit of surgery.
(and (assoc 'lazy-lock-mode minor-mode-alist)
     (setcdr (cdr (cadr (assoc 'lazy-lock-mode minor-mode-alist))) ""))


;; Automatically load version control tools.
(require 'vc)


;; Follow mode (make one big virtual window)
(autoload 'follow-mode "follow"
  "Synchronize windows showing the same buffer, minor mode." t)
(autoload 'follow-delete-other-windows-and-split "follow"
  "Delete other windows, split the frame in two, and enter Follow Mode." t)


;; Functions menubar
(cond (running-xemacs
       (require 'func-menu)
       (define-key global-map 'f8 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cl" 'fume-list-functions)
       (define-key global-map "\C-cg" 'fume-prompt-function-goto)
       (setq fume-max-items 25
             fume-fn-window-position 3
             fume-auto-position-popup t
             fume-display-in-modeline-p nil
             fume-menubar-menu-location "Strip"
             fume-buffer-name "*Function List*"
             fume-no-prompt-on-valid-default nil)))


;; Add some default major modes

(setq auto-mode-alist
      (append
       '(("\\.for$"    . fortran-mode)
         ("\\.inc$"    . fortran-mode)
         ("\\.h$"      . c++-mode)
         ("\\.icc$"    . c++-mode)
         ("\\.m$"      . matlab-mode)
         ("\\.perl$"   . cperl-mode)
         ("\\.pod$"    . cperl-mode)
         ("\\.txt$"    . auto-fill-mode)
         ("\\.scons$"  . python-mode)
         ("SConstruct" . python-mode)
         ("SConscript" . python-mode)
         ("Rakefile"   . ruby-mode)
         ("rakefile"   . ruby-mode)
         ) auto-mode-alist))

;; Start gnuserve
(gnuserv-start)

(setq-default truncate-partial-width-windows nil)
