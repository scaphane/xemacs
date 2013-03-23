;;; WAVE CL mode for GNU Emacs  (beta test version 0.1 11/10/92)
;;; Copyright (c) Lubos Pochman, Precision Visuals, Boulder.
;;; Written by Lubos Pochman, Precision Visuals, Boulder.
;;; Inspired by fortran-mode and older version of wave-mode from
;;; Joe Wingerd, Precision Visuals, Boulder.
;;;
;;; $Id: wave.el,v 1.1 2005/09/30 09:15:11 scaphane Exp $
;;;
;;; This file is not part of the GNU Emacs distribution.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Bugs to support@boulder.vni.com

;;; Revision History:
;;; 0.1 - Initial version Inspired by fortran-mode 11/15/92
;;; 0.2 - Added functionality from old wave-mode (JW) 11/20/92
;;;       Single, Double quotes, case conversion, spacing etc.
;;; 0.3 - Fixed bugs in input mode. Added block statement matching
;;;       abbrev, conversion etc. expansion on indent.
;;;       Released for internal Beta testing 12/7/92
;;; 0.4 - Fixed bugs in input mode (space padding, abbrev prefix handling).
;;;       Added M-TAB for hard tab, wave-surround-by-blank flag variable.
;;; 0.5   First publically released Beta version. 1/27/93
;;; 0.6 - Fixed infinite loop bug. 7/13/93
;;;       Fixed system abbrevs clash with structure field reference.
;;;       Fixed wrong line split/newline when line not formatted.
;;;       Added join line feature with continuation line handling (ESC^).
;;; 0.7 - Dave Landers, Oct 96:
;;;       Added lots more wave keywords for case conversion and highlighting.
;;;       Changed some highlighting face bindings.
;;;       Fixed indenting when comment char (;) in a string 
;;;          (wave-indent-parallel-comment).
;;;        Fixed wave-split-line (M-RET) - knows more about comment.
;;;        DECLARE FUNC upcases function names like PRO and FUNCTION.
;;;        Two new variables:
;;;            wave-upcase-procedure-name: Capitalize or UPCASE function names 
;;;                 on PRO, FUNCTION, DECLARE_FUNC:  t=UPCASE / nil=Capitalize
;;;            wave-upcase-common-name: Capitalize or UPCASE common names in 
;;;                 COMMON statement:  t=UPCASE / nil=Capitalize
;;;          Default for both of these is nil (same behavior as before).
;;;        Fixed wave-is-continue for the case like this:  ` ; $ ; '.
;;;        Fixed wave-indent-subprogram (used to indent only to point, now 
;;;            indents to end of program).
;;;        Added wave-indent-buffer (C-c TAB) to indent the entire buffer.
;;;        Changed binding of wave-abbrev-help from M-? to C-?.

(defvar wave-block-indent 3
  "*Extra indentation applied to blocks lines.")

(defvar wave-continuation-indent 2
  "*Extra indentation applied to continuation lines.")

(defvar comment-line-start nil
  "Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip nil
  "Regexp to match the start of a full-line comment.")

(defvar wave-minimum-statement-indent 0
  "*Minimum indentation for wave statements.")

(defvar wave-newline-and-indent t
  "*Automatically indents current line, inserts newline and indents it.")

(defvar wave-surround-by-blank t
  "*Automatically surrounds '=','<','>' with blanks, appends blank to comma.")

(defvar wave-upcase-procedure-name nil
  "*UPPERCASE (t) or Capitalize (nil) function and procedure names.")

(defvar wave-upcase-common-name nil
  "*UPPERCASE (t) or Capitalize (nil) common block names.")


(defvar wave-indent-action-table nil
  "Associated array containing lists of search string (car), and function as a cdr.
'wave-indent-line' function searches the table and if match is found, executes the function.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar wave-comment-indent-char ? 
  "Character to be inserted for Wave comment indentation.
Normally a space.")

(defvar wave-continuation-char ?$
  "Character which is inserted as a last character on previous line by
   \\[wave-split-line] to begin a continuation line.  Normally $.")

(defvar wave-comment-region ";; "
  "*String inserted by \\[wave-comment-region] at start of each line in region.")

(defvar wave-doclib-start "^[;*]\\+"
  "*Start of document library header.")

(defvar wave-doclib-end "^[;*]-"
  "*End of document library header.")

(defvar wave-block-match-back-max 40
  "*Maximum number of lines to search for matching block delimiter to blocks lines.")

(defvar wave-block-match nil
  "Internal flag indicating if search for match block is enabled.")

(defvar wave-block-start-delimiters nil
  "Regexp defining list of delimiters starting a block.")

(defvar wave-block-end-delimiters nil
  "Regexp defining list of delimiters ending a block.")

(defvar wave-block-delimiters nil
  "Associated list of WAVE CL command block delimiters.
Car is the key containing end delimiter,
cdr is the list containing 2-3 begin delimiters.")

(defvar wave-dquote-location 0
  "Dquote-location - location in the buffer of a opening double quote")
(defvar wave-squote-location 0
  "Squote-location - location in the buffer of a opening single quote")
(defvar wave-sysvar-mode nil
  "Sysvar-mode - true between a '!' and a newline")

(defvar wave-startup-message t
  "*Non-nil displays a startup message when wave-mode is first called.")

(defconst wave-mode-version "0.7")

(defvar wave-mode-syntax-table nil
  "Syntax table in use in wave-mode buffers.")

(if wave-mode-syntax-table
    ()
  (setq wave-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?+   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?-   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?*   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?/   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?^   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?#   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?=   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?%   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?<   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?>   "."  wave-mode-syntax-table)
  (modify-syntax-entry ?\'  "\"" wave-mode-syntax-table)
  (modify-syntax-entry ?\"  "\"" wave-mode-syntax-table)
  (modify-syntax-entry ?\\  "\\" wave-mode-syntax-table)
  (modify-syntax-entry ?_   "w"  wave-mode-syntax-table)
  (modify-syntax-entry ?$   "w"  wave-mode-syntax-table)
  (modify-syntax-entry ?.   "w"  wave-mode-syntax-table)
  (modify-syntax-entry ?\;  "<"  wave-mode-syntax-table)
  (modify-syntax-entry ?\n  ">"  wave-mode-syntax-table))

(defvar wave-mode-map () 
  "Keymap used in wave mode.")

(if wave-mode-map
    ()
  (setq wave-mode-map (make-sparse-keymap))
  (define-key wave-mode-map ";"        'wave-comment)
  (define-key wave-mode-map "!"        'wave-sysvar)
  (define-key wave-mode-map "'"        'wave-squote)
  (define-key wave-mode-map "\""       'wave-dquote)
  (define-key wave-mode-map "0"        'wave-octal-number)
  (define-key wave-mode-map "1"        'wave-octal-number)
  (define-key wave-mode-map "2"        'wave-octal-number)
  (define-key wave-mode-map "3"        'wave-octal-number)
  (define-key wave-mode-map "4"        'wave-octal-number)
  (define-key wave-mode-map "5"        'wave-octal-number)
  (define-key wave-mode-map "6"        'wave-octal-number)
  (define-key wave-mode-map "7"        'wave-octal-number)
  (define-key wave-mode-map "\C-?"     'wave-abbrev-help)
  (define-key wave-mode-map "\M-\t"    'wave-hard-tab)
  (define-key wave-mode-map "\C-?"     'wave-delete-char)
  (define-key wave-mode-map "\C-c;"    'wave-comment-region)
  (define-key wave-mode-map "\C-\M-a"  'beginning-of-wave-subprogram)
  (define-key wave-mode-map "\C-\M-e"  'end-of-wave-subprogram)
  (define-key wave-mode-map "\M-;"     'wave-indent-comment)
  (define-key wave-mode-map "\M-\C-h"  'mark-wave-subprogram)
  (define-key wave-mode-map "\M-\C-d"  'mark-wave-doclib)
  (define-key wave-mode-map "\M-\r"    'wave-split-line)
  (define-key wave-mode-map "\M-^"     'wave-join-line)
  (define-key wave-mode-map "\M-\C-q"  'wave-indent-subprogram)
  (define-key wave-mode-map "\C-c\t"   'wave-indent-buffer)
  (define-key wave-mode-map "\C-c\C-p" 'wave-previous-statement)
  (define-key wave-mode-map "\C-c\C-n" 'wave-next-statement)
  (define-key wave-mode-map "\r"       'wave-newline)
  (define-key wave-mode-map "\t"       'wave-indent-line))

(defun wave-back-6 () (if (wave-check-system-abbrev) (backward-char 6)))
(defun wave-back-3 () (if (wave-check-system-abbrev) (backward-char 3)))
(defun wave-back-2 () (if (wave-check-system-abbrev) (backward-char 2)))

(defvar wave-mode-abbrev-table nil)
(if wave-mode-abbrev-table
    ()
  (define-abbrev-table 'wave-mode-abbrev-table ())
  (let ((abbrevs-changed nil))
;;
;; Keywords, system functions, conversion routines
;;
    (define-abbrev wave-mode-abbrev-table  ".b"   "BEGIN" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".co"  "COMMON" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".c"   "CASE OF" 'wave-back-3)
    (define-abbrev wave-mode-abbrev-table  ".cb"  "BYTE( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".cx"  "FIX( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".cl"  "LONG( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".cf"  "FLOAT( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".cs"  "STRING( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".cc"  "COMPLEX( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".cd"  "DOUBLE( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".d"   "DO" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".e"   "ELSE" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ec"  "ENDCASE" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ee"  "ENDELSE" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ef"  "ENDFOR" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ei"  "ENDIF ELSE IF" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".el"  "ENDIF ELSE" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".en"  "ENDIF" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".er"  "ENDREP" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ew"  "ENDWHILE" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".f"   "FOR DO" 'wave-back-3)
    (define-abbrev wave-mode-abbrev-table  ".fu"  "FUNCTION" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".g"   "GOTO," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".h"   "HELP," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".i"   "IF" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".k"   "KEYWORD_SET( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".n"   "N_ELEMENTS( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".on"  "ON_ERROR," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".oi"  "ON_IOERROR," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ow"  "OPENW," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".or"  "OPENR," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ou"  "OPENU," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".p"   "PRO" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".pr"  "PRINT," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".pt"  "PLOT," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".r"   "REPEAT UNTIL" 'wave-back-6)
    (define-abbrev wave-mode-abbrev-table  ".re"  "READ," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".rf"  "READF," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".ru"  "READU," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".rt"  "RETURN" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".sc"  "STRCOMPRESS( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".sn"  "STRLEN( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".sl"  "STRLOWCASE( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".su"  "STRUPCASE( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".sm"  "STRMID( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".sp"  "STRPOS( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".st"  "STRPUT( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".sr"  "STRTRIM( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".t"   "THEN" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".u"   "UNTIL" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".wu"  "WRITEU," 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".w"   "WHILE DO" 'wave-back-3)
;;
;;  WAVE toolbox and widgets
;;
    (define-abbrev wave-mode-abbrev-table  ".tac"  "WtAddCallback( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tah"  "WtAddHandler( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tca"  "WtCallback( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tcl"  "WtClose( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tc"   "WtCreate( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tg"   "WtGet( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".ti"   "WtInit( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tl"   "WtList( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tlo"  "WtLoop( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tml"  "WtMainLoop( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tp"   "WtPointer( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".ts"   "WtSet( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".tt"   "WtTimer( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wb"   "WwButtonBox( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wcm"  "WwCommand( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wco"  "WwControlBox( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wdi"  "WwDialog( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wdr"  "WwDrawing( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wf"   "WwFileSelection( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wg"   "WwGetValue( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wi"   "WwInit( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wla"  "WwLayout( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wli"  "WwList( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wlo"  "WwLoop" 'wave-check-system-abbrev)
    (define-abbrev wave-mode-abbrev-table  ".wmw"  "WwMainWindow( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wmb"  "WwMenuBar( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wm"   "WwMessage( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wo"   "WwOptionMenu( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wp"   "WwPopupMenu( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wr"   "WwRadioBox( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".ws"   "WwSetValue( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wt"   "WwText( )" 'wave-back-2)
    (define-abbrev wave-mode-abbrev-table  ".wto"  "WwToolBox( )" 'wave-back-2)
;;
;; Handle lower to upper conversion for reserved words
;;
  (define-abbrev wave-mode-abbrev-table "and"      "AND"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "begin"    "BEGIN"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "case"     "CASE"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "common"   "COMMON"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "do"       "DO"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "else"     "ELSE"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "END"      "END"       'wave-show-begin)
  (define-abbrev wave-mode-abbrev-table "end"      "END"       'wave-show-begin)
  (define-abbrev wave-mode-abbrev-table "ENDCASE"  "ENDCASE"   'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "endcase"  "ENDCASE"   'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "ENDELSE"  "ENDELSE"   'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "endelse"  "ENDELSE"   'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "ENDFOR"   "ENDFOR"    'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "endfor"   "ENDFOR"    'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "ENDIF"    "ENDIF"     'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "endif"    "ENDIF"     'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "ENDREP"   "ENDREP"    'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "endrep"   "ENDREP"    'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "ENDWHILE" "ENDWHILE"  'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "endwhile" "ENDWHILE"  'wave-show-command)
  (define-abbrev wave-mode-abbrev-table "eq"       "EQ"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "for"      "FOR"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "function" "FUNCTION"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "ge"       "GE"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "goto"     "GOTO"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "gt"       "GT"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "if"       "IF"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "le"       "LE"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "lt"       "LT"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "mod"      "MOD"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "ne"       "NE"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "not"      "NOT"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "of"       "OF"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "on_ioerror" "ON_IOERROR" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "on_error_goto" "ON_ERROR_GOTO" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "or"       "OR"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "pro"      "PRO"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "repeat"   "REPEAT"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "return"   "RETURN"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "then"     "THEN"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "until"    "UNTIL"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "while"    "WHILE"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "xor"      "XOR"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "declare"  "DECLARE"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "func"     "FUNC"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".size"    ".SIZE"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".locals"  ".LOCALS"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "..locals" "..LOCALS"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".run"     ".RUN"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".con"     ".CON"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".skip"    ".SKIP"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".step"    ".STEP"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table ".rnew"    ".RNEW"     'wave-check-abbrev)
;;
;; Lower to Upper conversion for some Kernal Procedures
;;   Added by Dave Landers - Sep 96
;; 
  (define-abbrev wave-mode-abbrev-table "addvar"	"ADDVAR" 	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "axis"		"AXIS"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "byteorder"	"BYTEORDER"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "cd"		"CD"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "close"		"CLOSE"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "compile"	"COMPILE"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "contour"	"CONTOUR"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "cursor"	"CURSOR"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "device"	"DEVICE"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "empty"		"EMPTY"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "erase"		"ERASE"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "exit"		"EXIT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "flush"		"FLUSH"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "free_lun"	"FREE_LUN"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "get_lun"	"GET_LUN"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "info"		"INFO"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "journal"	"JOURNAL"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "message"	"MESSAGE"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "on_error"	"ON_ERROR" 	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "openr"		"OPENR"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "openu"		"OPENU"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "openw"		"OPENW"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "oplot"		"OPLOT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "plot"		"PLOT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "plots"		"PLOTS"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "plot_io"	"PLOT_IO"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "plot_oi"	"PLOT_OI"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "plot_oo"	"PLOT_OO"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "point_lun"	"POINT_LUN"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "polyfill"	"POLYFILL"     	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "print"		"PRINT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "printf"	"PRINTF"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "read"		"READ"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "readf"		"READF"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "readu"		"READU"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "restore"	"RESTORE"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "save"		"SAVE"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "set_plot"	"SET_PLOT"     	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "shade_surf"	"SHADE_SURF"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "spawn"		"SPAWN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "surface"	"SURFACE"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tv"		"TV"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tvlct"		"TVLCT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "upvar"		"UPVAR"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wait"		"WAIT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wdelete"	"WDELETE"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "window"	"WINDOW"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "writeu"	"WRITEU"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wset"		"WSET"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wshow"		"WSHOW"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "xyouts"	"XYOUTS"       	'wave-check-abbrev)
;;
;; Lower to upper conversion for some Kernal Funcitons
;;   Added by Dave Landers - Sep 96
;;
  (define-abbrev wave-mode-abbrev-table "abs"		"ABS"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "acos"		"ACOS"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "alog"		"ALOG"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "alog10"       	"ALOG10"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "asarr"		"ASARR"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "asin"		"ASIN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "askeys"       	"ASKEYS"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "assoc"		"ASSOC"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "atan"		"ATAN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "bindgen"      	"BINDGEN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "bytarr"       	"BYTARR"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "byte"		"BYTE"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "bytscl"       	"BYTSCL"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "check_math"   	"CHECK_MATH"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "cindgen"      	"CINDGEN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "complex"      	"COMPLEX"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "complexarr"   	"COMPLEXARR"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "conj"		"CONJ"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "cos"		"COS"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "cosh"		"COSH"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dblarr"       	"DBLARR"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dc_read_fixed"	"DC_READ_FIXED"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dc_read_free"	"DC_READ_FREE"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dc_write_fixed" "DC_WRITE_FIXED" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dc_write_free"	"DC_WRITE_FREE"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dindgen"      	"DINDGEN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "double"       	"DOUBLE"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "eof"		"EOF"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "execute"      	"EXECUTE"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "exp"		"EXP"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "findfile"     	"FINDFILE"     	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "findgen"      	"FINDGEN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "finite"       	"FINITE"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "fix"		"FIX"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "float"		"FLOAT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "fltarr"       	"FLTARR"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "fstat"		"FSTAT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "imaginary"    	"IMAGINARY"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "indgen"       	"INDGEN"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "intarr"       	"INTARR"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "isaskey"      	"ISASKEY"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "ishft"		"ISHFT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "keyword_set"  	"KEYWORD_SET"  	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "lindgen"      	"LINDGEN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "linknload"    	"LINKNLOAD"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "list"		"LIST"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "lonarr"       	"LONARR"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "long"		"LONG"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "make_array"   	"MAKE_ARRAY"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "max"		"MAX"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "median"       	"MEDIAN"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "min"		"MIN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "n_elements"   	"N_ELEMENTS"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "n_params"     	"N_PARAMS"     	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "n_tags"       	"N_TAGS"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "param_present"	"PARAM_PRESENT"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "polyshade"    	"POLYSHADE"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "randomn"      	"RANDOMN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "randomu"      	"RANDOMU"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "rebin"		"REBIN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "reform"       	"REFORM"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "replicate"    	"REPLICATE"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "shift"		"SHIFT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "sin"		"SIN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "sindgen"      	"SINDGEN"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "sinh"		"SINH"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "size"		"SIZE"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "smooth"       	"SMOOTH"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "sort"		"SORT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "sqrt"		"SQRT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strarr"       	"STRARR"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strcompress"  	"STRCOMPRESS"  	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "string"       	"STRING"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strlen"       	"STRLEN"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strlookup"    	"STRLOOKUP"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strlowcase"   	"STRLOWCASE"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strmatch"     	"STRMATCH"     	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strmessage"   	"STRMESSAGE"   	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strmid"       	"STRMID"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strpos"       	"STRPOS"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strtrim"      	"STRTRIM"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strupcase"    	"STRUPCASE"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "systime"      	"SYSTIME"      	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tag_names"    	"TAG_NAMES"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tan"		"TAN"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tanh"		"TANH"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "today"		"TODAY"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "total"		"TOTAL"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "transpose"    	"TRANSPOSE"    	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tvrd"		"TVRD"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "unique"       	"UNIQUE"       	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "where"		"WHERE"		'wave-check-abbrev)
;;
;; Widget routines
;;    Added by Dave Landers - Sep 96
;;
  (define-abbrev wave-mode-abbrev-table "wwalert"      	"WwAlert"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwbuttonbox"  	"WwButtonBox"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwcallback"   	"WwCallback"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwcommand"    	"WwCommand"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwcontrolsbox"	"WwControlsBox"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwdialog"     	"WwDialog"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwdrawing"    	"WwDrawing"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwfileselection" "WwFileSelection" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwgenericdialog" "WwGenericDialog" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwgetbutton"	"WwGetButton"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwgetkey"     	"WwGetKey"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwgetposition"	"WwGetPosition"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwgetvalue"   	"WwGetValue"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwhandler"    	"WwHandler"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwinit"       	"WwInit"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwlayout"     	"WwLayout"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwlist"       	"WwList"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwlistutils"  	"WwListUtils"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwloop"       	"WwLoop"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwmainwindow" 	"WwMainWindow"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwmenubar"    	"WwMenuBar"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwmenuitem"   	"WwMenuItem"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwmessage"    	"WwMessage"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwmulticlickhandler" "WwMultiClickHandler" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwoptionmenu"  "WwOptionMenu"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwpopupmenu"  	"WwPopupMenu"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwpreview"    	"WwPreview"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwpreviewutils" "WwPreviewUtils" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwradiobox"    "WwRadioBox"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwresource"   	"WwResource"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwsetcursor"  	"WwSetCursor"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwsetvalue"   	"WwSetValue"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwtable"      	"WwTable"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwtableutils" 	"WwTableUtils"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwtext"       	"WwText"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwtimer"      	"WwTimer"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wwtoolbox"    	"WwToolBox"	'wave-check-abbrev)
;;
;; VDA (Tm and Wo) routines
;;

  (define-abbrev wave-mode-abbrev-table "tmaddselectedvars" "TmAddSelectedVars" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmaddvar"      "TmAddVar"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmcodegen"     "TmCodeGen"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmcopy"        "TmCopy"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmcut"         "TmCut"         'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmdelvar"      "TmDelVar"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmdelete"      "TmDelete"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmdeselectvars" "TmDeselectVars" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmdynamicdisplay" "TmDynamicDisplay" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmendcodegen"  "TmEndCodeGen"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumerateattributes" "TmEnumerateAttributes" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumerateitems" "TmEnumerateItems" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumeratemethods" "TmEnumerateMethods" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumerateselectedvars" "TmEnumerateSelectedVars" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumeratetoolnames" "TmEnumerateToolNames" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumeratevars" "TmEnumerateVars" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmexecutemethod" "TmExecuteMethod" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmexport"      "TmExport"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmexportselection"      "TmExportSelection" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetattribute" "TmGetAttribute" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetmessage"  "TmGetMessage"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetmethod"   "TmGetMethod"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgettop"      "TmGetTop"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetuniquetoolname" "TmGetUniqueToolName" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetvarmainname" "TmGetVarMainName" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tminit"        "TmInit"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlist"        "TmList"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistappend"  "TmListAppend"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistclear"   "TmListClear"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistdelete"  "TmListDelete"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistdestroy" "TmListDestroy" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistextend"  "TmListExtend"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistgetmethod" "TmListGetMethod" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistinsert"  "TmListInsert"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistreplace" "TmListReplace" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistretrieve" "TmListRetrieve" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlistsetmethod" "TmListSetMethod" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmpaste"       "TmPaste"       'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmregister"    "TmRegister"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmrestoretemplate" "TmRestoreTemplate" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmrestoretools" "TmRestoreTools" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmsavetools"   "TmSaveTools"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmsetattribute" "TmSetAttribute" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmsetmethod"   "TmSetMethod"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmstartcodegen" "TmStartCodeGen" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmunregister"  "TmUnregister"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmaddgrael"    "TmAddGrael"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmaddselectedgrael" "TmAddSelectedGrael" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmaxis"        "TmAxis"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmbitmap"      "TmBitmap"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmbottomgrael" "TmBottomGrael" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmdelgrael"    "TmDelGrael"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmdelselectedgraels" "TmDelSelectedGraels" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumerategraelmethods" "TmEnumerateGraelMethods" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumerategraels" "TmEnumerateGraels" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmenumerateselectedgraels" "TmEnumerateSelectedGraels" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmexecutegraelmethod" "TmExecuteGraelMethod" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetgraelmethod" "TmGetGraelMethod" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetgraelrectangle" "TmGetGraelRectangle" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgetuniquegraelname" "TmGetUniqueGraelName" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmgroupgraels" "TmGroupGraels" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmlegend"      "TmLegend"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmline"        "TmLine"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmrect"        "TmRect"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmsetgraelmethod" "TmSetGraelMethod" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmsetgraelrectangle" "TmSetGraelRectangle" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmtext"        "TmText"        'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmtopgrael"    "TmTopGrael"    'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tmungroupgraels" "TmUngroupGraels" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "woaddbuttons"  "WoAddButtons"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "woaddmessage"  "WoAddMessage"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "woaddstatus"   "WoAddStatus"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wobuildresourcefilename" "WoBuildResourceFilename" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wobuttonbar"   "WoButtonBar"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wobuttonbarset" "WoButtonBarSet" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wobuttonbarsetsensitivity" "WoButtonBarSetSensitivity" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocheckfile"   "WoCheckFile"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorbutton" "WoColorButton" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorbuttongetvalue" "WoColorButtonGetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorbuttonsetvalue" "WoColorButtonSetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorgrid"   "WoColorGrid"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorgridgetvalue" "WoColorGridGetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorgridsetvalue" "WoColorGridSetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wocolorwheel"  "WoColorWheel"  'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "woconfirmclose" "WoConfirmClose" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wodialogstatus" "WoDialogStatus" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wofontoptionmenu" "WoFontOptionMenu" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wofontoptionmenugetvalue" "WoFontOptionMenuGetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wofontoptionmenusetvalue" "WoFontOptionMenuSetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wogenericdialog" "WoGenericDialog" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wogettoolnamefromtitle" "WoGetToolNameFromTitle" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wogetuniquewindowtitle" "WoGetUniqueWindowTitle" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wolabeledtext" "WoLabeledText" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wolinestyleoptionmenu" "WoLinestyleOptionMenu" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wolinestyleoptionmenugetvalue" "WoLinestyleOptionMenuGetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wolinestyleoptionmenusetvalue" "WoLinestyleOptionMenuSetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "woloadresources" "WoLoadResources" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "woloadstrings" "WoLoadStrings" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "womenubar"     "WoMenuBar"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "womenubarsetsensitivity" "WoMenuBarSetSensitivity" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "womenubarsettoggle" "WoMenuBarSetToggle" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "womessage"     "WoMessage"     'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wosaveaspixmap" "WoSaveAsPixmap" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wosetcursor"   "WoSetCursor"   'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wosettoolicon" "WoSetToolIcon" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wosetwindowtitle" "WoSetWindowTitle" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wostatus"      "WoStatus"      'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wovariableoptionmenu" "WoVariableOptionMenu" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wovariableoptionmenugetvalue" "WoVariableOptionMenuGetValue" 'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "wovariableoptionmenusetvalue" "WoVariableOptionMenuSetValue" 'wave-check-abbrev)
;;
;; A Few Standard Library routines
;;    Added by Dave Landers - Sep 96
;;
  (define-abbrev wave-mode-abbrev-table "congrid"      	"CONGRID"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "contourfill"   "CONTOURFILL"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "dist"      	"DIST"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "hanning"      	"HANNING"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "loadct"      	"LOADCT"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "nint"      	"NINT"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "reverse"      	"REVERSE"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strjoin"      	"STRJOIN"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strsplit"      "STRSPLIT"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "strsubst"      "STRSUBST"	'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "sum"      	"SUM"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "t3d"      	"T3D"		'wave-check-abbrev)
  (define-abbrev wave-mode-abbrev-table "tek_color"  	"TEK_COLOR"	'wave-check-abbrev)))
;;
;; Define font lock for wave-mode
;;
(defconst wave-font-lock-keywords-1
  (purecopy
   (list
    ;; Subroutine and function declarations
    '("^[ \t]*pro[ \t]+.*$" . font-lock-function-name-face)
    '("^[ \t]*function[ \t]+.*$" . font-lock-function-name-face)
    ;; Common blocks
    '("^[ \t]*\\(common[ \t]+[A-z]+[A-z0-9$_]*\\)" 1 font-lock-type-face)
    ;; Include files
    '("^[ \t]*@[A-z]+[A-z0-9$_\.]*" . font-lock-string-face)
    ;; DECLARE FUNC
    '("^[ \t]*\\(declare[ \t]+func\\)" 1 font-lock-type-face)
    ))
  "For consideration as a value of `wave-font-lock-keywords'.
This does fairly subdued highlighting of comments and function names.")

(defconst wave-font-lock-keywords-2
  (append wave-font-lock-keywords-1
	  (purecopy
	   (list
	    ;; Variable declarations and conversions
	    ;;     Added more - Dave Landers - Sep 96
	    '("\\b\\(byte\\|fix\\|long\\|float\\|double\\|complex\\|string\\|bytarr\\|intarr\\|lonarr\\|fltarr\\|dblarr\\|complexarr\\|strarr\\|list\\|asarr\\|bindgen\\|indgen\\|lindgen\\|findgen\\|dindgen\\|cindgen\\|sindgen\\|imaginary\\|nint\\)\\b" 
	      1 font-lock-type-face)
	    
	    ;; System Variables
	    '("[ \t]*![A-z]+[A-z0-9$_.]*[ \t]*" . font-lock-function-name-face)
	    
	    ;; Other key words
	    '("\\b\\(begin\\|break\\|case\\|do\\|else\\|end\\|endcase\\|endelse\\|endfor\\|endif\\|endrep\\|endwhile\\|exit\\|for\\|goto\\|if\\|of\\|on_ioerror\\|on_error\\|on_error_goto\\|repeat\\|return\\|stop\\|then\\|until\\|while\\|\\.\\.locals\\)\\b"
	      1 font-lock-type-face)
	    	    
	    ;; executive commands
	    '("^\\(\\.run\\|\\.rnew\\|\\.skip\\|\\.con\\|\\.step\\|\\.locals\\|\\.size\\)\\b"
	      1 font-lock-type-face)

	    ;; Frequently used functions and procedures
	    ;;     Added lots here - Dave Landers - Sep 96
	    
	    ;; variable and parameter query routines - DL
;;;===;;;===;;;
	    '("\\b\\(askeys\\|isaskey\\|keyword_set\\|n_elements\\|n_params\\|n_tags\\|param_present\\|size\\|tag_names\\)\\b" 1 font-lock-keyword-face)

	    ;; file & I/O routines
;;;===;;;===;;;
	    '("\\b\\(assoc\\|eof\\|fstat\\|close\\|flush\\|free_lun\\|get_lun\\|openr\\|openu\\|openw\\|point_lun\\|print\\|printf\\|read\\|readf\\|readu\\|restore\\|save\\|writeu\\)\\b" 1 font-lock-keyword-face)

	    ;; common data manipulation routines - DL
	    '("\\b\\(congrid\\|bytscl\\|max\\|min\\|rebin\\|reform\\|reverse\\|shift\\|replicate\\|sort\\|sum\\|total\\|unique\\|where\\|byteorder\\)\\b" 1 font-lock-keyword-face)

	    ;; system access (etc) routines - DL
	    '("\\b\\(execute\\|cd\\|info\\|message\\|wait\\)\\b" 1 font-lock-keyword-face)

	    ;; plotting & graphics routines - DL
	    '("\\b\\(axis\\|contour\\|contourfill\\|cursor\\|device\\|empty\\|erase\\|oplot\\|plot\\|plots\\|plot_io\\|plot_oi\\|plot_oo\\|set_plot\\|shade_surf\\|surface\\|tv\\|tvlct\\|tvrd\\|wdelete\\|window\\|wset\\|wshow\\|xyouts\\|loadct\\)\\b" 1 font-lock-keyword-face)

	    ;; Boolean and relational operations
	    '("\\b\\(and\\|or\\|not\\|lt\\|le\\|eq\\|ge\\|gt\\|ne\\|mod\\|xor\\)\\b"
	      1 font-lock-type-face)
	    )))
  "For consideration as a value of `wave-font-lock-keywords'.
This highlights variable types, \"keywords,\" etc.")
;; The keywords in the preceding lists assume case-insensitivity.
(put 'wave-mode 'font-lock-keywords-case-fold-search t)

;; Default option for keywords. If fontifying is to slow on you machine
;; use wave-font-lock-keywords-1 instead
(defconst wave-font-lock-keywords wave-font-lock-keywords-2
  "Additional expressions to highlight in Wave mode.")
;;
;;
;;
;;
(defun wave-mode ()
  "Major mode for editing WAVE CL .pro files.

To install put in your ~/.emacs file:
  (you may want to specify a file path for wave.el file)

  (setq auto-mode-alist
      (append
         '((\"\\.pro$\" . wave-mode))
         auto-mode-alist))
  (autoload 'wave-mode \"wave\"
      \"Major mode for WAVE CL .pro files\" t)


Type `C-?' to display a list of built-in abbrevs for Wave keywords.

Indentation:
  By default current and next comment line is indented, when RET is
  entered. This can be suppressed by setting `wave-newline-and-indent'
  to nil. TAB is used for explicit indentation.

  Code Indentation:
    Variable `wave-block-indent' specifies relative indent for
    block statements(begin...end),
    variable `wave-continuation-indent' specifies relative indent for
    continuation lines.
    Continuation lines inside {}, [], (), are indented by
    `wave-continuation-indent' after opening parenthesis.
    Continuation lines in PRO, FUNCTION declarations are indented
    just after the prodcedure/function name.

  Comment Indentation:
    Text in full line comments is indented to previous comment line
    text (if any), unless previous comment line is empty, or start
    of Wave CL document library.
    Code line comment is indented to the value of `comment-column'.

Variables controlling indentation style and extra features:

 wave-block-indent
    Extra indentation within blocks.  (default 3)
 wave-continuation-indent
    Extra indentation within continuation lines.  (default 2)
 wave-doclib-start
    Start of Wave CL document library header.  (default ^[;*]\+)
 wave-doclib-end
    End of Wave CL document library header.    (default ^[;*]-)
 wave-minimum-statement-indent
    Minimum indentation for wave statements. (default 0)
 wave-newline-and-indent
    Automatically indents current line, inserts newline and
    indents it.   (default is t) 
 wave-surround-by-blank
    Automatically surrounds '=','<','>' with blanks, appends blank to comma.
    (default is t)
wave-upcase-procedure-names
    Controls whether to Capitalize (nil) or UPPERCASE (t) function 
    and procedure names (default is nil).
wave-upcase-common-names
    Controls whether to Capitalize (nil) or UPPERCASE (t) the names 
    of COMMON blocks (default is nil).
 wave-comment-region
    String inserted by \\[wave-comment-region] at start of each line in 
    region.  (default \";; \")
 wave-startup-message
    Set to nil to inhibit message first time wave-mode is used.
 wave-block-match-back-max
    Maximum number of lines to search for matching block delimiter
    to blocks lines.
    (default is 40) Set to 0 to suppress block matching.
 wave-font-lock-keywords
    Additional expressions to highlight in Wave mode. If fontifying is
    to slow on you machine use wave-font-lock-keywords-1 instead
    (default wave-font-lock-keywords-2)

Turning on Wave mode calls the value of the variable wave-mode-hook 
with no args, if that value is non-nil.

Command Table:
\\{wave-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (if wave-startup-message
      (message "Emacs Wave mode version %s.  Bugs to support@boulder.vni.com" wave-mode-version))
  (setq wave-startup-message nil)
  (setq local-abbrev-table wave-mode-abbrev-table)
  (set-syntax-table wave-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'wave-indent-line)
  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip "^[;*][ \t]*") ;[^ \t\n]* handles comment strings such as ;;...
  (make-local-variable 'comment-line-start)
  (setq comment-line-start "; ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[ \t]+;[ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'wave-doclib-start)
  (setq wave-doclib-start "^[;*]\\+")
  (make-local-variable 'wave-doclib-end)
  (setq wave-doclib-end "^[;*]-")
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'wave-block-start-delimiters)
  (setq wave-block-start-delimiters "\\(\\bbegin\\|^[ \t]*pro\\|^[[ \t]*function\\|\\bcase\\)\\b")
  (make-local-variable 'wave-block-end-delimiters)
;;  (setq wave-block-end-delimiters "\\b\\(end\\|endcase\\|endfor\\|endif\\|endelse\\|endrep\\|endwhile\\)\\b")
  (setq wave-block-end-delimiters "^[ \t]*\\(end\\|endcase\\|endfor\\|endif\\|endelse\\|endrep\\|endwhile\\)\\b")
  (make-local-variable 'wave-block-delimiters)
  (setq wave-block-delimiters
	'(("endif" . ("then" "begin"))
	  ("endelse" . ("else" "begin"))
	  ("endcase" . ("case" "of"))
	  ("endfor" . ("for" "begin"))
	  ("endrep" . ("repeat" "begin"))
	  ("endwhile" . ("while" "begin"))))
  (make-local-variable 'wave-indent-action-table)
  (setq wave-indent-action-table
	[("!" . wave-expand-sysvar)		;; Capitalize system vars
	 ("&" . wave-expand-multi-stmt)	        ;; Add spaces to mulit stmt sign
	 ("=" . wave-expand-equal)		;; Add spaces to equal sign if not keyword
	 ("<" . wave-expand-lessthan)		;; Add spaces to less than
	 (">" . wave-expand-greathan)       	;; Add spaces to greater than
	 ("," . wave-expand-comma)		;; Add spaces to comma
	 ("\\bpro\\b" . wave-expand-pro)	;; Upper case proc name, capitalize keywords
	 ("\\bfunction\\b" . wave-expand-func)  ;; Upper case func name, capitalize keywords
	 ("\\bcommon\\b" . wave-expand-common)  ;; Upper case common name
	 ("\\bdeclare[ \t]*func\\b" . wave-expand-declare-func) ;; Upper case function names in DECLARE FUNC
         (":" . wave-expand-label)		;; Capitalize label
         (nil . nil)])				;; End of array sentinel
  
  (make-local-variable 'wave-dquote-location)
  (setq wave-dquote-location 0)
  (make-local-variable 'wave-squote-location)
  (setq wave-squote-location 0)
  (make-local-variable 'wave-sysvar-mode)
  (setq wave-sysvar-mode nil)

  (use-local-map wave-mode-map)
  (setq mode-name "Wave")
  (setq major-mode 'wave-mode)
  (setq abbrev-mode t)

  (run-hooks 'wave-mode-hook))

;;
;; Following page contains routines handling input of the WAVE CL code
;;
(defun wave-comment (n)
  "Set 'wave-comment-mode'.
'wave-comment-mode' will end when the user types a newline."
  (interactive "p")
    (set-buffer-modified-p (buffer-modified-p))
    (insert-char last-command-char n))

(defun wave-hard-tab ()
  "Inserts TAB in buffer in current position."
  (interactive)
  (insert "\t"))

(defun wave-not-command ()
  "Returns true if not in command (comment or quotes)."
(or (wave-in-comment) (wave-between-squote) (wave-between-dquote)
	  (wave-in-squote) (wave-in-dquote)))

(defun wave-check-abbrev ()
  "Reverses abbrev expansion if in comment, quotes."
  (if (wave-not-command)
      (unexpand-abbrev)))

(defun wave-check-system-abbrev ()
  "Reverses abbrev expansion if not preceeded by blank and returns false."
  (let* ((pos (point))
	 (found nil))
    (if (wave-not-command)
	(unexpand-abbrev)
      (save-excursion
	(goto-char (- last-abbrev-location 1))
	(if (not (or (looking-at "[ \t]")
		     (save-excursion (forward-char) (bolp))))
	    (unexpand-abbrev)
	  (setq found t))))
    (goto-char pos)
    found))

(defun wave-in-comment ()
  "True if current point is in comment"
  (save-excursion
    (re-search-backward comment-start
			(save-excursion (beginning-of-line) (point)) t)))

(defun wave-multi-stmt ()
  "Multiple WAVE commands placed on one-line, surround by blanks."
  (interactive)
  (if (not (or  (wave-in-comment) (wave-in-squote) (wave-in-dquote)
		(not wave-surround-by-blank)))
      (progn
	(if (not (eq (preceding-char) 32))
	    (insert " "))
	(insert last-command-char " "))
    (insert last-command-char)))

(defun wave-sysvar ()
  "Set system variable mode.  When a new-line is pressed the first letter
 of the first word in the system variable will be capitalized."
  (interactive)
  (insert last-command-char)
  (if (not (or (wave-in-comment) (wave-in-squote) (wave-in-dquote)))
      (setq wave-sysvar-mode t)))
 
(defun wave-delete-char ()
  "Deletes char backward - if char is single or double quote toggles
the mode and resets the location"
  (interactive)
  (if (save-excursion (backward-char) (looking-at "'"))
      (if (not (or (wave-in-comment) (wave-in-dquote)))
	  (if (wave-in-squote)
	      (setq wave-squote-location 0)
	    (save-excursion
	      (beginning-of-line)
	      (if (re-search-forward "'" (save-excursion (end-of-line) (point)) t)
		  (setq wave-squote-location (match-beginning 0))
		(setq wave-squote-location 0))))))
  (if (save-excursion (backward-char) (looking-at "\""))
      (if (not (or (wave-in-comment) (wave-in-squote)))
	  (if (wave-in-dquote)
	      (setq wave-dquote-location 0)
	    (save-excursion
	      (beginning-of-line)
	      (if (re-search-forward "\"" (save-excursion (end-of-line) (point)) t)
		  (setq wave-dquote-location (match-beginning 0))
		(setq wave-dquote-location 0))))))
  (backward-delete-char-untabify 1))
   
(defun wave-octal-number ()
  "Deletes char backward - if char is single or double quote toggles
the mode and resets the location"
  (interactive)
  (if (= wave-dquote-location (1- (point)))
      (setq wave-dquote-location 0))
  (insert last-command-char))

(defun wave-in-squote ()
  "Tests if single quote location is on current line."
  (and
   (save-excursion (beginning-of-line) (>= wave-squote-location (point)))
   (save-excursion (end-of-line) (<= wave-squote-location (point)))))

(defun wave-between-squote ()
  "Tests if point is in single quotes."
  (save-excursion
    (let* ((to (point))
	   (count 0))
      (beginning-of-line)
      (while (re-search-forward "'" to t) (setq count (1+ count)))
      (eq (% count 2) 1))))


(defun wave-squote ()
  "Set the location of the quote.
If wave-squote-location > 0, reset it, and show the user where
the matching quote is."
  (interactive)
  (if (not (or (wave-in-comment) (wave-in-dquote)))
      (if (and (wave-in-squote)
	       (save-excursion
		 (goto-char wave-squote-location) (looking-at "'")))
	  (progn
	    (save-excursion
	      (insert last-command-char)
	      (if (> wave-squote-location 0)
		  (progn
		    (goto-char wave-squote-location)
		    (message " ")
		    (sit-for 1))))
	    (setq wave-squote-location 0)
	    (forward-char))
	(progn
	  (insert last-command-char)
	  (setq wave-squote-location (1- (point)))))
    (insert last-command-char)))
 
(defun wave-in-dquote ()
  "Tests if double quote location is on current line."
  (and
   (save-excursion (beginning-of-line) (>= wave-dquote-location (point)))
   (save-excursion (end-of-line) (<= wave-dquote-location (point)))))


(defun wave-between-dquote ()
  "Tests if point is in double quotes."
  (save-excursion
    (let* ((to (point))
	   (count 0))
      (beginning-of-line)
      (while (re-search-forward "\"" to t) (setq count (1+ count)))
      (eq (% count 2) 1))))


(defun wave-dquote ()
  "Set the location of the quote.
If wave-dquote-location > 0 reset it, and show the user where
the matching quote is."
(interactive)
(if (not (or (wave-in-comment) (wave-in-squote)))
    (if (and (wave-in-dquote)
	     (save-excursion
	       (goto-char wave-dquote-location) (looking-at "\"")))
	(progn
	  (save-excursion
	    (insert last-command-char)
	    (if (> wave-dquote-location 0)
		(progn
		  (goto-char wave-dquote-location)
		  (message " ")
		  (sit-for 1))))
	  (setq wave-dquote-location 0)
	  (forward-char))
      (progn
	(insert last-command-char)
	(setq wave-dquote-location (- (point) 1))))
  (insert last-command-char)))

(defun wave-show-command ()
  "Finds the start of a matching command and blinks to it for a sec.
It uses wave-block-delimiters alist to get an appropriate start of the command
pattern."
  (if (wave-not-command)
      (unexpand-abbrev)
    (save-excursion
      (if wave-block-match
	  (let* ((found nil)
		 (count 0)
		 (first-statement nil)
		 (end-key (downcase last-abbrev-text))
		 (start-key (cdr (assoc end-key wave-block-delimiters)))
		 (start-found nil)
		 (start-key-element (nth 0 start-key))
		 (level 0))
	    ;;
	    ;; While not first statement and not found start delimiter
	    ;;
	    (while (not (or found first-statement))
	      (setq start-found nil)
	      (setq first-statement (wave-previous-statement))
	      ;;
	      ;; Look for start delimiter, if level=0 then found else decrease level
	      ;;
	      (save-excursion
		(while (and (not start-found) (wave-is-continuation-line))
		  (if (wave-look-at-word start-key-element)
		      (if (and
			   (setq start-key-element (nth 1 start-key))
			   (wave-look-at-word start-key-element))
			  (progn
			    (setq start-found t)
			    (if (= level 0)
				(progn
				  (setq found t)
				  (message " ")
				  (sit-for 1))
			      (setq start-key-element (nth 0 start-key))
			      (setq level (1- level))))))
		  (forward-line 1)))
	      (if (and (not start-found) (wave-look-at-word start-key-element))
		  (if (and
		       (setq start-key-element (nth 1 start-key))
		       (wave-look-at-word start-key-element))
		      (progn
			(setq start-found t)
			(if (= level 0)
			    (progn
			      (setq found t)
			      (message " ")
			      (sit-for 1))
			  (setq start-key-element (nth 0 start-key))
			  (setq level (1- level))))))
	      ;;
	      ;; Look for another end delimiter first, increase level if found
	      ;;
	      (save-excursion
		(beginning-of-line)
		(while (wave-is-continuation-line)
		  (if (wave-look-at-word end-key) (setq level (1+ level)))
		  (forward-line 1))
		(if (wave-look-at-word end-key) (setq level (1+ level))))
	      (setq count (1+ count))
	      (if (> count wave-block-match-back-max)
		  (setq found t))
	      ))))))

(defun wave-show-begin ()
  "Finds the start of an end block command and blinks to it for a sec."
  (if (wave-not-command)
      (unexpand-abbrev)
    (save-excursion
      (if wave-block-match
	  (let ((found nil)
		(count 0)
		(first-statement nil)
		(start-found nil)
		(level 0))
	    ;;
	    ;; While not first statement and not found start delimiter
	    ;;
	    (while (not (or found first-statement))
	      (setq start-found nil)
	      (setq first-statement (wave-previous-statement))
	      ;;
	      ;; Look for start delimiter, if level=0 then found else decrease level
	      ;;
	      (save-excursion
		(while (and (not start-found) (wave-is-continuation-line))
		  (if (wave-look-at wave-block-start-delimiters)
		      (progn
			(setq start-found t)
			(if (= level 0)
			    (progn
			      (setq found t)
			      (message " ")
			      (sit-for 1))
			  (setq level (1- level)))))
		  (forward-line 1)))
	      (if (and (not start-found) (wave-look-at wave-block-start-delimiters))
		  (progn
		    (setq start-found t)
		    (if (= level 0)
			(progn
			  (setq found t)
			  (message " ")
			  (sit-for 1))
		      (setq level (1- level)))))
	      ;;
	      ;; Look for another end delimiter first, increase level if found
	      ;;
	      (save-excursion
		(beginning-of-line)
		(while (wave-is-continuation-line)
		  (if (wave-look-at wave-block-end-delimiters) (setq level (1+ level)))
		  (forward-line 1))
		(if (wave-look-at wave-block-end-delimiters) (setq level (1+ level))))
	      (setq count (1+ count))
	      (if (> count wave-block-match-back-max)
		  (setq found t))
	      ))))))

(defun wave-equal ()
  "Two cases: Assignment statement, and keyword assignment. Keyword
 assignment will occur when function or procedure is declared,
 blanks will be removed from both sides of the equal sign.
All other conditions will be treated as standard
 assignment statements and the equal sign will be surrounded by blanks."
  (interactive)
  (if wave-surround-by-blank
      (if (or (wave-pro-def) (wave-func-def))
	  (if (eq (preceding-char) 32)
	      (delete-backward-char 1)
	    (insert last-command-char))
	(if (not (or  (wave-in-comment) (wave-in-squote) (wave-in-dquote)))
	    (progn
	      (if (not (eq (preceding-char) 32))
		  (insert " "))
	      (insert last-command-char " "))
	  (insert last-command-char)))
    (insert last-command-char)))

(defun wave-lessthan ()
  "Always surround the less than sign with blanks."
  (interactive)
  (if (not (or  (wave-in-comment) (wave-in-squote) (wave-in-dquote)
		(not wave-surround-by-blank)))
      (progn
	(if (not (eq (preceding-char) 32))
	    (insert " "))
	(insert last-command-char " "))
    (insert last-command-char)))

(defun wave-greathan ()
  "Always surround the greater than sign with blanks."
  (interactive)
  (if (not (or  (wave-in-comment) (wave-in-squote) (wave-in-dquote)
		(not wave-surround-by-blank)))
      (progn
	(if (not (eq (preceding-char) 32))
	    (insert " "))
	(insert last-command-char " "))
    (insert last-command-char)))

(defun wave-comma ()
  "Always append blank after comma."
  (interactive)
  (if (not (or (wave-in-comment) (wave-in-squote) (wave-in-dquote)
	       (not wave-surround-by-blank)))
      (insert last-command-char " ")
    (insert last-command-char)))

(defun wave-func-def ()
  "Tests if command is function definition."
(save-excursion
  (if (save-excursion (forward-line -1) (wave-is-continuation-line))
      (wave-previous-statement)
    (beginning-of-line))
  (wave-look-at "\\bfunction\\b")))

(defun wave-pro-def ()
  "Tests if command is procedure definition."
  (save-excursion
    (if (save-excursion (forward-line -1) (wave-is-continuation-line))
	(wave-previous-statement)
      (beginning-of-line))
    (wave-look-at ".*\\bpro\\b")))

(defun wave-newline ()
  "Turns off comment mode, turns on abbrev mode
handles indentation, capitalization etc"
  (interactive)
  (cond
   ;;
   ;; Handle System Variable Capitalization
   ;;
   (wave-sysvar-mode
    (save-excursion
      (beginning-of-line)
      (while (re-search-forward "!" (save-excursion (end-of-line) (point)) t)
	(capitalize-word 1))

      (setq wave-sysvar-mode nil)))
   ;;
   ;; Handle unterminated single and double quotes
   ;;
   ((> wave-squote-location 0)
    (save-excursion
      (goto-char wave-squote-location)
      (message " ")
      (sit-for 1))
    (setq wave-squote-location 0)
    (message "Warning - unbalanced single quotes?"))
   ((> wave-dquote-location 0)
    (save-excursion
      (goto-char wave-dquote-location)
      (message " ")
      (sit-for 1))
    (setq wave-dquote-location 0)
    (message "Warning - unbalanced double quotes?"))
   )
  ;;
  ;; Do automatic reindent while typing
  ;; This seems to be kludge, but markers and abbrev expansion doesn't
  ;; work together well, so we have to do that instead
  ;;
  (setq wave-block-match t)
  (wave-format-line)
  (let* ((pos (point))
	 (old-cfi (calculate-wave-indent))
	 (new-cfi old-cfi)
	 (at-end-line (eolp)))
    (wave-indent-line t)
    (setq new-cfi (calculate-wave-indent))
    (if at-end-line
	(end-of-line)
      (goto-char (+ pos (- new-cfi old-cfi))))
    (setq wave-block-match nil)
    (if wave-newline-and-indent
	(progn (newline-and-indent)
	       (if (save-excursion (looking-at "^[ \t]*$"))
		   (end-of-line)))
      (newline))))

;;
;; Following page contains routines used in update or indent mode.
;;
;;
;;  Use global variable 'comment-column' to set parallel comment
;;
(defun wave-comment-hook ()
  (save-excursion
    (skip-chars-backward " \t")
    (max (+ 1 (current-column))
	 comment-column)))

(defun wave-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;;
  ;; Recognize existing comments of either kind.
  ;;
  (cond ((re-search-forward comment-start-skip
			    (save-excursion (end-of-line) (point)) t)
	 (indent-for-comment))
	;;
	;; No existing comment.
	;; Insert one, unless line is now blank.
	;;
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (wave-comment-hook))
	 (insert comment-start))
	;;
	;; Else insert separate-line comment, making a new line if nec.
	;;
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert comment-line-start)
	 (insert-char (if (stringp wave-comment-indent-char)
			  (aref wave-comment-indent-char 0)
			wave-comment-indent-char)
			(- (save-excursion
			     (forward-line -1)
			     (wave-current-line-indentation)) (current-column))))))

(defun wave-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts wave-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert wave-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert wave-comment-region)))
      (let ((com (regexp-quote wave-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun wave-abbrev-help ()
  "List the currently defined abbrevs in Wave mode."
  (interactive)
  (message "Listing abbrev table...")
  (require 'abbrevlist)
  (list-one-abbrev-table wave-mode-abbrev-table "*Help*")
  (message "Listing abbrev table...done"))

(defun wave-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
;;
;; Continue a comment line by inserting another comment with the same indention, etc.
;;
  (if (save-excursion (beginning-of-line) (looking-at (concat "^[ \t]*" comment-start "+[ \t]*")))
      (insert "\n" (buffer-substring (match-beginning 0) (match-end 0)))
;;
;; Continue command ending in a comment by adding continuation char
;; before the comment
;;
    (if (wave-in-comment)
	(progn
	  (wave-indent-line)
	  (beginning-of-line)
	  (forward-char (wave-comment-hook))
	  (insert wave-continuation-char " ")
	  (end-of-line)
	  (insert "\n ")
	  (wave-indent-line))
;;	    
;; insert normal continuation
;;
      (insert " " wave-continuation-char)
      (insert "\n " )
      (forward-line -1)
      (wave-indent-line)
      (forward-line 1))
    (wave-indent-line)))

(defun wave-join-line (&optional arg)
  "Join this line to previous and fix up whitespace at join.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if (eq (preceding-char) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	(fixup-whitespace)))
  (if (eq (preceding-char) wave-continuation-char)
      (progn
	(delete-region (point) (1- (point)))
	(fixup-whitespace))))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun beginning-of-wave-subprogram ()
  "Moves point to the beginning of the current wave subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward "^[ \t]*\\b\\(pro\\|function\\)\\b" nil 'move)
    (beginning-of-line)))

(defun end-of-wave-subprogram ()
  "Moves point to the end of the current wave subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward "^[ \t]*\\b\\(pro\\|function\\)\\b" nil 'move)
    (re-search-backward "^[ \t]*\\bend\\b[ \t]*" nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun mark-wave-subprogram ()
  "Put mark at end of wave subprogram, point at beginning. 
The marks are pushed."
  (interactive)
  (end-of-wave-subprogram)
  (let ((beg (point)))
    (beginning-of-wave-subprogram)
    (push-mark beg)
    (exchange-point-and-mark)))
  
(defun mark-wave-doclib ()
  "Put point at end of wave doc library header, mark at beginning. 
The marks are pushed."
  (interactive)
  (if (re-search-backward wave-doclib-start nil t)
      (progn
	(beginning-of-line)
	(let ((beg (point)))
	  (if (re-search-forward wave-doclib-end)
	      (progn
		(beginning-of-line 2)
		(push-mark beg)
		(exchange-point-and-mark)))))))
  
(defun wave-previous-statement ()
  "Moves point to beginning of the previous wave statement.
Returns 'first-statement if that statement is the first
non-comment Wave statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement)
    (setq not-first-statement (= (forward-line -1) 0))
    (while (and (> (point) (point-min))
		(or (looking-at comment-line-start-skip)
		    (looking-at "^[ \t]*\\b[a-zA-Z]+[a-zA-Z0-9$_]*\\b:[ \t]*$")
		    (looking-at "^[ \t]*@")
		    (looking-at "[ \t]*$")
		    (looking-at (concat "[ \t]*"  comment-start-skip))))
      (setq not-first-statement (= (forward-line -1) 0)))
    (while (and (> (point) (point-min))
		(save-excursion
		  (setq not-first-statement (= (forward-line -1) 0))
		  (wave-is-continuation-line)))
      (setq not-first-statement (= (forward-line -1) 0)))
    (if (not not-first-statement)
	'first-statement)))
 
(defun wave-end-of-statement ()
"Moves point to the end of the current wave statement.
Assumes that current position is somewhere in a statement. Returns position."
(interactive)
(let ((not-last-statement t))
  (while (and not-last-statement (wave-is-continuation-line))
    (setq not-last-statement (= (forward-line 1) 0)))
  (end-of-line) (point)))
      
(defun wave-next-statement ()
  "Moves point to beginning of the next wave statement.
 Returns 'last-statement if that statement is the last
 non-comment Wave statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement (= (forward-line 1) 0))
		(> (point-max) (point))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
		    (save-excursion
		      (beginning-of-line 0)
		      (looking-at ".*\\$"))
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

(defun wave-expand-multi-stmt ()
  "Multiple WAVE commands placed on one-line, surround by blanks."
  (if (and wave-surround-by-blank
	   (not (or  (wave-in-comment) (wave-between-squote) (wave-between-dquote))))
      (progn
	(if (not (eq (char-after (- (point) 2)) 32))
	    (save-excursion (backward-char 1) (insert " ")))
	(if (not (eq (following-char) 32))
	    (insert " ")))))
 
(defun wave-expand-sysvar ()
  "Set system variable mode.  The first letter
 of the first word in the system variable will be capitalized."
  (if (and wave-surround-by-blank
	   (not (or  (wave-in-comment) (wave-between-squote) (wave-between-dquote))))
      (capitalize-word 1)))

(defun wave-expand-equal ()
  "Two cases: Assignment statement, and keyword assignment. Keyword
 assignment will occur when function or procedure is declared or called,
 blanks will be removed from both sides of the equal sign.
All other conditions will be treated as standard
 assignment statements and the equal sign will be surrounded by blanks."
  (if (or (wave-pro-def) (wave-func-def))
      (progn
	(if (eq (char-after (- (point) 2)) 32)
	    (progn
	      (save-excursion
		(backward-char 1)
		(delete-char -1))
	      (if (eq (following-char) 32)
		  (delete-char 1))
	      (backward-word 1)
	      (capitalize-word 1) (forward-char 1))))
    (if (and wave-surround-by-blank
	     (not (or  (wave-in-comment) (wave-between-squote) (wave-between-dquote))))
	(progn
	  (if (not (eq (char-after (- (point) 2)) 32))
	      (save-excursion (backward-char 1) (insert " ")))
	  (if (not (eq (following-char) 32))
	      (insert " "))))))
  
(defun wave-expand-lessthan ()
  "Always surround the less than sign with blanks."
  (if (and wave-surround-by-blank
	   (not (or  (wave-in-comment) (wave-between-squote) (wave-between-dquote))))
      (progn
	(if (not (eq (char-after (- (point) 2)) 32))
	    (save-excursion (backward-char 1) (insert " ")))
	(if (not (eq (following-char) 32))
	    (insert " ")))))

(defun wave-expand-greathan ()
  "Always surround the greater than sign with blanks."
  (if (and wave-surround-by-blank
	   (not (or  (wave-in-comment) (wave-between-squote) (wave-between-dquote))))
      (progn
	(if (not (eq (char-after (- (point) 2)) 32))
	    (save-excursion (backward-char 1) (insert " ")))
	(if (not (eq (following-char) 32))
	    (insert " ")))))

(defun wave-expand-comma ()
  "Always append blank after comma.  If this is the start
of a greater than equal to the wave-equal sign will delete the trailing
blank."
  (if (and wave-surround-by-blank
	   (not (or  (wave-in-comment) (wave-between-squote) (wave-between-dquote))))
      (if (not (eq (following-char) 32))
	  (insert " "))))

(defun wave-expand-common ()
  "Capitalize the name of a COMMON block."
  (save-excursion
   (beginning-of-line)
   (forward-word 2)
   (backward-word 1)
   (if (> (following-char) ?Z)
       (if wave-upcase-common-name
	   (upcase-word 1)
	 (capitalize-word 1)))))

(defun wave-expand-declare-func ()
  "Capitalize the function names in a DECLARE FUNC statement"
  (save-excursion
   (beginning-of-line)
   (forward-word 3)
   (backward-word 1)
   (if (> (following-char) ?Z)
       (let* ( (here (point))
	       (there (re-search-forward comment-start 
			    (save-excursion (end-of-line) (point)) 1)))
	 (if (not there) (setq there (save-excursion (end-of-line) (point))))
	 (if wave-upcase-procedure-name
	     (upcase-region here there)
		 (capitalize-region here there))))))

(defun wave-expand-label ()
  "Capitalize the name of a label."
  (save-excursion
    (let ((not-label nil))
      (while (progn
	       (if (re-search-backward
		    "\\({\\|(\\)"
		    (save-excursion (beginning-of-line) (point)) t)
		   (setq not-label t))
	       (and (not not-label)
		    (save-excursion (forward-line -1)
				    (wave-is-continuation-line))))
	(forward-line -1)
	(end-of-line))
      (if (not not-label)
	  (progn
	    (backward-word 1)
	    (if (and
		 (re-search-backward
		  "^[ \t]*" (save-excursion (beginning-of-line) (point)) t)
		 (looking-at "[ \t]*\\b[a-zA-Z]+[a-zA-Z0-9$_]*\\b:")
		 (> (following-char) ?Z))
		(capitalize-word 1))
	    (forward-char 1))))))

(defun wave-expand-func ()
  "Format the WAVE FUNCTION definition lines."
  (interactive)
  ;;
  ;; Capitalize the Function Name
  ;;
  (save-excursion
   (beginning-of-line)
   (forward-word 2)
   (backward-word 1)
   (if (> (following-char) ?Z)
       (if wave-upcase-procedure-name
	   (upcase-word 1)
	 (capitalize-word 1)))))

(defun wave-expand-pro ()
"Format the WAVE PRO definition lines"
  ;;
  ;; Capitalize the Procedure Name
  ;;
  (save-excursion
   (beginning-of-line)
   (forward-word 2)
   (backward-word 1)
   (if (> (following-char) ?Z)
       (if wave-upcase-procedure-name
	   (upcase-word 1)
	 (capitalize-word 1)))))

(defun wave-format-line ()
  "Formats current line, expands abbrevs, executes action routines."
  ;;
  ;; Expand Abbrevs on the line
  ;; Never leave point in left margin.
  ;;
  (save-excursion
    (beginning-of-line)
    ;;
    ;; Now execute all action routines on the line
    ;;
    (let ((count 0)
	  (action-key (car (aref wave-indent-action-table 0)))
	  (action-routine (cdr (aref wave-indent-action-table 0))))
      (while action-key
	(beginning-of-line)
	(while (wave-look-at action-key)
	(funcall action-routine))
	(setq count (1+ count))
	(setq action-key (car (aref wave-indent-action-table count)))
	(setq action-routine (cdr (aref wave-indent-action-table count)))))))


(defun wave-indent-line (&optional arg)
  "Indents current wave line based on its contents and on previous lines."
  (interactive)
  (let ((cfi (calculate-wave-indent)))
    (save-excursion
      (beginning-of-line)
      (if (not
	   (or (looking-at comment-start-skip)
	       (looking-at comment-line-start-skip)))
	  (progn
	    (beginning-of-line)
	    (if (or
		 (looking-at "^[ \t]*\\b[a-zA-Z]+[a-zA-Z0-9$_]*\\b:[ \t]*$")
		 (looking-at "^[ \t]*@"))
		(progn
		  (wave-indent-to-column 0)
		  (setq cfi 0))
	      (if (not (= cfi (wave-current-line-indentation)))
		  (wave-indent-to-column cfi)))
	    (beginning-of-line)
	    (wave-indent-parallel-comment)
	    (if (not arg) (wave-format-line))
	    ;;
	    ;; Expand abbrevs
	    ;;
	    (beginning-of-line)
	    (if (not (or
		      (looking-at comment-line-start-skip)
		      (looking-at (concat "[ \t]*"  comment-start-skip))))
		(let ((end-region (save-excursion (end-of-line) (point))))
		  (if (re-search-forward
		       (concat ".*" comment-start) end-region t)
		      (setq end-region (1- (match-end 0))))
		  (expand-region-abbrevs (progn (beginning-of-line) (point))
					 end-region 1))))
	;;
	;; Check for comment prefixed by space
	;;
	(beginning-of-line)
	(if (looking-at comment-start-skip)
	    (if (not (= cfi (wave-current-line-indentation)))
		(wave-indent-to-column cfi)))))
    ;;
    ;; Finally set the indent
    ;;
    (if (< (current-column) cfi)
	(move-to-column cfi))))

(defun wave-indent-subprogram ()
  "Properly indents the Wave subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-wave-subprogram)
    (message "Indenting subprogram...")
    (indent-region (mark) (point) nil))
  (message "Indenting subprogram...done."))

(defun wave-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion 
    (message "Indenting buffer...")
    (indent-region 1 (save-excursion (end-of-buffer) (point)) nil))
  (message "Indenting buffer...done."))

(defun calculate-wave-indent ()
  "Calculates the wave indent column based on previous lines."
  (let (icol first-statement (case-fold-search t))
    ;;
    ;; check previous statement
    ;;
    (save-excursion
      (let ((end-search (point)))
	(setq first-statement (wave-previous-statement))
	;;
	;; If first statement or beginning of buffer, set minimum indent
	;; else check for block start
	;;
	(if first-statement
	    (setq icol wave-minimum-statement-indent)
	  (progn
	    (if (= (point) (point-min))
		(setq icol wave-minimum-statement-indent)
	      (setq icol (wave-current-line-indentation)))
	    (beginning-of-line)
	    ;;
	    ;; indent block
	    ;;
	    (let ((found nil))
	      (while (and (< (point) end-search)
			  (not found) (wave-is-continuation-line))
		  (if (wave-look-at wave-block-start-delimiters)
		      (setq found t)
		    (forward-line 1)))
	      (if (wave-look-at wave-block-start-delimiters)
		  (setq found t))
	      (if found
		  (setq icol (+ icol wave-block-indent))))))))
    ;;
    ;; If previous line is cont. line
    ;;
    (save-excursion
      (if (= (forward-line -1) 0)
	  ;;
	  ;; indent for cont. line
	  ;;
	  (if (wave-is-continuation-line)
	      (progn
		(forward-line 1)
		(setq icol (calculate-wave-cont-indent))))
	(setq icol wave-minimum-statement-indent)))
    ;;
    ;; Now check current line
    ;;
    (save-excursion
      (beginning-of-line)
      (cond
       ;;
       ;; Ignore empty line
       ;;
       ((looking-at "[ \t]*$"))
       ;;
       ;; Full line comment - indent as previous line
       ;;
       ((looking-at comment-line-start-skip)
	(setq icol (save-excursion
		     (forward-line -1)
		     (wave-current-line-indentation))))
       (t
	;;
	;; End of block statement
	;;
	(save-excursion
	  (if (wave-look-at wave-block-end-delimiters)
	      (setq icol (- icol wave-block-indent)))))))
    (max wave-minimum-statement-indent icol)))
;;
;; Parenthesses balacing/indent
;;
(defun calculate-wave-cont-indent ()
  "Calculates the wave continuation indent column based on previous lines."
  (let ((col (+ wave-continuation-indent
		(save-excursion
		  (wave-previous-statement)
		  (wave-current-line-indentation))))
	(case-fold-search t)
	(found nil)
	(level [0 0 0])
	(lindex 0)
	(end-reg (point))
	(beg-reg (save-excursion (wave-previous-statement) (point))))
    ;;
    ;; If PRO or FUNCTION declaration indent after name, and first comma, if there
    ;;
    (save-excursion
      (wave-previous-statement)
      (if (wave-look-at "\\b\\(pro\\|function\\)\\b")
	  (progn
	    (forward-word 1)
	    (looking-at "[ \t]*,[ \t]*")
	    (goto-char (match-end 0))
	    (setq col (current-column)))
	;;
	;; Look for start delimiter, if level=0 then found else decrease level
	;; Look for another end delimiter, increase level if found
	;;
	(save-excursion
	  (goto-char end-reg)
	  (while (and (not found) (re-search-backward "\\(\\s(\\|\\s)\\)" beg-reg t))
	    (if (looking-at "[])}]")
		(progn
		  (cond
		   ((looking-at ")") (setq lindex 0))
		   ((looking-at "\\]") (setq lindex 1))
		   ((looking-at "}") (setq lindex 2)))
		  (aset level lindex (1+ (aref level lindex))))
	      (progn
		(cond
		 ((looking-at "(") (setq lindex 0))
		 ((looking-at "\\[") (setq lindex 1))
		 ((looking-at "{") (setq lindex 2)))
		(if (= (aref level lindex) 0)
		    (setq found t)
			   (aset level lindex (1- (aref level lindex)))))))
	  ;;
	  ;; If open delimiter was found, set the col for indent.
	  ;;
	  (if found
	      (if (looking-at "[[{]")	 
		  (setq col (+ wave-continuation-indent (current-column)))
		(setq col (1+ (current-column)))))
	    
	  ;;
	  ;; If closing delimiter, indent back
	  ;;
	  (goto-char end-reg)
	  (beginning-of-line)
	  (if (looking-at "[ \t]*[]}]")
	      (setq col (- col wave-continuation-indent))
	    (if (looking-at "[ \t]*[)]")
		(setq col (1- col))))
	  (max wave-minimum-statement-indent col))))))

(defun wave-is-continuation-line ()
  "Tests if current line is continuation line."
  (save-excursion
  (beginning-of-line)
  (if (not (looking-at comment-line-start-skip))
    (let ((end-search (save-excursion (end-of-line) (point))))
      (if (re-search-forward (concat ".*" comment-start) end-search t)
	  (setq end-search (1- (match-end 0))))
      (beginning-of-line)
      (if (re-search-forward (regexp-quote (char-to-string wave-continuation-char)) end-search t)
	  (if (not (or (wave-between-squote) (wave-between-dquote) (wave-in-comment)))
	      t
	    nil)))
    nil)))

(defun wave-look-at (string)
  "Searches current line form current point for 'string',
ignores comment, single,double quotes."
  (let* ((end-search (save-excursion (end-of-line) (point)))
	 (start (point))
	 (end end-search)
	 (done nil) (found nil)
	 (save-point (point)))
    (if (re-search-forward (concat ".*" comment-start) end-search t)
	(setq end-search (1- (match-end 0))))
    (goto-char save-point)
    (while (and (not found) (not done))
      (if (re-search-forward "\"[^0-7]" end-search t)
	  (progn
	    (setq end (match-beginning 0))
	    (goto-char start)
	    (setq found (re-search-forward string end t))
	    (if (not found)
		(progn
		  (goto-char (1+ end))
		  (re-search-forward "\"[^\"]" (+ end-search 1) t)
		  (setq start (1+ (match-beginning 0)))
		  (goto-char start))))
	(if (re-search-forward "'" end-search t)
	    (progn
	      (setq end (match-beginning 0))
	      (goto-char start)
	      (setq found (re-search-forward string end t))
	      (if (not found)
		  (progn
		    (goto-char (1+ end))
		    (re-search-forward "'[^']" (+ end-search 1) t)
		    (setq start (1+ (match-beginning 0)))
		    (goto-char start))))
	  (setq done t)
	  (setq found (re-search-forward string end-search t)))))
    (if (not found)
	(goto-char save-point))
    found))

(defun wave-look-at-word (string)
  "Searches current line for word 'string', ignores comment."
  (wave-look-at (concat "\\b" string "\\b")))

(defun wave-current-line-indentation ()
  "Indentation of current line.
This is the column position of the first non-whitespace character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    ;;
    ;; If comment line, skip comment delimiter
    ;;
    (cond ((looking-at comment-line-start-skip)
	   (goto-char (match-end 0))
	   (skip-chars-forward
	    (if (stringp wave-comment-indent-char)
		wave-comment-indent-char
	      (char-to-string wave-comment-indent-char))))
	  )
    ;;
    ;; Else move past whitespace, return column
    ;;
    (skip-chars-forward " \t")
    (current-column)))

(defun wave-indent-to-column (col)
  "Indents current line with spaces to column COL."
  (save-excursion
    (beginning-of-line)
    ;;
    ;; If full line comment, and not doclib delimiters, indent comment text
    ;;
    (if (looking-at comment-line-start-skip)
	(if (not (or (looking-at wave-doclib-start)
		     (looking-at wave-doclib-end)
		     (save-excursion
		       (forward-line -1)
		       (or (looking-at "^[;*][ \t]*$")
			   (looking-at wave-doclib-start)
			   (not (looking-at ".*;"))))))
	    (let ((char (if (stringp wave-comment-indent-char)
			    (aref wave-comment-indent-char 0)
			  wave-comment-indent-char)))
	      (looking-at comment-line-start-skip)
	      (goto-char (match-end 0))
	      (delete-horizontal-regexp (concat " \t" (char-to-string char)))
	      (insert-char char (- col (current-column)))))
      ;;
      ;; Put body of statement where specified.
      ;;
      (delete-horizontal-space)
      (indent-to col))))

;; Fixed the following to: (1) not indent comment character (;) in a string, 
;;    and (2) indent comment after a comment in a string
;; Known bug - could hit maximum recursion limit if there are 
;;   lots of comment characters (;) in a string

(defun wave-indent-parallel-comment ()
  "Indent any comment following code on the same line."
  (save-excursion
    (if (re-search-forward comment-start (save-excursion (end-of-line) (point)) t)
	(let ((b (match-beginning 0)))
	  (if (or (wave-between-squote) (wave-between-dquote) )
	      (wave-indent-parallel-comment)
	    (progn (goto-char b)
		   (if (not (= (current-column) (wave-comment-hook)))
		       (progn (delete-horizontal-space)
			      (indent-to (wave-comment-hook))))))))))

