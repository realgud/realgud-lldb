;; Copyright (C) 2016-2017 Rocky Bernstein

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; lldb debugger

(eval-when-compile (require 'cl-lib))

(require 'realgud)

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:lldb-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  the values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

(defconst realgud:lldb-frame-file-regexp
  (format "\\(.+\\):%s" realgud:regexp-captured-num))

(defconst realgud:lldb-frame-start-regexp
  "\\(?:^\\|\n\\)")

;; Some versions of lldb insert "frame" and some don't.
(defconst realgud:lldb-frame-num-regexp
  (format "[ ]*\\(?:frame \\)?#%s[:]? "
	  realgud:regexp-captured-num))

(setf (gethash "loc-callback-fn" realgud:lldb-pat-hash) 'realgud:lldb-loc-fn-callback)

;; realgud-loc-pat that describes a lldb location generally shown
;; before a command prompt.
;; For example:
;; * thread #1: tid = 12866, 0x00000000004004b4 hello`main(argc=1, argv=0x00007fffffffd668) + 4 at hello.c:5, name = 'hello', stop reason = breakpoint 1.1
(setf (gethash "loc" realgud:lldb-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^\\* thread #%s: .+ at %s, "
		       realgud:regexp-captured-num realgud:lldb-frame-file-regexp)
       :file-group 2
       :line-group 3))

;; Top frame number
(setf (gethash "top-frame-num" realgud:lldb-pat-hash) 0)

;; realgud-loc-pat that describes a lldb frame generally shown
;; before a command prompt or in frame switching commands
;;  frame #1: 0x00000000004015e2 ctest`main(argc=1, argv=0x00007fffffffd778) + 90 at ctest.c:83
;; Some versions of lldb give:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; instead

(setf (gethash "selected-frame" realgud:lldb-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(format "^%s.* at %s"
			realgud:lldb-frame-num-regexp
			realgud:lldb-frame-file-regexp
			)
       :num 1
       :file-group 2
       :line-group 3)
      )

;; realgud-loc-pat that describes a lldb prompt
;; For example:
;;   (lldb)
(setf (gethash "prompt" realgud:lldb-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(lldb) "
       ))

;; realgud-loc-pat that describes a "breakpoint set" line
;; For example:
;;   Breakpoint 1: where = hello`main + 4 at hello.c:5, address = 0x00000000004004b4
(setf (gethash "brkpt-set" realgud:lldb-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s: .* at \\(.+\\):%s, "
		       realgud:regexp-captured-num realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3))

;; realgud-loc-pat that describes a lldb "backtrace" command line.
;; For example:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; #1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 "/tmp/remake/remake") at strdup.c:42
;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
;;    at main.c:952
;; #46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0,
;;    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410

(setf (gethash "debugger-backtrace" realgud:lldb-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud:lldb-frame-start-regexp
			realgud:lldb-frame-num-regexp
			"\\(?:.\\|\\(?:[\n] \\)\\)+[ ]+at "
			realgud:lldb-frame-file-regexp
			)
       :num 1
       :file-group 2
       :line-group 3)
      )

(setf (gethash "font-lock-keywords" realgud:lldb-pat-hash)
      '(
	;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
	;;    at main.c:952
	("[ \n]+at \\(.*\\):\\([0-9]+\\)"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))

	;; The frame number and first type name, if present.
	;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;;      ---^
	( "#\\(?:^\\|\n\\)\\([0-9]+\\)  "
	 (1 realgud-backtrace-number-face))
	))

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger
(setf (gethash "lldb" realgud:variable-basename-hash) "realgud:lldb")

(defvar realgud:lldb-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'continue' and the value is
  the lldb command to use, like 'process continue'")

(setf (gethash "break"    realgud:lldb-command-hash) "b %l")
(setf (gethash "delete"   realgud:lldb-command-hash) "break delete %p")
(setf (gethash "clear"    realgud:lldb-command-hash) "break clear %X:%l")
(setf (gethash "continue" realgud:lldb-command-hash) "process continue")
(setf (gethash "eval"     realgud:lldb-command-hash) "print %s")
(setf (gethash "finish"   realgud:lldb-command-hash) "thread step out")
(setf (gethash "quit"     realgud:lldb-command-hash) "quit")
(setf (gethash "run"      realgud:lldb-command-hash) "run")
(setf (gethash "step"     realgud:lldb-command-hash) "thread step-in --count %p")
(setf (gethash "lldb" realgud-command-hash) realgud:lldb-command-hash)

(setf (gethash "lldb" realgud-pat-hash) realgud:lldb-pat-hash)

(provide-me "realgud:lldb-")
