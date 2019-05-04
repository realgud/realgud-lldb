;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "realgud.elc")) (file-name-directory (locate-library "loc-changes.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'realgud)
(load-file "../lldb/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)
(declare-function prompt-match          'regexp-helper)
(declare-function loc-match	        'realgud-helper)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)      (defvar realgud-pat-hash) (defvar realgud-bt-hash)
  (defvar loc-pat)       (defvar prompt-pat)       (defvar bps-pat)
  (defvar file-group)    (defvar line-group)       (defvar test-pos)
  (defvar test-dbgr)     (defvar test-text)        (defvar realgud-bt-pat)
  (defvar realgud-bt-re) (defvar realgud--lldb-pat-hash)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "lldb")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group loc-pat)
		  :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
;; (setq test-text "/home/rocky/c/ctest.c:80:2000:beg:0x8048748>")
;; (note "traceback location matching")

;; (assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
;; (assert-equal "/home/rocky/c/ctest.c"
;; 	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
;; 			    test-text) "extract file name")
;; (assert-equal "80"
;; 	      (match-string (realgud-cmdbuf-info-line-group test-dbgr)
;; 			    test-text) "extract line number")
(note "debugger-backtrace")
(setq realgud-bt-pat  (gethash "debugger-backtrace"
			       realgud--lldb-pat-hash))
(setq test-text
      "#0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
#1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 \"/tmp/remake/remake\") at strdup.c:42
#2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
    at main.c:952
#46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0,
    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410
")
(setq realgud-bt-re (realgud-loc-pat-regexp realgud-bt-pat))
(setq file-group (realgud-loc-pat-file-group realgud-bt-pat))
(setq line-group (realgud-loc-pat-line-group realgud-bt-pat))
(assert-equal 0 (string-match realgud-bt-re test-text))

(assert-equal "main.c"
	      (substring test-text
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "935"
	      (substring test-text
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 65 test-pos)
(assert-equal 65 (string-match realgud-bt-re test-text test-pos))
(assert-equal "strdup.c"
	      (substring test-text
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "42"
	      (substring test-text
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 149 test-pos)
(assert-equal 149 (string-match realgud-bt-re test-text test-pos))
(assert-equal "main.c"
	      (substring test-text
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "952"
	      (substring test-text
			 (match-beginning line-group)
			 (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 233 test-pos)
(assert-equal 233 (string-match realgud-bt-re test-text test-pos))
(assert-equal "vm_insnhelper.c"
	      (substring test-text
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "410"
	      (substring test-text
			 (match-beginning line-group)
			 (match-end line-group)))

(set (make-local-variable 'bps-pat)
     (gethash "brkpt-set"  realgud--lldb-pat-hash))

(setq test-text "Breakpoint 1: where = solptest`main + 9 at unit_test_main.ipp:303:12, address = 0x00000001002380d9")

(assert-t (numberp (loc-match test-text bps-pat))
	  "breakpoint location with column")


(assert-equal "1"
	      (match-string (realgud-loc-pat-num
			     bps-pat) test-text)
	      "extract breakpoint number")

(assert-equal "unit_test_main.ipp"
	      (match-string (realgud-loc-pat-file-group
			     bps-pat) test-text)
	      "extract breakpoint file name")

(assert-equal "303"
	      (match-string (realgud-loc-pat-line-group
			     bps-pat) test-text)
	      "extract breakpoint line number")

(assert-equal "12"
	      (match-string (realgud-loc-pat-column-group
			     bps-pat) test-text)
	      "extract breakpoint line number")


(setq test-text "Breakpoint 2: where = solptest`main + 9 at /tmp/foo.c:63, address = 0x00000001002380d9")

(assert-t (numberp (loc-match test-text bps-pat))
	  "breakpoint location without column")


(assert-equal "2"
	      (match-string (realgud-loc-pat-num
			     bps-pat) test-text)
	      "extract breakpoint number")

(assert-equal "/tmp/foo.c"
	      (match-string (realgud-loc-pat-file-group
			     bps-pat) test-text)
	      "extract breakpoint file name")

(assert-equal "63"
	      (match-string (realgud-loc-pat-line-group
			     bps-pat) test-text)
	      "extract breakpoint line number")



(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud--lldb-pat-hash))
(prompt-match "(lldb) ")

(end-tests)
