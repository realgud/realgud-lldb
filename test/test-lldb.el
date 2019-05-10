;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../lldb/core.el")
(load-file "./regexp-helper.el")

(eval-when-compile
  (defvar realgud--lldb-minibuffer-history)
  (defvar test:realgud-lldb-executable-save)
  (defvar test:realgud-minibuffer-history-save)
)

(declare-function realgud--lldb-suggest-invocation 'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:realgud-lldb-executable-save (symbol-function 'realgud--lldb-executable))
(setq test:realgud-minibuffer-history-save realgud--lldb-minibuffer-history)

(defun realgud--lldb-executable (filename)
  "Mock function for testing"
  (cond ((equal filename "bar.sh") 7)
	((equal filename "foo") 8)
	((equal filename "baz") 8)
	(t 3)))

(defun lldb-test()
  (note "realgud--lldb-suggest-invocation")
  (setq realgud--lldb-minibuffer-history nil)
  (let ((my-directory (file-name-directory (__FILE__))))
    (save-excursion
      (note "Test preference to buffer editing")
      (setq default-directory
	    (concat my-directory "lldb"))
      (find-file-literally "foo.c")
      (assert-equal "lldb foo" (realgud--lldb-suggest-invocation)
		    "Should find file sans extension - foo")
      (find-file-literally "baz.c")
      (assert-equal "lldb baz" (realgud--lldb-suggest-invocation)
		    "Should find file sans extension - baz")
      )
    (save-excursion
      (note "Pick up non-sans executable")
      (setq default-directory
	    (concat my-directory  "lldb/test2"))
      ;; (assert-equal "lldb bar.sh" (realgud--lldb-suggest-invocation))
      (setq realgud--lldb-minibuffer-history '("lldb testing"))
      (setq default-directory
	    (concat my-directory  "lldb/test2"))
      (assert-equal "lldb testing" (realgud--lldb-suggest-invocation)
		    "After setting minibuffer history - takes precidence")
      )
    (setq default-directory my-directory)
    )
  )
(lldb-test)
(end-tests)

;; Restore the old values.
;; You might have to run the below if you run this interactively.
(fset 'realgud--lldb-executable test:realgud-lldb-executable-save)
(setq realgud--lldb-minibuffer-history test:realgud-minibuffer-history-save)
(setq default-directory (file-name-directory (__FILE__)))
