[![Build Status][travis-image]][travis-url]
[![MELPA][melpa-image]][melpa]
[![MELPA Stable][melpa-stable-image]][melpa-stable]

Introduction
============

Emacs Lisp Module to add [lldb](https://lldb.llvm.org/) support to [realgud](http://github.com/realgud/realgud).

Installation
=============

From ELPA or MELPA
-------------------

Inside GNU Emacs evaluate:

```lisp
  (package-install realgud-lldb)
```


From github source
------------------

* Have `realgud` and `test-simple` installed.
* From inside GNU Emacs, evaluate:
```lisp
  (compile (format "EMACSLOADPATH=:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc"))))
```

[travis-image]: https://api.travis-ci.org/realgud/realgud-lldb.svg?branch=master
[travis-url]: https://travis-ci.org/realgud/realgud-lldb
[melpa-stable-image]: http://stable.melpa.org/packages/realgud-lldb-badge.svg
[melpa-stable]: http://stable.melpa.org/#/realgud-lldb
[melpa-image]: http://melpa.org/packages/realgud-lldb-badge.svg
[melpa]: http://melpa.org/#/realgud-lldb
