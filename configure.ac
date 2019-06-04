dnl FIXME: pick up from realgud.el
AC_INIT(realgud-lldb, 1.0.2,)
AC_CONFIG_SRCDIR(lldb/lldb.el)
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE

AC_PATH_PROG([EMACS], [emacs], [emacs])
AC_ARG_WITH(emacs, AC_HELP_STRING([--with-emacs],
                  [location of emacs program]), EMACS=$withval)

AC_MSG_NOTICE("Checking emacs version")
$EMACS -batch -q --no-site-file -eval \
  '(if (< emacs-major-version 25)
       (progn
         (error "You need GNU Emacs 25 or later.")
         (kill-emacs 1)
       )
   )'
if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue until above error is corrected.])
fi

##################################################################
# See if --with-lispdir was set. If not, set it to a reasonable default
# based on where bash thinks bashdb is supposed to be installed.
##################################################################

AM_MISSING_PROG(GIT2CL, git2cl, $missing_dir)

# Check whether --with-lispdir was given.
if test "${with_lispdir+set}" = set; then :
else
  my_lispdir=$(EMACS_PROG=$EMACS $SH_PROG $(dirname $0)/compute-lispdir.sh)
  if test "${my_lispdir+set}" = set; then :
    with_lispdir=$my_lispdir
    echo "'compute-lispdir.sh' lispdir install directory override: '$with_lispdir'"
  fi
fi

##
## Find out where to install the debugger emacs lisp files
##
AM_PATH_LISPDIR
lispdir_realgud=$lispdir/realgud
AC_SUBST([lispdir])
AC_SUBST([lispdir_realgud])

AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir_realgud" != "x")

AC_CONFIG_FILES([Makefile \
                 common.mk \
		 lldb/Makefile \
		 test/Makefile \
		 ])
AC_OUTPUT
