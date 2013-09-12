dnl
dnl Execute arbitrary emacs lisp
dnl
AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil \"${OUTPUT}\"))" >& AC_FD_CC 2>&1  
	${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x 'ignore) (prin1-to-string x)) nil \"${OUTPUT}\"nil 5))" >& AC_FD_CC 2>&1
	retval=`cat ${OUTPUT}`
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1=$retval
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_XEMACS_P, [
  AC_MSG_CHECKING([if $EMACS is really XEmacs])
  AC_EMACS_LISP(xemacsp,(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\") ,"noecho")
  XEMACS=${EMACS_cv_SYS_xemacsp}
  EMACS_FLAVOR=emacs
  if test "$XEMACS" = "yes"; then
     EMACS_FLAVOR=xemacs
  fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)
])

AC_DEFUN(AC_PATH_LISPDIR, [
  AC_XEMACS_P
  if test "$prefix" = "NONE"; then
	AC_MSG_CHECKING([prefix for your Emacs])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(lispdir,          --with-lispdir            Where to install lisp files, lispdir=${withval})
  AC_MSG_CHECKING([where .elc files should go])
  if test -z "$lispdir"; then
    dnl Set default value
    theprefix=$prefix
    if test "x$theprefix" = "xNONE"; then
	theprefix=$ac_default_prefix
    fi
    lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp"
	   break
	fi
    done
  fi
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)
])

dnl
dnl Determine the emacs version we are running.
dnl Automatically substitutes @EMACS_VERSION@ with this number.
dnl
AC_DEFUN(AC_EMACS_VERSION, [
AC_MSG_CHECKING(for emacs version)
AC_EMACS_LISP(version,(and (boundp 'emacs-major-version) (format \"%d.%d\" emacs-major-version emacs-minor-version)),"noecho")

EMACS_VERSION=${EMACS_cv_SYS_version}
AC_SUBST(EMACS_VERSION)
AC_MSG_RESULT(${EMACS_VERSION})
])

dnl
dnl Determine whether the specified version of Emacs supports packages
dnl or not.  Currently, only XEmacs 20.3 does, but this is a general
dnl check.
dnl
AC_DEFUN(AC_EMACS_PACKAGES, [
AC_ARG_WITH(package-dir,      --with-package-dir        Configure as a XEmacs package in directory, [ EMACS_PACKAGE_DIR="${withval}"])
if test -n "$EMACS_PACKAGE_DIR"; then
  if test "$prefix" != "NONE"; then
	AC_MSG_ERROR([--with-package-dir and --prefix are mutually exclusive])
  fi
  dnl Massage everything to use $(prefix) correctly.
  prefix=$EMACS_PACKAGE_DIR
  datadir='$(prefix)/etc/w3'
  infodir='$(prefix)/info'
  lispdir='$(prefix)/lisp/w3'
fi
AC_SUBST(EMACS_PACKAGE_DIR)
])

dnl
dnl Check whether a function exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(progn (fmakunbound '$2) (condition-case nil (progn (require '$library) (fboundp '$2)) (error (prog1 nil (message \"$library not found\"))))),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
if test "${EMACS_cv_SYS_$1}" = "t"; then
	EMACS_cv_SYS_$1=yes
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
if test -z "$3"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Check whether a variable exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_VAR, [
AC_MSG_CHECKING(for $2 in $1)
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(progn (makunbound '$2) (condition-case nil (progn (require '$library) (boundp '$2)) (error nil))),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
AC_MSG_RESULT($HAVE_$1)
])

dnl
dnl Perform sanity checking and try to locate the custom and widget packages
dnl
AC_DEFUN(AC_CHECK_CUSTOM, [
AC_MSG_CHECKING(for acceptable custom library)
AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_CUSTOM,[
AC_EMACS_CHECK_LIB(widget,widget-convert-text,"noecho")
AC_EMACS_CHECK_LIB(wid_edit,widget-convert-text,"noecho")
if test "${HAVE_widget}" = "yes"; then
	EMACS_cv_ACCEPTABLE_CUSTOM=yes
else
	if test "${HAVE_wid_edit}" != "no"; then
		EMACS_cv_ACCEPTABLE_CUSTOM=yes
	else
		EMACS_cv_ACCEPTABLE_CUSTOM=no
	fi
fi
if test "${EMACS_cv_ACCEPTABLE_CUSTOM}" = "yes"; then
	AC_EMACS_LISP(widget_dir,(file-name-directory (locate-library \"widget\")),"noecho")
	EMACS_cv_ACCEPTABLE_CUSTOM=$EMACS_cv_SYS_widget_dir
fi
])
   AC_ARG_WITH(custom,           --with-custom             Specify where to find the custom package, [ EMACS_cv_ACCEPTABLE_CUSTOM=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   CUSTOM=${EMACS_cv_ACCEPTABLE_CUSTOM}
   AC_SUBST(CUSTOM)
   AC_MSG_RESULT("${CUSTOM}")
])

dnl
dnl Perform sanity checking and try to locate the gnus package
dnl
AC_DEFUN(AC_CHECK_GNUS, [
AC_MSG_CHECKING(for recent gnus version)
AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_GNUS,[
AC_EMACS_CHECK_LIB(mm_decode, mm-get-content-id,"noecho")
if test "${HAVE_mm_decode}" = "yes"; then
	EMACS_cv_ACCEPTABLE_GNUS=yes
else
	EMACS_cv_ACCEPTABLE_GNUS=no
fi

if test "${EMACS_cv_ACCEPTABLE_GNUS}" = "yes"; then
	AC_EMACS_LISP(gnus_dir,(file-name-directory (locate-library \"mm-decode\")),"noecho")
	EMACS_cv_ACCEPTABLE_GNUS=$EMACS_cv_SYS_gnus_dir
fi
])
   AC_ARG_WITH(gnus,           --with-gnus               Specify where to find the gnus package, [ EMACS_cv_ACCEPTABLE_GNUS=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   GNUS=${EMACS_cv_ACCEPTABLE_GNUS}
   AC_SUBST(GNUS)
   AC_MSG_RESULT("${GNUS}")
])

dnl
dnl Perform sanity checking and try to locate the URL package
dnl
AC_DEFUN(AC_CHECK_URL, [
AC_MSG_CHECKING(for recent URL version)
AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_URL,[
AC_EMACS_CHECK_LIB(url_methods, url-scheme-get-property,"noecho")
if test "${HAVE_url_methods}" = "yes"; then
	EMACS_cv_ACCEPTABLE_URL=yes
else
	EMACS_cv_ACCEPTABLE_URL=no
fi

if test "${EMACS_cv_ACCEPTABLE_URL}" = "yes"; then
	AC_EMACS_LISP(url_dir,(file-name-directory (locate-library \"url-methods\")),"noecho")
	EMACS_cv_ACCEPTABLE_URL=$EMACS_cv_SYS_url_dir
fi
])
   AC_ARG_WITH(url,             --with-url                Specify where to find the URL package, [ EMACS_cv_ACCEPTABLE_URL=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   URL=${EMACS_cv_ACCEPTABLE_URL}
   AC_SUBST(URL)
   AC_MSG_RESULT("${URL}")
])


dnl
dnl Figure out how we can rebuild the custom-load.el files
dnl
AC_DEFUN(AC_CHECK_CUSTOMLOADS, [
AC_MSG_CHECKING(how to rebuild custom autoloads)
AC_CACHE_VAL(EMACS_cv_REBUILD_CUSTOMLOADS,[
AC_EMACS_CHECK_LIB(cus_dep,Custom-make-dependencies,"noecho")
EMACS_cv_REBUILD_CUSTOMLOADS=${HAVE_cus_dep}
])
if test "${EMACS_cv_REBUILD_CUSTOMLOADS}" != "no"; then
   REBUILD_CUSTOMLOADS='$(EMACS) $(BATCHFLAGS) -l cus-dep -f Custom-make-dependencies $(srcdir)'
else
   REBUILD_CUSTOMLOADS='$(EMACS) $(BATCHFLAGS) $(DEPS) -f emacs-batch-build-custom-load $(srcdir)'
fi
AC_MSG_RESULT("${REBUILD_CUSTOMLOADS}")
AC_SUBST(REBUILD_CUSTOMLOADS)
])
