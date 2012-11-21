## This file contains various m4 macros that can be included in your
## own projects to enable proper detection of the scripting languages
## In your own aclocal.m4 file, you can use syntax like
##   include(gnatcoll/aclocal.m4)

##############################################################
# Machine-specific linker switches
#   @EXTRA_LINK_SWITCHES@: list of system-specific linker
#      switches. Its syntax is compatible with GPR files.
##############################################################

AC_DEFUN(AM_SYSTEM_LINK_SWITCHES,
[
    case $build_os in
       *darwin*)   EXTRA_LINK_SWITCHES='"-Wl,-no_pie"';;
       *)          EXTRA_LINK_SWITCHES='';;
    esac
    AC_SUBST(EXTRA_LINK_SWITCHES)
])

##############################################################
# Copy a file, as part of config.status
#  AM_LINK_FILE(SOURCE,DEST)
##############################################################

AC_DEFUN(AM_LINK_FILE,
[
   AC_CONFIG_COMMANDS([$2],
                      [rm -f $2
                       cp -f $1 $2], [$3])
])

##############################################################
# Checking for build type
# The following variable is exported by configure:
#   @BUILD_TYPE@: either "Production" or "Debug"
##############################################################

AC_DEFUN(CHECK_BUILD_TYPE,
[
  AC_ARG_ENABLE(build,
    [AC_HELP_STRING(
       [--enable-build=<type>],
       [Default build type for the library (Debug, Production)])],
    BUILD_TYPE=$enableval,
    BUILD_TYPE=Production)
  AC_SUBST(BUILD_TYPE)
])

#############################################################
# Check whether gnatmake can compile, bind and link an Ada program
#    AM_TRY_ADA(gnatmake,filename,content,success,failure)
#############################################################

AC_DEFUN(AM_TRY_ADA,
[
   mkdir conftest
   cat > conftest/src.ada <<EOF
[$3]
EOF
   if AC_TRY_COMMAND([cd conftest && gnatchop -q src.ada && $1 $2 >/dev/null 2>../conftest.out])
   then
      : Success
      $4
   else
      : Failure
      $5
   fi
   rm -rf conftest*
])

#############################################################
# Check whether platform/GNAT supports atomic increment/decrement
# operations.
# The following variable is then set:
#     SYNC_COUNTERS_IMPL
# to either "intrinsic" or "mutex"
# Code comes from the polyorb configure.ac
#############################################################

AC_DEFUN(AM_HAS_INTRINSIC_SYNC_COUNTERS,
[
  AC_MSG_CHECKING([whether platform supports atomic inc/dec])
  AM_TRY_ADA([gnatmake], [check.adb],
[
with Interfaces; use Interfaces;
procedure Check is
   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32;
   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   X : aliased Interfaces.Integer_32;
   Y : Interfaces.Integer_32 := 0;
   pragma Volatile (Y);
   --  On some platforms (e.g. i386), GCC has limited support for
   --  __sync_add_and_fetch_4 for the case where the result is not used.
   --  Here we want to test for general availability, so make Y volatile to
   --  prevent the store operation from being discarded.
begin
   Y := Sync_Add_And_Fetch (X'Access, 1);
end Check;
],
[
   AC_MSG_RESULT(yes)
   SYNC_COUNTERS_IMPL="intrinsic"
],[
   AC_MSG_RESULT(no)
   SYNC_COUNTERS_IMPL="mutex"
])

   AC_SUBST(SYNC_COUNTERS_IMPL)
])

#############################################################
# Check whether we have the GNAT sources available
# The following variables are exported by configure:
#   @WITH_PROJECTS@: "yes" or "no"
#   GNAT_SOURCES: "gnat_util", "copy", "no"
#############################################################

AC_DEFUN(AM_GNAT_SOURCES,
[
  AC_MSG_CHECKING(whether gnat sources are found)
  if test -d gnat_src; then
     AC_MSG_RESULT(yes)
     HAS_GNAT_SOURCES=yes
     GNAT_SOURCES=copy
  else
     AC_MSG_RESULT(no)

     AC_MSG_CHECKING(whether gnat_util exists)
     AM_PATH_PROJECT(gnat_util, HAVE_GNAT_UTIL)
     if test "$HAVE_GNAT_UTIL" = "yes"; then
        AC_MSG_RESULT(yes);
        HAS_GNAT_SOURCES=yes
        GNAT_SOURCES=gnat_util
     else
        AC_MSG_RESULT(no)
        HAS_GNAT_SOURCES=no
        GNAT_SOURCES=copy
     fi
  fi

  AC_SUBST(GNAT_SOURCES)
])

AC_DEFUN(AM_PROJECTS,
[
  # Allow the user to disable projects support so that gnatcoll does not depend
  # on the installed gnat compiler (gnat_utils project)
  AC_ARG_ENABLE(projects,
    AC_HELP_STRING(
      [--disable-projects],
      [Disable support for GNAT Projects [[default=enabled]]]),
    [WITH_PROJECTS=$enableval],
    [WITH_PROJECTS=$HAS_GNAT_SOURCES])

  AC_SUBST(WITH_PROJECTS)
])

#############################################################
# Check whether GNAT on that target supports building shared
# libraries
# The following variables are exported by configure:
#   @GNAT_BUILDS_SHARED@: either "yes" or "no"
#   @DEFAULT_LIBRARY_TYPE@: either "static" or "relocatable"
#############################################################

AC_DEFUN(AM_GNAT_BUILDS_SHARED,
[
   AC_MSG_CHECKING(whether gnat can build shared libs)

   DEFAULT_LIBRARY_TYPE=static

   AC_ARG_ENABLE(shared,
     [AC_HELP_STRING(
        [--disable-shared],
        [Disable building of shared libraries])
AC_HELP_STRING(
        [--enable-shared],
        [Build shared libraries if supported on the target
Make them the installation default])],
     [GNAT_BUILDS_SHARED=$enableval
      if test $enableval = yes; then
         DEFAULT_LIBRARY_TYPE=relocatable
      fi],
     [GNAT_BUILDS_SHARED=yes])

   if test x$GNAT_BUILDS_SHARED = xyes; then
      # Create a temporary directory (from "info autoconf")
      : ${TMPDIR=/tmp}
      {
        tmp=`(umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null` \
           && test -n "$tmp" && test -d "$tmp"
      } || {
        tmp=$TMPDIR/foo$$-$RANDOM
        (umask 077 && mkdir -p "$tmp")
      } || exit $?

      mkdir $tmp/lib
      echo "package Foo is end Foo;" > $tmp/foo.ads
      cat > $tmp/lib.gpr <<EOF
project Lib is
   for Source_Dirs use (".");
   for Library_Dir use "lib";
   for Library_Name use "lib";
   for Library_Kind use "relocatable";
end Lib;
EOF

      gnatmake -c -q -P$tmp/lib 2>/dev/null
      if test $? = 0 ; then
         GNAT_BUILDS_SHARED=yes
      else
         GNAT_BUILDS_SHARED=no
         DEFAULT_LIBRARY_TYPE=static
      fi
      rm -rf $tmp
      AC_MSG_RESULT($GNAT_BUILDS_SHARED)
   else
      AC_MSG_RESULT([no (--disabled-shared)])
   fi

   AC_SUBST(GNAT_BUILDS_SHARED)
   AC_SUBST(DEFAULT_LIBRARY_TYPE)
])

#############################################################
# Checking for syslog
# This checks whether syslog exists on this system.
# This module can be disabled with
#    --disable-syslog
# The following variables are exported by configure:
#    @WITH_SYSLOG@: either "yes" or "no"
############################################################

AC_DEFUN(AM_PATH_SYSLOG,
[
   AC_ARG_ENABLE(syslog,
     AC_HELP_STRING(
        [--disable-syslog],
        [Disable support for syslog [[default=enabled]]]),
     [WITH_SYSLOG=$enableval],
     [WITH_SYSLOG=yes])

   if test x$WITH_SYSLOG = xyes ; then
     AC_CHECK_HEADER([syslog.h],
                     [WITH_SYSLOG=yes],
                     [WITH_SYSLOG=no])
   fi

   AC_SUBST(WITH_SYSLOG)
])

#############################################################
# Search for a library anywhere on LD_LIBRARY_PATH
# This will return the empty string if not found, and the directory
# otherwise.
# This can be used to add -Ldir parameters on the command line: if
# the library was found with AC_CHECK_LIB but not this macro, this
# means it is in the standard search path and no command line switch
# is required.
#    AM_LIB_PATH(libname)
# Sets am_path_<libname> to the path
#############################################################

AC_DEFUN(AM_LIB_PATH,
[
   am_path_$1=""
   as_save_IFS=$IFS; IFS=$PATH_SEPARATOR
   for lib_dir in $LD_LIBRARY_PATH
   do
      IFS=$as_save_IFS
      if test -f "$lib_dir/lib$1.so"; then
         am_path_$1=$lib_dir
         break
      elif test -f "$lib_dir/lib$1.dll"; then
         am_path_$1=$lib_dir
         break
      elif test -f "$lib_dir/lib$1.dylib"; then
         am_path_$1=$lib_dir
         break
      fi
   done
   IFS=$as_save_IFS
])

#############################################################
# Checking for postgreSQL
# This checks whether the libpq exists on this system
# This module can be disabled with
#    --with-postgresql=path
# The following variables are exported by configure:
#   @WITH_POSTGRES@: whether postgres was detected
#   @PATH_LIBPQ@: path to libpq, or "" if not found
#   @HAS_PQPREPARE@: whether the version of postgreSQL has PQprepare ()
#############################################################

AC_DEFUN(AM_PATH_POSTGRES,
[
   NEED_PSQL=no
   AC_ARG_WITH(postgresql,
     [AC_HELP_STRING(
       [--with-postgresql=<path>],
       [Specify the full path to the PostgreSQL installation])
AC_HELP_STRING(
       [--without-postgresql],
       [Disable PostgreSQL support])],
     [POSTGRESQL_PATH_WITH=$withval; NEED_PSQL=yes],
     POSTGRESQL_PATH_WITH=yes)

   PATH_LIBPQ=""
   if test x"$POSTGRESQL_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for PostgreSQL)
      AC_MSG_RESULT(no, use --with-postgresql if needed)
      WITH_POSTGRES=no

   else
     if test x"$POSTGRESQL_PATH_WITH" = xyes ; then
       PATH_LIBPQ=`pg_config | grep ^LIBDIR | cut -d\  -f3`
       if test x"$PATH_LIBPQ" != x ; then
          PATH_LIBPQ="-L$PATH_LIBPQ"
       else
          AM_LIB_PATH(pq)
          if test x"$am_path_pq" != x ; then
             PATH_LIBPQ="-L$am_path_pq"
          fi
       fi
       AC_CHECK_LIB(pq,PQreset,WITH_POSTGRES=yes,WITH_POSTGRES=no,[$PATH_LIBPQ])
     else
       WITH_POSTGRES=yes
       if test -f "$POSTGRESQL_PATH_WITH/libpq.so" -o -f "$POSTGRESQL_PATH_WITH/libpq.dll" -o -f "$POSTGRESQL_PATH_WITH/libpq.dylib" ; then
          PATH_LIBPQ="-L$POSTGRESQL_PATH_WITH"
       elif test -f "$POSTGRESQL_PATH_WITH/lib/libpq.so" -o -f "$POSTGRESQL_PATH_WITH/lib/libpq.dll" -o -f "$POSTGRESQL_PATH_WITH/lib/libpq.dylib" ; then
          PATH_LIBPQ="-L$POSTGRESQL_PATH_WITH/lib"
       else
          AC_MSG_CHECKING(for PostgreSQL)
          AC_MSG_RESULT(not found in $POSTGRESQL_PATH_WITH)
          WITH_POSTGRES=no
       fi
     fi

     if test x"$WITH_POSTGRES" = xno -a x"$NEED_PSQL" = xyes ; then
       AC_MSG_ERROR([PostgreSQL not found])
     fi
   fi

   if test x"$WITH_POSTGRES" = xyes ; then
     AC_CHECK_LIB(pq,PQprepare,HAS_PQPREPARE=yes,HAS_PQPREPARE=no,[$PATH_LIBPQ])
   else
     HAS_PQPREPARE=no
   fi

   AC_SUBST(WITH_POSTGRES)
   AC_SUBST(PATH_LIBPQ)
   AC_SUBST(HAS_PQPREPARE)
])

j############################################################
# Checking for sqlite
# This checks whether sqlite is installed on the system. It can
# be disabled with
#    -with-sqlite=no
# The following variables are exported by configure:
#    @WITH_SQLITE@: whether sqlite is detected
#    @PATH_LIBSQLITE@: path to libsqlite3
#############################################################

AC_DEFUN(AM_PATH_SQLITE,
[
   NEED_SQLITE=no
   AC_ARG_WITH(sqlite,
     [AC_HELP_STRING(
        [--with-sqlite=<path>],
        [Specify the full path to the sqlite installation, or "embedded"])
AC_HELP_STRING(
        [--without-sqlite],
        [Disable sqlite support])],
     [SQLITE_PATH_WITH=$withval; NEED_SQLITE=yes],
     SQLITE_PATH_WITH=yes)

   PATH_LIBSQLITE=""
   if test x"$SQLITE_PATH_WITH" = xembedded ; then
      AC_MSG_CHECKING(for sqlite)
      AC_MSG_RESULT(embedded, use --with-sqlite to use a dynamic lib)
      WITH_SQLITE=embedded

   else
      if test x"$SQLITE_PATH_WITH" = xno ; then
        AC_MSG_CHECKING(for sqlite)
        AC_MSG_RESULT(no, use --with-sqlite to use a dynamic lib)
        WITH_SQLITE=no
      else
         if test x"$SQLITE_PATH_WITH" != xyes ; then
           PATH_LIBSQLITE="-L$SQLITE_PATH_WITH/lib"
         fi

         AC_CHECK_LIB(sqlite3, sqlite3_open,
                      [WITH_SQLITE=yes],
                      [WITH_SQLITE=no],
                      $SQLITE_CFLAGS $PATH_LIBSQLITE)

         if test x"$WITH_SQLITE" = xno ; then
            AC_MSG_CHECKING(for sqlite)
            AC_MSG_RESULT(embedded, use --with-sqlite to use a dynamic lib)
            WITH_SQLITE=embedded
         fi
      fi
   fi

   AC_SUBST(WITH_SQLITE)
   AC_SUBST(PATH_LIBSQLITE)

])

#############################################################
# Checking for gmp
# This checks whether the gnu multiprecision library is available.
# The result can be forced by using the
#    --with-gmp=path
# The following variables are exported on exit:
#   @GMP_CFLAGS@:  Compiler flags
#   @GMP_LIBS@:    Extra command line switches for the linker
#   @WITH_GMP@:    "yes" or "no" depending on whether gmp is available
#############################################################

AC_DEFUN(AM_PATH_GMP,
[
   AC_ARG_WITH(gmp,
     [AC_HELP_STRING(
       [--with-gmp=<path>],
       [Specify the full path of the gmp install])
AC_HELP_STRING(
       [--without-gmp],
       [Disable support for gmp])],
     GMP_PATH_WITH=$withval,
     GMP_PATH_WITH=yes)

   GMP_CFLAGS=""
   GMP_LIBS=""
   if test x"$GMP_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for gmp)
      AC_MSG_RESULT(no, use --with-gmp if needed)
      WITH_GMP=no

   else
     if test x"$GMP_PATH_WITH" = xyes ; then
       AC_CHECK_LIB(gmp,__gmpz_init,WITH_GMP=yes,WITH_GMP=no)
       AC_CHECK_HEADER(gmp.h, [], [WITH_GMP=no])
       GMP_LIBS="-lgmp"
     else
       GMP_LIBS="-L$GMP_PATH_WITH/lib -lgmp"
       GMP_CFLAGS="-I$GMP_PATH_WITH/include"
       WITH_GMP=yes
     fi
   fi

   AC_SUBST(WITH_GMP)
   AC_SUBST(GMP_CFLAGS)
   AC_SUBST(GMP_LIBS)
])

#############################################################
# Checking for readline
#############################################################

AC_DEFUN(AM_CHECK_READLINE,
[
   AC_ARG_ENABLE(readline,
     [AC_HELP_STRING(
        [--disable-readline],
        [Disable support for readline])],
     WITH_READLINE=$enableval,
     WITH_READLINE="")

   if test "$WITH_GPL" = "no" ; then
      AC_MSG_CHECKING(for readline)
      AC_MSG_RESULT([no, this is a pure GPL library (see --enable-gpl)])
      WITH_READLINE=no
   elif test "$WITH_READLINE" = "" ; then
      AC_CHECK_LIB(readline,readline,WITH_READLINE=yes,WITH_READLINE=no)
   elif test "$WITH_READLINE" = "yes" ; then
      AC_CHECK_LIB(readline,readline,WITH_READLINE=yes,WITH_READLINE=no)
      if test "$WITH_READLINE" = "no" ; then
         AC_MSG_ERROR([Readline not found])
      fi
   fi

   AC_SUBST(WITH_READLINE)
])

#############################################################
# Checking for python
# This checks whether python is available on the system, and if yes
# what the paths are. The result can be forced by using the
#    --with-python=path
# command line switch
# The following variables are exported by configure on exit:
#    @PYTHON_BASE@:    Either "no" or the directory that contains python
#    @PYTHON_VERSION@: Version of python detected
#    @PYTHON_CFLAGS@:  Compiler flags to use for python code
#    @PYTHON_DIR@:     Directory for libpython.so
#    @PYTHON_LIBS@:    extra command line switches to pass to the linker.
#    @WITH_PYTHON@: either "yes" or "no" depending on whether
#                      python support is available.
#############################################################

AC_DEFUN(AM_PATH_PYTHON,
[
   NEED_PYTHON=no

   AC_ARG_WITH(python,
     [AC_HELP_STRING(
       [--with-python=<path>],
       [Specify the prefix of the Python installation])
AC_HELP_STRING(
       [--without-python],
       [Disable python support])],
     [PYTHON_PATH_WITH=$withval; NEED_PYTHON=$PYTHON_PATH_WITH],
     PYTHON_PATH_WITH=yes)
   AC_ARG_WITH(python-exec,
     [AC_HELP_STRING(
        [--with-python-exec=<path>],
        [forces a specific python executable (python3 for instance)])],
     [PYTHON_EXEC=$withval])
   AC_ARG_ENABLE(shared-python,
     AC_HELP_STRING(
       [--enable-shared-python],
       [Link with shared python library instead of static]),
     PYTHON_SHARED=$enableval,
     PYTHON_SHARED=no)

   if test "$PYTHON_EXEC" = ""; then
      PYTHON_EXEC="python"
   fi

   WITH_PYTHON=yes
   if test x"$PYTHON_PATH_WITH" = xno ; then
      AC_MSG_CHECKING(for python)
      AC_MSG_RESULT(no, use --with-python if needed)
      PYTHON_BASE=no
      WITH_PYTHON=no

   else
      AC_PATH_PROG(PYTHON, ${PYTHON_EXEC}, no, $PYTHON_PATH_WITH/bin:$PYTHON_PATH_WITH:$PATH)
      if test x"$PYTHON" = xno ; then
         PYTHON_BASE=no
         WITH_PYTHON=no
      else
        AC_MSG_CHECKING(for python >= 2.0)
        if test x"$PYTHON_PATH_WITH" != xyes ; then
           PYTHON_BASE=$PYTHON_PATH_WITH
        else
           PYTHON_BASE=`$PYTHON -c 'import sys; print(sys.prefix)' `
        fi

        PYTHON_MAJOR_VERSION=`$PYTHON -c 'import sys; print(sys.version_info[[0]])' 2>/dev/null`
        PYTHON_MINOR_VERSION=`$PYTHON -c 'import sys; print(sys.version_info[[1]])' 2>/dev/null`
        if test $PYTHON_MAJOR_VERSION -lt 2 ; then
           AC_MSG_RESULT(no, need at least version 2.0)
           PYTHON_BASE=no
           WITH_PYTHON=no
        else
           case "${host}" in
             # On windows, there are two possible types of installation for
             # python, where the layout of directories are different. Even when
             # running cygwin, we might want to link with a mingwin version of
             # python for easier redistribution of the application.

             *-*mingw32* | *cygwin* )
               if test -d ${PYTHON_BASE}/lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/config; then
                 PYTHON_WIN32=no
               else
                 PYTHON_WIN32=yes
               fi
               ;;
             *)
               PYTHON_WIN32=no
               ;;
           esac

           if test x$PYTHON_WIN32 == xyes; then
             PYTHON_VERSION=$PYTHON_MAJOR_VERSION$PYTHON_MINOR_VERSION
             PYTHON_DIR=${PYTHON_BASE}/libs
           else
             PYTHON_VERSION=$PYTHON_MAJOR_VERSION.$PYTHON_MINOR_VERSION
             if test x$PYTHON_SHARED = xyes; then
                PYTHON_DIR=${PYTHON_BASE}/lib
             else
                PYTHON_DIR=${PYTHON_BASE}/lib/python${PYTHON_VERSION}/config
             fi
           fi

           AC_MSG_RESULT(yes (version $PYTHON_MAJOR_VERSION.$PYTHON_MINOR_VERSION))
        fi
      fi
   fi

   if test x"$PYTHON_BASE" != xno; then
      # Find the libs that are required to link with python. We first try
      # with python-config --libs, but this might not exist on the platform, or
      # might be incorrect, so we also have hard-coded fallbacks.

      if test $PYTHON_MAJOR_VERSION != 2; then
         PYCONFIG=python${PYTHON_MAJOR_VERSION}-config
      else
         PYCONFIG=python-config
      fi

      AC_PATH_PROG(PYTHON_CONFIG, ${PYCONFIG}, no, $PYTHON_PATH_WITH/bin:$PYTHON_PATH_WITH:$PATH)

      AC_MSG_CHECKING(if we can link with python (using python-config))
      if test x"$PYTHON_CONFIG" != xno ; then
         PYTHON_LIBS=`$PYTHON_CONFIG --libs`
         PYTHON_CFLAGS=`$PYTHON_CONFIG --includes`
      else
         PYTHON_LIBS=""
         PYTHON_CFLAGS=""
      fi

      SAVE_CFLAGS="${CFLAGS}"
      SAVE_LIBS="${LIBS}"
      CFLAGS="${SAVE_CFLAGS} ${PYTHON_CFLAGS}"
      LIBS="${SAVE_LIBS} ${PYTHON_LIBS}"


      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([
/*    will only work with gcc, but needed to use it with the mingwin python */
#define PY_LONG_LONG long long
#include <Python.h>
],[   Py_Initialize();])],
        [AC_MSG_RESULT(yes)],

        [
         AC_MSG_RESULT(no)
         AC_MSG_CHECKING(if we can link with python)
         # hard-code python dependencies
         if test \( -f ${PYTHON_DIR}/libpython${PYTHON_VERSION}.a \) -a \( ! x$PYTHON_SHARED = xyes \) ; then
            PYTHON_LIBS="${PYTHON_DIR}/libpython${PYTHON_VERSION}.a"
         else
            PYTHON_LIBS="-L${PYTHON_DIR} -lpython${PYTHON_VERSION}"
         fi

         case "${host}" in
            hppa*-hp-hpux1* )
               PYTHON_LIBS="-Wl,-E -lm ${PYTHON_LIBS}"
               ;;
            powerpc-ibm-aix5.* | powerpc-*-darwin* | *-darwin*)
               PYTHON_LIBS="-ldl -lm ${PYTHON_LIBS}"
               ;;
            *-sunos5.5* | *-solaris2.5* | *-sunos5* | *-solaris* )
               PYTHON_LIBS="-lresolv -lsocket -lnsl -ldl -lm ${PYTHON_LIBS}"
               ;;
            *linux* )
               PYTHON_LIBS="-Wl,-export-dynamic -lm -ldl ${PYTHON_LIBS}"
               ;;
            ia64-*hp-hpux11* )
               PYTHON_LIBS="-ldld -ldl -lm -Wl,-E ${PYTHON_LIBS}"
               ;;
            *-freebsd* )
               PYTHON_LIBS="-lm -lutil ${PYTHON_LIBS}"
               ;;
         esac

         if test x$PYTHON_WIN32 == xyes; then
            PYTHON_CFLAGS="-I${PYTHON_BASE}/include"
         else
            PYTHON_CFLAGS="-I${PYTHON_BASE}/include/python${PYTHON_VERSION}"
         fi

         # -lutil is almost always needed, for forkpty()
         CFLAGS="${SAVE_CFLAGS} ${PYTHON_CFLAGS}"
         LIBS="${SAVE_LIBS} ${PYTHON_LIBS}"

         AC_LINK_IFELSE(
           [AC_LANG_PROGRAM([
/* will only work with gcc, but needed to use it with the mingwin python */
#define PY_LONG_LONG long long
#include <Python.h>
],[Py_Initialize();])],
           [AC_MSG_RESULT(yes)],
           [LIBS="${LIBS} -lutil"
            AC_LINK_IFELSE(
             [AC_LANG_PROGRAM([
/* will only work with gcc, but needed to use it with the mingwin python */
#define PY_LONG_LONG long long
#include <Python.h>
],[Py_Initialize();])],
             [PYTHON_LIBS="${PYTHON_LIBS} -lutil"
            AC_MSG_RESULT(yes)],

            [LIBS="${LIBS} -lpthread -lutil -lz"
             AC_LINK_IFELSE(
               [AC_LANG_PROGRAM([
/* will only work with gcc, but needed to use it with the mingwin python */
#define PY_LONG_LONG long long
#include <Python.h>],[Py_Initialize();])],
               [PYTHON_LIBS="${PYTHON_LIBS} -lpthread -lutil -lz"
                AC_MSG_RESULT(yes)],

               [LIBS="${LIBS} -lpthread -lssl -lutil -lz"
                AC_LINK_IFELSE(
                  [AC_LANG_PROGRAM([
   /* will only work with gcc, but needed to use it with the mingwin python */
   #define PY_LONG_LONG long long
   #include <Python.h>],[Py_Initialize();])],
                  [PYTHON_LIBS="${PYTHON_LIBS} -lpthread -lssl -lutil -lz"
                   AC_MSG_RESULT(yes)],

               [AC_MSG_RESULT(no, [can't compile and link python example])
                WITH_PYTHON=no
                PYTHON_BASE=[]
                PYTHON_LIBS=[]])])])

        ])])

     # Restore an environment python-free, so that further tests are not
     # impacted in case we did not find python

     CFLAGS="${SAVE_CFLAGS}"
     LIBS="${SAVE_LIBS}"
   fi

   if test x"$WITH_PYTHON" = xno -a x"$NEED_PYTHON" != xno ; then
     AC_MSG_ERROR([Python not found])
   fi

   AC_SUBST(PYTHON_BASE)
   AC_SUBST(PYTHON_VERSION)
   AC_SUBST(PYTHON_DIR)
   AC_SUBST(PYTHON_LIBS)
   AC_SUBST(PYTHON_CFLAGS)
   AC_SUBST(WITH_PYTHON)
])

###########################################################################
## Checking for pygobject
##
###########################################################################

AC_DEFUN(AM_PATH_PYGOBJECT,
[
    AC_ARG_ENABLE(pygobject,
      AC_HELP_STRING(
        [--disable-pygobject],
        [Disable support for PyGobject [[default=enabled]]]),
      [WITH_PYGOBJECT=$enableval],
      [WITH_PYGOBJECT=$WITH_PYTHON])

    AC_MSG_CHECKING(for pygobject)

    if test "$PKG_CONFIG" = "" -o "$PKG_CONFIG" = "no" ; then
       AC_MSG_RESULT(no (pkg-config not found))
       WITH_PYGOBJECT=no

    elif test "$GTK_VERSION" = "no" ; then
       AC_MSG_RESULT(no (gtk+ not found))
       WITH_PYGOBJECT=no

    elif test x"$WITH_PYGOBJECT" = x -o x"$WITH_PYGOBJECT" = xno ; then
       AC_MSG_RESULT(no (disabled by user))
       WITH_PYGOBJECT=no

    else
       for version in 3.0 2.0 ; do
           module="pygobject-$version"
           $PKG_CONFIG $module --exists
           if test $? = 0 ; then
               break;
           fi
           module=""
       done

       if test "$module" == "" ; then
          AC_MSG_RESULT(no)
          WITH_PYGOBJECT=no
       else
          PYGOBJECT_INCLUDE=`$PKG_CONFIG $module --cflags`
          PYGOBJECT_LIB=`$PKG_CONFIG $module --libs`
          AC_MSG_RESULT(yes ($version))
          WITH_PYGOBJECT=yes
          PYGOBJECT_INCLUDE="$PYGOBJECT_INCLUDE -DPYGOBJECT"
       fi
    fi

    AC_SUBST(WITH_PYGOBJECT)
    AC_SUBST(PYGOBJECT_INCLUDE)
    AC_SUBST(PYGOBJECT_LIB)
])

###########################################################################
## Checking for pygtk
##   $1=minimum pygtk version required
## This function checks whether pygtk exists on the system, and has a recent
## enough version. It exports the following variables:
##    @WITH_PYGTK@:    "yes" or "no"
##    @PYGTK_PREFIX@:  installation directory of pygtk
##    @PYGTK_INCLUDE@: cflags to use when compiling a pygtk application
## This function must be called after the variable PKG_CONFIG has been set,
## ie probably after gtk+ itself has been detected. Python must also have been
## detected first.
###########################################################################


AC_DEFUN(AM_PATH_PYGTK,
[
    AC_ARG_ENABLE(pygtk,
      AC_HELP_STRING(
        [--disable-pygtk],
        [Disable support for PyGTK [[default=enabled]]]),
      [WITH_PYGTK=$enableval],
      [WITH_PYGTK=$WITH_PYTHON])

    if test "$PKG_CONFIG" = "" -o "$PKG_CONFIG" = "no" ; then
       AC_MSG_CHECKING(for pygtk)
       AC_MSG_RESULT(no (pkg-config not found))
       WITH_PYGTK=no

    elif test "$GTK_VERSION" != "2.0" ; then
       AC_MSG_CHECKING(for pygtk)
       AC_MSG_RESULT(no (incompatible gtk+ version))
       WITH_PYGTK=no

    else
       min_pygtk_version=ifelse([$1], ,2.8,$1)
       module=pygtk-2.0
       AC_MSG_CHECKING(for pygtk - version >= $min_pygtk_version)

       if test x"$WITH_PYGTK" = x -o x"$WITH_PYGTK" = xno ; then
          AC_MSG_RESULT(no)
          PYGTK_PREFIX=""
          PYGTK_INCLUDE=""
          WITH_PYGTK=no

       elif test "$PYTHON_BASE" != "no" ; then
          $PKG_CONFIG $module --exists
          if test $? != 0 ; then
             AC_MSG_RESULT(no)
             WITH_PYGTK=no

          else
             pygtk_version=`$PKG_CONFIG $module --modversion`
             $PKG_CONFIG $module --atleast-version=$min_pygtk_version
             if test $? = 0 ; then
                PYGTK_INCLUDE="`$PKG_CONFIG $module --cflags` -DPYGTK"
                PYGTK_PREFIX=`$PKG_CONFIG $module --variable=prefix`
                AC_MSG_RESULT(yes (version $pygtk_version))
                WITH_PYGTK=yes
             else
                AC_MSG_RESULT(no (found $pygtk_version))
                PYGTK_PREFIX=""
                PYGTK_INCLUDE=""
                WITH_PYGTK=no
             fi
          fi

       else
          AC_MSG_RESULT(no since python not found)
          PYGTK_PREFIX=""
          PYGTK_INCLUDE=""
          WITH_PYGTK=no
       fi
    fi

    AC_SUBST(PYGTK_PREFIX)
    AC_SUBST(PYGTK_INCLUDE)
    AC_SUBST(WITH_PYGTK)
])

##########################################################################
## Compute the extension for shared libraries
##########################################################################

AC_DEFUN(AM_SO_SUFFIX,
[
    case $build_os in
      *darwin*) SO_EXT=.dylib ;;
      *cygwin*|*mingw*)  SO_EXT=.dll ;;
      *)        SO_EXT=.so ;;
    esac
    AC_SUBST(SO_EXT)
])

##########################################################################
## Converts a list of space-separated words into a list suitable for
## inclusion in .gpr files
##   $1=the list
##   $2=exported name
##########################################################################

AC_DEFUN(AM_TO_GPR,
[
   value=[$1]

   # Special handling on darwin for gcc 4.5 and 4.7
   case "$build_os" in
      *darwin*)
         value=`echo $value | sed -e "s/-framework \([[^ ]]*\)/-Wl,-framework -Wl,\1/g"`
   esac

   output=$2
   result=""
   for v in $value; do
      if test "$result" != ""; then
         result="$result, "
      fi
      result="$result\"$v\""
   done
   $2=$result
   AC_SUBST($2)

])

##########################################################################
## Check the availability of a project file
## The file is searched on the predefined PATH and (ADA|GPR)_PROJECT_PATH
##   $1=project to test
##   $2=exported name
##########################################################################

AC_DEFUN(AM_PATH_PROJECT,
[
   project=[$1]
   output=$2

   # Create a temporary directory (from "info autoconf")
   : ${TMPDIR=/tmp}
   {
     tmp=`(umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null` \
        && test -n "$tmp" && test -d "$tmp"
   } || {
     tmp=$TMPDIR/foo$$-$RANDOM
     (umask 077 && mkdir -p "$tmp")
   } || exit $?

   mkdir $tmp/lib
   echo "with \"$project\"; project default is for Source_Dirs use (); end default;" > $tmp/default.gpr

   gnat make -P$tmp/default.gpr >/dev/null 2>/dev/null
   if test $? = 0 ; then
     $2=yes
   else
     $2=no
   fi
   AC_SUBST($2)
])

##########################################################################
## Detects GTK and GtkAda
## Input:
##   If CONFIGURE_SWITCH_WITH_GTK is set, it specifies the default value
##     for gtk. Otherwise, configure will choose the most recent version.
## This exports the following variables
##     @PKG_CONFIG@: path to pkg-config, or "no" if not found
##     @GTK_GCC_FLAGS@: cflags to pass to the compiler. It isn't call
##                      GTK_CFLAGS for compatibility reasons with GPS
##     @WITH_GTK@: Either "yes" or "no", depending on whether gtk+ was found
##     @GTK_VERSION@: one of 2.0, 3.0 or "no"
##########################################################################

AC_DEFUN(AM_PATH_GTK,
[
   AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
   if test "$PKG_CONFIG" = "no" ; then
      WITH_GTK=no
      GTK_VERSION=no
   else
      AC_ARG_WITH(gtk,
         AC_HELP_STRING(
       [--with-gtk=version],
       [Specify the version of GTK to support (3.0 or 2.0)])
AC_HELP_STRING(
       [--without-gtk],
       [Disable support for GTK]),
         [WITH_GTK=$withval],
         [
            AC_MSG_CHECKING(for default gtk+ version)
            # Detect the version we should use, from the system
            for WITH_GTK in "$CONFIGURE_SWITCH_WITH_GTK" "3.0" "2.0" "no"; do
                GTK_PREFIX=`$PKG_CONFIG gtk+-${WITH_GTK} --variable=prefix`
                if test "$GTK_PREFIX" != ""; then
                   break
                fi
            done
            AC_MSG_RESULT($WITH_GTK)
         ])

      if test "$WITH_GTK" != "no"; then
          AC_MSG_CHECKING(for gtk+ ${WITH_GTK})
          GTK_PREFIX=`$PKG_CONFIG gtk+-${WITH_GTK} --variable=prefix`
          AC_MSG_RESULT($GTK_PREFIX)
          GTK_GCC_FLAGS=`$PKG_CONFIG gtk+-${WITH_GTK} --cflags`
          GTK_GCC_LIBS=`$PKG_CONFIG gtk+-${WITH_GTK} --libs`
          if test x"$GTK_GCC_FLAGS" != x ; then
             AC_MSG_CHECKING(for gtkada.gpr)
             AM_PATH_PROJECT(gtkada, HAVE_GTKADA)
             AC_MSG_RESULT($HAVE_GTKADA)
             GTK_VERSION=$WITH_GTK
             WITH_GTK=${HAVE_GTKADA}
          else
             GTK_VERSION=no
             WITH_GTK=no
          fi
      fi
   fi

   AC_SUBST(PKG_CONFIG)
   AC_SUBST(GTK_GCC_FLAGS)
   AC_SUBST(GTK_GCC_LIBS)
   AC_SUBST(WITH_GTK)
   AC_SUBST(GTK_VERSION)
])
