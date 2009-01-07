.PHONY: all examples test valgrind clean docs install

include Makefile.conf

ifeq (${BUILDS_SHARED},yes)
all: static relocatable
install: install_relocatable install_static
else
all: static
install: install_static
endif

## Builds explicitly the shared or the static libraries

static:
	${MAKE} LIBRARY_TYPE=static build_library_type
shared relocatable:
	${MAKE} LIBRARY_TYPE=relocatable build_library_type

## Builds either the static or the shared version, based on the
## LIBRARY_TYPE variable

build_library_type:
	${MAKE} -C src -f Makefile.gnatcoll
ifeq (${WITH_PYTHON},yes)
	${MAKE} -C src -f Makefile.python
endif
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk
endif
ifeq (${WITH_POSTGRES},yes)
	${MAKE} -C src -f Makefile.postgres
	${MAKE} -C src -f Makefile.tools
endif
ifeq (${WITH_GMP},yes)
	${MAKE} -C src -f Makefile.gmp
endif

examples:
	${MAKE} -C examples

## Only works after installation, so we should install to a local directory
## first, so as not to break the users's environment, but still test the
## actual installation process.
## However, we do not force a recompilation here for now, so that we can still
## run the tests on the current binaries, even if we are doing some modifs
## that are not yet compilable
test: 
	${MAKE} prefix=${shell pwd}/local_install install >/dev/null
	${MAKE} prefix=${shell pwd}/local_install -C testsuite $@

## GNU standards say we must not recompile in such a case
## Install either the static or the shared lib, based on the value of
## LIBRARY_TYPE
install_library_type:
	${MKDIR} ${bindir}
	${MKDIR} ${libdir}/${TARNAME}/${LIBRARY_TYPE}
	${MKDIR} ${libdir}/gnat/${TARNAME}
	${MKDIR} ${datadir}/examples
	${MKDIR} ${includedir}/${TARNAME}
	${MKDIR} ${datadir}/gps/plug-ins
	${MAKE} -C src -f Makefile.gnatcoll install
ifeq (${WITH_PYTHON},yes)
	${MAKE} -C src -f Makefile.python install
endif
	${MAKE} -C docs install
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk install
endif
ifeq (${WITH_POSTGRES},yes)
	${MAKE} -C src -f Makefile.postgres install
	${MAKE} -C src -f Makefile.tools install
endif
ifeq (${WITH_GMP},yes)
	${MAKE} -C src -f Makefile.gmp install
endif
	${CP} distrib/gnatcoll_gps.xml ${datadir}/gps/plug-ins
	${CP} distrib/*.gpr ${libdir}/gnat

install_static:
	${MAKE} LIBRARY_TYPE=static install_library_type
install_relocatable install_shared:
	${MAKE} LIBRARY_TYPE=relocatable install_library_type

clean:
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.gnatcoll $@
	${MAKE} LIBRARY_TYPE=static      -C src -f Makefile.gnatcoll $@
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.python $@
	${MAKE} LIBRARY_TYPE=static      -C src -f Makefile.python $@
ifeq (${WITH_GTK},yes)
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.gtk $@
	${MAKE} LIBRARY_TYPE=static     -C src -f Makefile.gtk $@
endif
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.postgres $@
	${MAKE} LIBRARY_TYPE=static     -C src -f Makefile.postgres $@
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.gmp $@
	${MAKE} LIBRARY_TYPE=static     -C src -f Makefile.gmp $@
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.tools $@
	${MAKE} LIBRARY_TYPE=static     -C src -f Makefile.tools $@
	${MAKE} -C testsuite $@
	${MAKE} -C docs $@
	${MAKE} -C examples $@

docs:
	${MAKE} -C docs
