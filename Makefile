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
	gprbuild -Pgnatcoll_build -p
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk buildall
endif
ifneq ($(subst no,,${WITH_SQLITE}${WITH_POSTGRES}),)
	${MAKE} -C src -f Makefile.tools buildall
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
	${MAKE} -C src -f Makefile.gnatcoll libinstall
ifeq (${WITH_PYTHON},yes)
	${MAKE} -C src -f Makefile.python libinstall
endif
	${MAKE} -C docs install
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk libinstall
endif
	${MAKE} -C src -f Makefile.postgres libinstall
	${MAKE} -C src -f Makefile.sqlite libinstall
ifneq ($(subst no,,${WITH_SQLITE}${WITH_POSTGRES}),)
	${MAKE} -C src -f Makefile.tools installbin
endif
ifeq (${WITH_GMP},yes)
	${MAKE} -C src -f Makefile.gmp libinstall
endif
	${CP} distrib/gnatcoll_gps.xml ${datadir}/gps/plug-ins
	${CP} distrib/*.gpr ${libdir}/gnat

install_static:
	${MAKE} LIBRARY_TYPE=static install_library_type
install_relocatable install_shared:
	${MAKE} LIBRARY_TYPE=relocatable install_library_type

clean:
	-gprclean -r -q -Pgnatcoll_build -XLIBRARY_TYPE=relocatable
	-gprclean -r -q -Pgnatcoll_build -XLIBRARY_TYPE=static
ifeq (${WITH_GTK},yes)
	-gprclean -r -q -Psrc/gnatcoll_gtk -XLIBRARY_TYPE=relocatable
	-gprclean -r -q -Psrc/gnatcoll_gtk -XLIBRARY_TYPE=static
endif
	-${MAKE} -C testsuite $@
	-${MAKE} -C docs $@
	-${MAKE} -C examples $@

docs:
	${MAKE} -C docs
