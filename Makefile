.PHONY: all examples test valgrind clean docs install

include Makefile.conf

all:
	${MAKE} -C src -f Makefile.gnatlib $@
	${MAKE} -C src -f Makefile.python $@
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk $@
endif

static:
	${MAKE} GNATLIB_LIBRARY_TYPE=static
shared relocatable: all
	${MAKE} GNATLIB_LIBRARY_TYPE=relocatable

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
install:
	${MKDIR} ${bindir}
	${MKDIR} ${libdir}/${TARNAME}/${GNATLIB_LIBRARY_TYPE}
	${MKDIR} ${libdir}/gnat/${TARNAME}
	${MKDIR} ${datadir}/examples
	${MKDIR} ${includedir}/${TARNAME}
	${MKDIR} ${datadir}/gps/plug-ins
	${MAKE} -C src -f Makefile.gnatlib $@
	${MAKE} -C src -f Makefile.python $@
	${MAKE} -C docs $@
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk $@
endif
	${INSTALL} distrib/gnatlib_gps.xml ${datadir}/gps/plug-ins
	${INSTALL} distrib/*.gpr ${libdir}/gnat
	${INSTALL} distrib/${GNATLIB_LIBRARY_TYPE}/*.gpr ${libdir}/gnat

install_static:
	${MAKE} GNATLIB_LIBRARY_TYPE=static install
install_relocatable:
	${MAKE} GNATLIB_LIBRARY_TYPE=relocatable install

clean:
	${MAKE} GNATLIB_LIBRARY_TYPE=relocatable -C src -f Makefile.gnatlib $@
	${MAKE} GNATLIB_LIBRARY_TYPE=static      -C src -f Makefile.gnatlib $@
	${MAKE} GNATLIB_LIBRARY_TYPE=relocatable -C src -f Makefile.python $@
	${MAKE} GNATLIB_LIBRARY_TYPE=static      -C src -f Makefile.python $@
ifeq (${WITH_GTK},yes)
	${MAKE} GNATLIB_LIBRARY_TYPE=relocatable -C src -f Makefile.gtk $@
	${MAKE} GNATLIB_LIBRARY_TYPE=static     -C src -f Makefile.gtk $@
endif
	${MAKE} -C testsuite $@
	${MAKE} -C docs $@
	${MAKE} -C examples $@

docs:
	${MAKE} -C docs
