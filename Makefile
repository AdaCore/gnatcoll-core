.PHONY: all examples test valgrind clean docs install

include Makefile.conf

all:
	${MAKE} -C src -f Makefile.gnatcoll $@
	${MAKE} -C src -f Makefile.python $@
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk $@
endif
ifeq (${WITH_POSTGRES},yes)
	${MAKE} -C src -f Makefile.postgres $@
endif

static:
	${MAKE} GNATCOLL_LIBRARY_TYPE=static
shared relocatable: all
	${MAKE} GNATCOLL_LIBRARY_TYPE=relocatable

both: static shared install_static install_shared

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
	${MKDIR} ${libdir}/${TARNAME}/${GNATCOLL_LIBRARY_TYPE}
	${MKDIR} ${libdir}/gnat/${TARNAME}
	${MKDIR} ${datadir}/examples
	${MKDIR} ${includedir}/${TARNAME}
	${MKDIR} ${datadir}/gps/plug-ins
	${MAKE} -C src -f Makefile.gnatcoll $@
	${MAKE} -C src -f Makefile.python $@
	${MAKE} -C docs $@
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk $@
endif
ifeq (${WITH_POSTGRES},yes)
	${MAKE} -C src -f Makefile.postgres $@
endif
	${INSTALL} distrib/gnatcoll_gps.xml ${datadir}/gps/plug-ins
	${INSTALL} distrib/*.gpr ${libdir}/gnat
	${INSTALL} distrib/${GNATCOLL_LIBRARY_TYPE}/*.gpr ${libdir}/gnat

install_static:
	${MAKE} GNATCOLL_LIBRARY_TYPE=static install
install_relocatable install_shared:
	${MAKE} GNATCOLL_LIBRARY_TYPE=relocatable install

clean:
	${MAKE} GNATCOLL_LIBRARY_TYPE=relocatable -C src -f Makefile.gnatcoll $@
	${MAKE} GNATCOLL_LIBRARY_TYPE=static      -C src -f Makefile.gnatcoll $@
	${MAKE} GNATCOLL_LIBRARY_TYPE=relocatable -C src -f Makefile.python $@
	${MAKE} GNATCOLL_LIBRARY_TYPE=static      -C src -f Makefile.python $@
ifeq (${WITH_GTK},yes)
	${MAKE} GNATCOLL_LIBRARY_TYPE=relocatable -C src -f Makefile.gtk $@
	${MAKE} GNATCOLL_LIBRARY_TYPE=static     -C src -f Makefile.gtk $@
endif
ifeq (${WITH_POSTGRES},yes)
	${MAKE} GNATCOLL_LIBRARY_TYPE=relocatable -C src -f Makefile.postgres $@
	${MAKE} GNATCOLL_LIBRARY_TYPE=static     -C src -f Makefile.postgres $@
endif
	${MAKE} -C testsuite $@
	${MAKE} -C docs $@
	${MAKE} -C examples $@

docs:
	${MAKE} -C docs
