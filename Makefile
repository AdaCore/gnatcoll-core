.PHONY: all examples test valgrind clean docs install

include Makefile.conf

ifeq (${BUILDS_SHARED},yes)
all: static relocatable
install: install_relocatable install_static
else
all: static
install: install_static
endif

include Makefile.gnat

## Builds explicitly the shared or the static libraries

static: do_links
	${MAKE} LIBRARY_TYPE=static build_library_type
shared relocatable: do_links
	${MAKE} LIBRARY_TYPE=relocatable build_library_type

## Builds either the static or the shared version, based on the
## LIBRARY_TYPE variable

build_library_type:
	${GPRBUILD} -m -j${PROCESSORS} -XLIBRARY_TYPE=${LIBRARY_TYPE} -Pgnatcoll_build -p
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk buildall
endif
	${MAKE} -C src -f Makefile.tools buildall

examples:
	${MAKE} -C examples

## Create links for the gnat sources

ifeq (${WITH_PROJECTS},yes)
do_links:
ifeq ($(OS),Windows_NT)
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	   $(CP) gnat_src/$(f) gnat > /dev/null 2>&1 ;)
else
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	   $(LN_S) ../gnat_src/$(f) gnat > /dev/null 2>&1 ;)
endif
ifeq ($(GNAT_SOURCES),copy)
	@(cd gnat && gnatmake -q xsnamest && ./xsnamest && mv snames.ns snames.ads && mv snames.nb snames.adb)
endif

else
do_links:
endif

## Only works after installation, so we should install to a local directory
## first, so as not to break the users's environment, but still test the
## actual installation process.
## However, we do not force a recompilation here for now, so that we can still
## run the tests on the current binaries, even if we are doing some modifs
## that are not yet compilable
## In the context of GPS, we only build the static version of GNATCOLL, even
## though BUILDS_SHARED is True. so here we need to only install static
## libraries (and will do the testing with those)
test: 
	@${MAKE} prefix=${shell pwd}/local_install install_static >/dev/null
	@${MAKE} prefix=${shell pwd}/local_install LIBRARY_TYPE=static -C testsuite

## GNU standards say we must not recompile in such a case
## Install either the static or the shared lib, based on the value of
## LIBRARY_TYPE
install_library_type:
	${MKDIR} ${bindir}
	${MKDIR} ${libdir}/${TARNAME}/${LIBRARY_TYPE}
	${MKDIR} ${libdir}/gnat/${TARNAME}
	${MKDIR} ${datadir}/examples
	${MKDIR} ${datadir}/gnatcoll
	${MKDIR} ${includedir}/${TARNAME}
	${MKDIR} ${datadir}/gps/plug-ins
	${MAKE} -C src -f Makefile.gnatcoll libinstall
	${MAKE} -C src -f Makefile.python libinstall
	${MAKE} -C docs install
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk libinstall
endif
	${MAKE} -C src -f Makefile.postgres libinstall
	${MAKE} -C src -f Makefile.sqlite libinstall
	${MAKE} -C src -f Makefile.tools installbin
ifeq (${WITH_GMP},yes)
	${MAKE} -C src -f Makefile.gmp libinstall
endif
	${CP} src/dborm.py ${datadir}/gnatcoll/
	${CP} distrib/gnatcoll_gps.xml ${datadir}/gps/plug-ins
	${CP} distrib/gnatcoll_runtime.xml ${datadir}/gps/plug-ins
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
