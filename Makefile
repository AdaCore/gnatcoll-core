.PHONY: all examples test valgrind clean docs install

include Makefile.conf

ifeq (${BUILDS_SHARED},yes)
all: generate_sources static relocatable tools_relocatable
install: install_relocatable install_static install_docs
else
all: generate_sources static tools_static
install: install_static install_docs
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

tools_static:
	${MAKE} LIBRARY_TYPE=static -C src -f Makefile.tools buildall
tools_relocatable:
	${MAKE} LIBRARY_TYPE=relocatable -C src -f Makefile.tools buildall

# Regenerate part of the sources. Unfortunately, this can be run only after
# we have build GNATCOLL, and then its tools, even though GNATCOLL itself
# relies on those generated sources.
# So this target simply does nothing if gnatcoll_db2ada is not found, in which
# case we use the checked in sources (which means that changing the dbschema
# requires to have already build GNATCOLL once before)

generate_sources:
	-src/obj/gnatcoll_db2ada${EXE} -dbtype=sqlite -dbname=:memory: \
		-output src/generated \
		-dbmodel=src/dbschema.txt \
		-createdb \
		-adacreate \
		-api GNATCOLL.Xref.Database \
		-load=src/initialdata.txt \
		-enum "f2f_kind,id,name,F2F_,Integer" \
		-enum "e2e_kind,id,name,E2E_,Integer"

examples:
	${MAKE} -C examples

## Create links for the gnat sources

ifeq ($(GNAT_SOURCES),copy)
do_links:
ifeq ($(OS),Windows_NT)
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	   $(CP) gnat_src/$(f) gnat > /dev/null 2>&1 ;)
else
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	   $(LN_S) ../gnat_src/$(f) gnat >/dev/null 2>&1 ;)
endif
	@(cd gnat && gnatmake -q xsnamest && ./xsnamest && mv snames.ns snames.ads && mv snames.nb snames.adb)

else
do_links:
endif

## Only works after installation, so we should install to a local directory
## first, so as not to break the users's environment, but still test the
## actual installation process.
## However, we do not force a recompilation here for now, so that we can still
## run the tests on the current binaries, even if we are doing some modifs
## that are not yet compilable
test_names=
test: 
	@${MAKE} prefix=${shell pwd}/local_install install >/dev/null
	@${MAKE} prefix=${shell pwd}/local_install test_names="${test_names}" -C testsuite

test_verbose:
	@${MAKE} prefix=${shell pwd}/local_install install >/dev/null
	@${MAKE} prefix=${shell pwd}/local_install test_names="${test_names}" -C testsuite verbose

## GNU standards say we must not recompile in such a case
## Install either the static or the shared lib, based on the value of
## LIBRARY_TYPE
install_library_type:
	${MKDIR} ${bindir}
	${MKDIR} ${libdir}/${TARNAME}/${LIBRARY_TYPE}
	${MKDIR} ${DESTDIR}${prefix}/lib/gnat/${TARNAME}
	${MKDIR} ${datadir}/examples
	${MKDIR} ${datadir}/gnatcoll
	${MKDIR} ${includedir}/${TARNAME}
	${MKDIR} ${datadir}/gps/plug-ins
	${MAKE} -C src -f Makefile.gnatcoll libinstall
	${MAKE} -C src -f Makefile.python libinstall
ifeq (${WITH_GTK},yes)
	${MAKE} -C src -f Makefile.gtk libinstall
endif
	${MAKE} -C src -f Makefile.postgres libinstall
	${MAKE} -C src -f Makefile.sqlite libinstall
	${MAKE} -C src -f Makefile.readline libinstall
	${MAKE} -C src -f Makefile.tools installbin
ifeq (${WITH_GMP},yes)
	${MAKE} -C src -f Makefile.gmp libinstall
endif
	${CP} src/dborm.py ${datadir}/gnatcoll/
	${CP} distrib/gnatcoll_gps.xml ${datadir}/gps/plug-ins
	${CP} distrib/gnatcoll_runtime.xml ${datadir}/gps/plug-ins
	${CP} distrib/*.gpr ${DESTDIR}${prefix}/lib/gnat

install_docs:
	${MKDIR} ${datadir}/doc/gnatcoll/html
	-${CP} -r docs/_build/html/* ${datadir}/doc/gnatcoll/html 2>/dev/null
	-${CP} docs/_build/latex/GNATColl.pdf ${datadir}/doc/gnatcoll/gnatcoll.pdf 2>/dev/null

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
	-${MAKE} -C src -f Makefile.tools clean
	-${MAKE} -C testsuite $@
	-${MAKE} -C docs $@
	-${MAKE} -C examples $@

docs:
	${MAKE} -C docs html latexpdf
