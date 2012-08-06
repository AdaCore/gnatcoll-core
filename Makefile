.PHONY: all examples test clean docs install generate_sources do_links static relocatable shared

## Put this first so that it is the default make target
all: static

include Makefile.conf

ifeq (${BUILDS_SHARED},yes)
# Additional targets
all: relocatable
endif

include Makefile.gnat

## Builds explicitly the shared or the static libraries

static: build_library_type/static
shared relocatable: build_library_type/relocatable

# Build either type of library. The argument (%) is the type of library to build

build_library_type/%: generate_sources do_links
	@echo "====== Building $(@F) libraries ======"
	${GPRBUILD} -m -j${PROCESSORS} -XLIBRARY_TYPE=$(@F) -Pgnatcoll_build -p

	@# Need to build libgnatcoll_gtk separately, because its project files
	@# requires gtkada.gpr, which might not exist on the machine.
ifeq (${WITH_GTK},yes)
	${GPRBUILD} -m -j${PROCESSORS} -XLIBRARY_TYPE=$(@F) -Psrc/gnatcoll_gtk -p
endif

	@# Build the tools (the list is the project's Main attribute)
	@# They are not build as part of the above because only the Main from
	@# gnatcoll_build.gpr are build. We could use aggregate projects to speed
	@# things up.
	${GPRBUILD} -q -m -j${PROCESSORS} -XLIBRARY_TYPE=$(@F) -Psrc/gnatcoll_tools

# Regenerate part of the sources. Unfortunately, this can be run only after
# we have build GNATCOLL, and then its tools, even though GNATCOLL itself
# relies on those generated sources.
# So this target simply does nothing if gnatcoll_db2ada is not found, in which
# case we use the checked in sources (which means that changing the dbschema
# requires to have already build GNATCOLL once before)

generate_sources:
	-@if [ -f src/obj/gnatcoll_db2ada${EXE} ]; then \
	   src/obj/gnatcoll_db2ada${EXE} -dbtype=sqlite -dbname=:memory: \
		-output src/generated \
		-dbmodel=src/dbschema.txt \
		-createdb \
		-adacreate \
		-api GNATCOLL.Xref.Database \
		-load=src/initialdata.txt \
		-enum "f2f_kind,id,name,F2F_,Integer" \
		-enum "e2e_kind,id,name,E2E_,Integer"; \
	fi

examples:
	${MAKE} -C examples all

## Create links for the gnat sources

do_links:
ifeq ($(GNAT_SOURCES),copy)
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	   $(LN_S) -f ../gnat_src/$(f) gnat >/dev/null 2>&1 ;)
	@(cd gnat && gnatmake -q xsnamest && ./xsnamest && mv snames.ns snames.ads && mv snames.nb snames.adb)
endif

## Only works after installation, so we should install to a local directory
## first, so as not to break the users's environment, but still test the
## actual installation process.
## However, we do not force a recompilation here for now, so that we can still
## run the tests on the current binaries, even if we are doing some modifs
## that are not yet compilable
test_names=

local_install: force
	@${MAKE} prefix=${shell pwd}/local_install install >/dev/null

test: local_install
	@${MAKE} prefix=${shell pwd}/local_install test_names="${test_names}" -C testsuite

test_verbose: local_install
	@${MAKE} prefix=${shell pwd}/local_install test_names="${test_names}" -C testsuite verbose

# Installs both static and shared libraries (if they were build)
# GNU standards say we must not recompile, for this target
install:
	${MKDIR} ${bindir}
	${MKDIR} ${DESTDIR}${prefix}/lib/gnat/${TARNAME}
	${MKDIR} ${datadir}/${TARNAME}
	${MKDIR} ${includedir}/${TARNAME}
	${CP} src/dborm.py ${datadir}/${TARNAME}/
	${CP} distrib/*.gpr ${DESTDIR}${prefix}/lib/gnat

	${MKDIR} ${datadir}/examples/${TARNAME}
	@# Do not want to copy .svn directories
	${CP} -rf examples/* ${datadir}/examples/${TARNAME}

	${MKDIR} ${datadir}/gps/plug-ins
	${CP} distrib/gnatcoll_gps.xml ${datadir}/gps/plug-ins
	${CP} distrib/gnatcoll_runtime.xml ${datadir}/gps/plug-ins

	${MKDIR} ${datadir}/doc/${TARNAME}/html
	-${CP} -r docs/_build/html/* ${datadir}/doc/${TARNAME}/html 2>/dev/null
	-${CP} docs/_build/latex/GNATColl.pdf ${datadir}/doc/${TARNAME}/gnatcoll.pdf 2>/dev/null

	${MKDIR} ${libdir}/${TARNAME}/static
ifeq (${BUILDS_SHARED},yes)
	${MKDIR} ${libdir}/${TARNAME}/relocatable
endif

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

## Clean either type of library, based on the value of (%)

clean_library/%:
	-gprclean -r -q -Pgnatcoll_build -XLIBRARY_TYPE=$(@F)
	@# Separate pass to also remove the Main
	-gprclean -r -q -Psrc/gnatcoll_tools -XLIBRARY_TYPE=$(@F)
ifeq (${WITH_GTK},yes)
	-gprclean -r -q -Psrc/gnatcoll_gtk -XLIBRARY_TYPE=$(@F)
endif

clean: clean_library/static clean_library/relocatable
	-${MAKE} -C testsuite $@
	-${MAKE} -C docs $@
	-${MAKE} -C examples $@

docs:
	${MAKE} -C docs html latexpdf

force:
