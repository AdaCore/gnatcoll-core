.PHONY: all examples test clean docs install generate_sources do_links static relocatable shared

## Put this first so that it is the default make target
all:

include Makefile.conf

ifeq (${BUILDS_SHARED},yes)
# Additional targets. Builds relocatble first so that the tools are
# preferably linked statically.
all: relocatable static
install:  install-clean install_library_type/static \
		install_library_type/relocatable
else
all: static
install:  install-clean install_library_type/static
endif

include Makefile.gnat

## Builds explicitly the shared or the static libraries

static: build_library_type/static
shared relocatable: build_library_type/relocatable

# Build either type of library. The argument (%) is the type of library to build

build_library_type/%: generate_sources do_links
	@${RM} src/gnatcoll-atomic.adb

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

#######################################################################
#  install

GPRINST_OPTS=-p -f --prefix=${prefix} --install-name=gnatcoll \
	--exec-subdir=${bindir} --project-subdir=lib/gnat \
	--build-var=LIBRARY_TYPE --build-name=$(@F) -XLIBRARY_TYPE=$(@F)

install-clean:
ifneq (,$(wildcard $(prefix)/lib/gnat/manifests/gnatcoll))
	-$(GPRINSTALL) --uninstall -f \
		--prefix=$(prefix) --project-subdir=lib/gnat gnatcoll
endif


install_library_type/%:
	@echo "====== Installing $(@F) libraries ======"
	${GPRINSTALL} -r ${GPRINST_OPTS} -Pgnatcoll_main
ifeq (${WITH_GTK},yes)
	${GPRINSTALL} ${GPRINST_OPTS} -Psrc/gnatcoll_gtk
endif
	${GPRINSTALL} --mode=usage ${GPRINST_OPTS} -Psrc/gnatcoll_tools

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
	   for f in src/generated/*.ad?; do \
	      tr -d '\r' < $$f > $$f.tmp && mv $$f.tmp $$f; \
	   done; \
	fi

examples:
	${MAKE} -C examples all

SQLITE_DIR=src/sqlite/amalgamation
sqlite3_shell: $(SQLITE_DIR)/sqlite3_for_gps
$(SQLITE_DIR)/sqlite3_for_gps: $(SQLITE_DIR)/shell.c $(SQLITE_DIR)/sqlite3.c
ifeq ($(OS),Windows_NT)
	-cd $(SQLITE_DIR); gcc -O2 -DSQLITE_OMIT_LOAD_EXTENSION -D__EXTENSIONS__ -o sqlite3_for_gps shell.c sqlite3.c
else
	# If we fail to compile, never mind. Some tests will simply be disabled
	-cd $(SQLITE_DIR); gcc -O2 -DSQLITE_OMIT_LOAD_EXTENSION -D__EXTENSIONS__ -o sqlite3_for_gps shell.c sqlite3.c -lpthread -ldl
endif

## Create links for the gnat sources

do_links:
ifeq ($(GNAT_SOURCES),copy)
ifeq ($(OS),Windows_NT)
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	    $(CP) -f gnat_src/$(f) gnat >/dev/null 2>&1 ;)
else
	-@$(foreach f,$(GNAT_SOURCES_FOR_GNATCOLL), \
	   $(LN_S) -f ../gnat_src/$(f) gnat >/dev/null 2>&1 ;)
endif
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

test: sqlite3_shell local_install
	@${MAKE} test_names="${test_names}" -C testsuite

test_verbose: local_install
	@${MAKE} test_names="${test_names}" -C testsuite verbose


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
