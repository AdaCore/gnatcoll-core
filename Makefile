##############################################################################
##                                                                          ##
##                              GNATCOLL LIBRARY                            ##
##                                                                          ##
##                         Copyright (C) 2017, AdaCore.                     ##
##                                                                          ##
## This library is free software;  you can redistribute it and/or modify it ##
## under terms of the  GNU General Public License  as published by the Free ##
## Software  Foundation;  either version 3,  or (at your  option) any later ##
## version. This library is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN# ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            ##
##                                                                          ##
## As a special exception under Section 7 of GPL version 3, you are granted ##
## additional permissions described in the GCC Runtime Library Exception,   ##
## version 3.1, as published by the Free Software Foundation.               ##
##                                                                          ##
## You should have received a copy of the GNU General Public License and    ##
## a copy of the GCC Runtime Library Exception along with this program;     ##
## see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    ##
## <http://www.gnu.org/licenses/>.                                          ##
##                                                                          ##
##############################################################################

# Makefile targets
# ----------------
#
# Setup:                   make [VAR=VALUE] setup (see below)
# Build:                   make
# Install:                 make install

# Variables which can be set:
#
# General:
#
#   prefix        : root install directory
#   ENABLE_SHARED : yes / no (or empty)
#   BUILD         : DEBUG PROD
#   PROCESSORS    : nb parallel compilations (0 to use all cores)
#   TARGET        : target triplet for cross-compilation
#   INTEGRATED    : installs the project as part of the compiler installation;
#                   this adds NORMALIZED_TARGET subdir to prefix
#
# Project specific:
#
#   GNATCOLL_MMAP    : whether MMAP is supported (yes/no)
#                      default is "yes"; has no effect on Windows
#   GNATCOLL_MADVISE : whether MADVISE is supported (yes/no)
#                      default is "yes"; has no effect on Windows

# helper programs
CAT := cat
ECHO  := echo
WHICH := which

# check for out-of-tree build
SOURCE_DIR := $(dir $(MAKEFILE_LIST))

# make -f with absolute path to current directory Makefile is in-tree build
ifeq ($(SOURCE_DIR), $(shell pwd)/)
	SOURCE_DIR := ./
endif

ifeq ($(SOURCE_DIR),./)
  RBD=
  GNATCOLL_GPR=gnatcoll.gpr
  MAKEPREFIX=
else
  RBD=--relocate-build-tree
  GNATCOLL_GPR=$(SOURCE_DIR)/gnatcoll.gpr
  MAKEPREFIX=$(SOURCE_DIR)/
endif

TARGET := $(shell gcc -dumpmachine)
NORMALIZED_TARGET := $(subst normalized_target:,,$(wordlist 6,6,$(shell gprconfig  --config=ada --target=$(TARGET) --mi-show-compilers)))
ifeq ($(NORMALIZED_TARGET),)
  $(error No toolchain found for target "$(TARGET)")
endif

GNATCOLL_OS := $(if $(findstring darwin,$(NORMALIZED_TARGET)),osx,$(if $(findstring windows,$(NORMALIZED_TARGET)),windows,unix))

prefix := $(dir $(shell $(WHICH) gnatls))..
GNATCOLL_VERSION := $(shell $(CAT) $(SOURCE_DIR)/version_information)
GNATCOLL_MMAP := yes
GNATCOLL_MADVISE := yes

BUILD         = PROD
PROCESSORS    = 0
BUILD_DIR     =
ENABLE_SHARED = yes
INTEGRATED    = no
GNATCOV       =

all: build

# Load current setup if any
-include makefile.setup

GTARGET=--target=$(NORMALIZED_TARGET)

ifeq ($(ENABLE_SHARED), yes)
   LIBRARY_TYPES=static relocatable static-pic
else
   LIBRARY_TYPES=static
endif

ifeq ($(GNATCOV), yes)
   LIBRARY_TYPES=static
   GNATCOV_RTS=gnatcovrts
   GNATCOV_BUILD_OPTS=--src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts
   GNATCOV_PROJECT_PATH=GPR_PROJECT_PATH=$(CURDIR)/gnatcov_rts/share/gpr:$(GPR_PROJECT_PATH)
else
   GNATCOV_RTS=
   GNATCOV_BUILD_OPTS=
   GNATCOV_PROJECT_PATH=
endif

ifeq ($(INTEGRATED), yes)
   integrated_install=/$(NORMALIZED_TARGET)
endif



GPR_VARS=-XGNATCOLL_MMAP=$(GNATCOLL_MMAP) \
	 -XGNATCOLL_MADVISE=$(GNATCOLL_MADVISE) \
	 -XGNATCOLL_VERSION=$(GNATCOLL_VERSION) \
	 -XGNATCOLL_OS=$(GNATCOLL_OS) \
	 -XBUILD=$(BUILD)

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

BUILDER=$(GNATCOV_PROJECT_PATH) gprbuild -p -m $(GTARGET) $(RBD) \
        -j$(PROCESSORS) $(GPR_VARS) \
	$(GPRBUILD_OPTIONS) $(GNATCOV_BUILD_OPTS)
INSTALLER=gprinstall -p -f $(GTARGET) $(GPR_VARS) \
	$(RBD) --sources-subdir=include/gnatcoll --prefix=$(prefix)$(integrated_install)
CLEANER=gprclean -q $(RBD) $(GTARGET)
UNINSTALLER=$(INSTALLER) -p -f --install-name=gnatcoll --uninstall

#########
# build #
#########

build: $(LIBRARY_TYPES:%=build-%)

build-%: $(GNATCOV_RTS)
ifeq ($(GNATCOV), yes)
	gnatcov instrument -P $(GNATCOLL_GPR) $(RBD) \
		--no-subprojects --level=stmt+decision
endif
	$(BUILDER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -XGPR_BUILD=$* \
		$(GPR_VARS) $(GNATCOLL_GPR) -v

###################
# Instrumentation #
###################

gnatcovrts:
	gnatcov setup --prefix=gnatcov_rts

###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)$(integrated_install)/share/gpr/manifests/gnatcoll))
	$(UNINSTALLER) $(GNATCOLL_GPR)
endif

install: uninstall $(LIBRARY_TYPES:%=install-%)

install-%:
	$(INSTALLER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -XGPR_BUILD=$* \
		--build-name=$* $(GPR_VARS) \
		--build-var=LIBRARY_TYPE --build-var=GNATCOLL_BUILD \
		--build-var=GNATCOLL_CORE_BUILD $(GNATCOLL_GPR)

###########
# Cleanup #
###########

clean: $(LIBRARY_TYPES:%=clean-%)

clean-%:
	-$(CLEANER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -XGPR_BUILD=$* \
		$(GPR_VARS) $(GNATCOLL_GPR)

#########
# setup #
#########

.SILENT: setup

setup:
	$(ECHO) "prefix=$(prefix)" > makefile.setup
	$(ECHO) "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	$(ECHO) "INTEGRATED=$(INTEGRATED)" >> makefile.setup
	$(ECHO) "BUILD=$(BUILD)" >> makefile.setup
	$(ECHO) "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	$(ECHO) "TARGET=$(TARGET)" >> makefile.setup
	$(ECHO) "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup
	$(ECHO) "GNATCOLL_OS=$(GNATCOLL_OS)" >> makefile.setup
	$(ECHO) "GNATCOLL_VERSION=$(GNATCOLL_VERSION)" >> makefile.setup
	$(ECHO) "GNATCOLL_MMAP=$(GNATCOLL_MMAP)" >> makefile.setup
	$(ECHO) "GNATCOLL_MADVISE=$(GNATCOLL_MADVISE)" >> makefile.setup

# Let gprbuild handle parallelisation. In general, we don't support parallel
# runs in this Makefile, as concurrent gprinstall processes may crash.
.NOTPARALLEL:
