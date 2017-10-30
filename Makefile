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
#
# Project specific:
#
#   GNATCOLL_MMAP    : whether MMAP is supported (yes/no)
#                      default is "yes"; has no effect on Windows
#   GNATCOLL_MADVISE : whether MADVISE is supported (yes/no)
#                      default is "yes"; has no effect on Windows
#   GNATCOLL_ATOMICS : atomic model (intrinsic/mutex)
#                      default is "intrinsic"

# helper programs
CAT := cat
ECHO  := echo
WHICH := which

# check for out-of-tree build
SOURCE_DIR := $(dir $(MAKEFILE_LIST))
ifeq ($(SOURCE_DIR),./)
  RBD=
  GNATCOLL_GPR=gnatcoll.gpr
  MAKEPREFIX=
else
  RBD=--relocate-build-tree
  GNATCOLL_GPR=$(SOURCE_DIR)/gnatcoll.gpr
  MAKEPREFIX=$(SOURCE_DIR)/
endif

HOST   := $(shell gcc -dumpmachine)
TARGET := $(shell gcc -dumpmachine)

GNATCOLL_OS := $(if $(findstring darwin,$(TARGET)),osx,unix)

ifneq (,$(filter $(findstring mingw,$(TARGET))$(findstring windows,$(TARGET)),mingw windows))
  GNATCOLL_MMAP := yes
  GNATCOLL_MADVISE := no
  GNATCOLL_OS := windows
else
  GNATCOLL_MMAP := yes
  GNATCOLL_MADVISE := yes
endif

prefix := $(dir $(shell $(WHICH) gnatls))..
GNATCOLL_VERSION := $(shell $(CAT) $(SOURCE_DIR)/version_information)
GNATCOLL_ATOMICS := intrinsic

BUILD         = PROD
PROCESSORS    = 0
BUILD_DIR     =
ENABLE_SHARED := "yes"

all: build

# Load current setup if any
-include makefile.setup

# target options for cross-build
ifeq ($(HOST),$(TARGET))
  GTARGET=
else
  GTARGET=--target=$(TARGET)
endif

ifeq ($(ENABLE_SHARED), yes)
   LIBRARY_TYPES=static relocatable static-pic
else
   LIBRARY_TYPES=static
endif

GPR_VARS=-XGNATCOLL_MMAP=$(GNATCOLL_MMAP) \
	 -XGNATCOLL_MADVISE=$(GNATCOLL_MADVISE) \
	 -XGNATCOLL_ATOMICS=$(GNATCOLL_ATOMICS) \
	 -XGNATCOLL_VERSION=$(GNATCOLL_VERSION) \
	 -XGNATCOLL_OS=$(GNATCOLL_OS) \
	 -XBUILD=$(BUILD)

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

BUILDER=gprbuild -p -m $(GTARGET) $(RBD) -j$(PROCESSORS) $(GPR_VARS) \
	$(GPRBUILD_OPTIONS)
INSTALLER=gprinstall -p -f --target=$(TARGET) $(GPR_VARS) \
	$(RBD) --sources-subdir=include/$(NAME) --prefix=$(prefix)
CLEANER=gprclean -q $(RBD)
UNINSTALLER=$(INSTALLER) -p -f --install-name=gnatcoll --uninstall

#########
# build #
#########

build: $(LIBRARY_TYPES:%=build-%)

build-%:
	$(BUILDER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* $(GPR_VARS) $(GNATCOLL_GPR)

###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/gnatcoll))
	$(UNINSTALLER) $(GNATCOLL_GPR)
endif

install: uninstall $(LIBRARY_TYPES:%=install-%)

install-%:
	$(INSTALLER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		--build-name=$* --build-var=LIBRARY_TYPE $(GPR_VARS) $(GNATCOLL_GPR)

###########
# Cleanup #
###########

clean: $(LIBRARY_TYPES:%=clean-%)

clean-%:
	-$(CLEANER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* $(GPR_VARS) $(GNATCOLL_GPR)

#########
# setup #
#########

.SILENT: setup

setup:
	$(ECHO) "prefix=$(prefix)" > makefile.setup
	$(ECHO) "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	$(ECHO) "BUILD=$(BUILD)" >> makefile.setup
	$(ECHO) "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	$(ECHO) "TARGET=$(TARGET)" >> makefile.setup
	$(ECHO) "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup
	$(ECHO) "GNATCOLL_OS=$(GNATCOLL_OS)" >> makefile.setup
	$(ECHO) "GNATCOLL_VERSION=$(GNATCOLL_VERSION)" >> makefile.setup
	$(ECHO) "GNATCOLL_MMAP=$(GNATCOLL_MMAP)" >> makefile.setup
	$(ECHO) "GNATCOLL_MADVISE=$(GNATCOLL_MADVISE)" >> makefile.setup
	$(ECHO) "GNATCOLL_ATOMICS=$(GNATCOLL_ATOMICS)" >> makefile.setup

