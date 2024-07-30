##############################################################################
##                                                                          ##
##                              GNATCOLL LIBRARY                            ##
##                                                                          ##
##                      Copyright (C) 2017-2024 AdaCore.                    ##
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
#   PYTHON        : force Python executable to run Python scripts
# Project specific:
#
#   GNATCOLL_PROJECTS : whether GNATCOLL projects package is included (yes/no)
#                       default is "yes";

# helper programs
WHICH := which
SED := sed
PYTHON :=

# check for out-of-tree build
SOURCE_DIR := $(dir $(MAKEFILE_LIST))

# make -f with absolute path to current directory Makefile is in-tree build
ifeq ($(SOURCE_DIR), $(shell pwd)/)
	SOURCE_DIR := .
endif

GNATCOLL_GPR=$(SOURCE_DIR)/gnatcoll.gpr

TARGET := $(shell gcc -dumpmachine)
NORMALIZED_TARGET := $(subst normalized_target:,,$(wordlist 6,6,$(shell gprconfig  --config=ada --target=$(TARGET) --mi-show-compilers)))
ifeq ($(NORMALIZED_TARGET),)
  $(error No toolchain found for target "$(TARGET)")
endif

prefix := $(dir $(shell $(WHICH) gnatls))..

GNATCOLL_PROJECTS := yes
BUILD         = PROD
PROCESSORS    = 0
ENABLE_SHARED = yes
INTEGRATED    = no
GNATCOV       =

all: build

GTARGET=--target=$(NORMALIZED_TARGET)

ifeq ($(GNATCOV), yes)
   INSTR_BUILD_OPTS=--gnatcov
else
   ifeq ($(SYMCC), yes)
      INSTR_BUILD_OPTS=--symcc
   else
      INSTR_BUILD_OPTS=
   endif
endif

ifeq ($(INTEGRATED), yes)
   integrated_install=/$(NORMALIZED_TARGET)
endif

BUILD_ARGS=--jobs=$(PROCESSORS) \
 --build=$(BUILD) \
 --target=$(NORMALIZED_TARGET) \
 --prefix=local-install \
 --install \
 --enable-shared=$(ENABLE_SHARED)

build:
	rm -rf local-install
	mkdir -p local-install/share/gpr
	
ifeq ($(GNATCOLL_PROJECTS), yes)
	$(SED) -e 's/^--  with "gnatcoll_projects"/with "gnatcoll_projects"/g' $(GNATCOLL_GPR) > local-install/share/gpr/gnatcoll.gpr
else
	$(SED) -e 's/^with "gnatcoll_projects"/--  with "gnatcoll_projects"/g' $(GNATCOLL_GPR) > local-install/share/gpr/gnatcoll.gpr
endif

	$(PYTHON) $(SOURCE_DIR)/minimal/gnatcoll_minimal.gpr.py build $(INSTR_BUILD_OPTS) $(BUILD_ARGS)
	$(PYTHON) $(SOURCE_DIR)/core/gnatcoll_core.gpr.py build $(INSTR_BUILD_OPTS) $(BUILD_ARGS)

ifeq ($(GNATCOLL_PROJECTS), yes)
	$(PYTHON) $(SOURCE_DIR)/projects/gnatcoll_projects.gpr.py build $(INSTR_BUILD_OPTS) $(BUILD_ARGS)
endif

install:
	@echo "Installing gnatcoll into $(prefix)"
	rsync -av ./local-install/ $(prefix)$(integrated_install)  

###########
# Cleanup #
###########

clean:
	$(PYTHON) $(SOURCE_DIR)/projects/gnatcoll_projects.gpr.py clean --add-gpr-path=local-install/share/gpr
	$(PYTHON) $(SOURCE_DIR)/core/gnatcoll_core.gpr.py clean --add-gpr-path=local-install/share/gpr
	$(PYTHON) $(SOURCE_DIR)/minimal/gnatcoll_minimal.gpr.py clean --add-gpr-path=local-install/share/gpr

# Let gprbuild handle parallelisation. In general, we don't support parallel
# runs in this Makefile, as concurrent gprinstall processes may crash.
.NOTPARALLEL:
