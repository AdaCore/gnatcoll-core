all:
include ../Makefile.conf

ADA_PROJECT_PATH:=${prefix}/lib/gnat${DIRSEP}${ADA_PROJECT_PATH}${DIRSEP}../src
LD_LIBRARY_PATH:=${prefix}/lib:${LD_LIBRARY_PATH}
export ADA_PROJECT_PATH
export LD_LIBRARY_PATH

all:
	@${MKDIR} obj
	gnatmake -Pexamples
ifeq ($(WITH_GTK),yes)
	gprbuild -Pexamplesgtk -p
endif
ifeq ($(WITH_GMP),yes)
	gprbuild -Pgmp/gmp_examples.gpr -p
endif
	@echo "==================================================="
	@echo "== To run, set the environment variable LD_LIBRARY_PATH"
	@echo "==   LD_LIBRARY_PATH=${prefix}/lib/gnat:\$$LD_LIBRARY_PATH"

clean:
	-gprclean -q -Pexamples
ifeq ($(WITH_GTK),yes)
	-gprclean -q -Pexamplesgtk
endif
ifeq ($(WITH_GMP),yes)
	-gprclean -q -Pgmp/gmp_examples.gpr
endif
