all:
include ../Makefile.conf

ADA_PROJECT_PATH:=${prefix}/lib/gnat${DIRSEP}${ADA_PROJECT_PATH}${DIRSEP}../src
LD_LIBRARY_PATH:=${prefix}/lib:${LD_LIBRARY_PATH}
export ADA_PROJECT_PATH
export LD_LIBRARY_PATH

all:
	gnatmake -p -Pexamples
ifeq ($(WITH_GMP),yes)
	gprbuild -p -Pgmp/gmp_examples.gpr
endif
	@echo "==================================================="
	@echo "== To run, set the environment variable LD_LIBRARY_PATH"
	@echo "==   LD_LIBRARY_PATH=${prefix}/lib/gnat:\$$LD_LIBRARY_PATH"

clean:
	-gprclean -q -Pexamples
ifeq ($(WITH_GMP),yes)
	-gprclean -q -Pgmp/gmp_examples.gpr
endif
