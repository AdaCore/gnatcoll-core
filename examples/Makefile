include ../Makefile.conf

ADA_PROJECT_PATH:=${prefix}/lib/gnat:${ADA_PROJECT_PATH}
LD_LIBRARY_PATH:=${prefix}/lib:${LD_LIBRARY_PATH}
export ADA_PROJECT_PATH
export LD_LIBRARY_PATH

all:
	gnatmake -Pexamples
	gnatmake -Pexamplesgtk
	@echo "==================================================="
	@echo "== To run, set the environment variable LD_LIBRARY_PATH"
	@echo "==   LD_LIBRARY_PATH=${prefix}/lib/gnat:\$$LD_LIBRARY_PATH"

clean:
	gnatclean -q -Pexamples
	gnatclean -q -Pexamplesgtk
