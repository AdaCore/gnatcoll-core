include ../Makefile.conf

## Current versions of gprmake have no way to build all sources of a
## given project. It doesn't really matter, we just build the examples
## directly. or we could do a "gnatmake -u", followed by "gprmake"

all_text:
	gnatmake -u -Pgnatlib_scripts
	gprmake -Pgnatlib_scripts
all_gtk:
	gnatmake -u -Pgnatlib_scripts_gtk
	gprmake -Pgnatlib_scripts_gtk
examples_text:
	gprmake -q -Pexamples/examples
examples_gtk:
	gprmake -q -Pexamples/examplesgtk
tests:
	gprmake -q -Ptestsuite/testsuite
	cd testsuite && ./run.sh
valgrind:
	@VALGRIND=yes ${MAKE} tests
clean_text:
	gnatclean -r -q -Ptestsuite/testsuite
	gnatclean -r -q -Pexamples/examples
clean_gtk:
	gnatclean -r -q -Pexamples/examplesgtk
docs:
	${MAKE} -C docs
