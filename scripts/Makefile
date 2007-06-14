include ../Makefile.conf


ifeq (${WITH_GTK},yes)
all:      all_text all_gtk
examples: examples_text examples_gtk
clean:    clean_text    clean_gtk
else
all:      all_text
examples: examples_text
clean:    clean_text
endif

## Current versions of gprmake have no way to build all sources of a
## given project. It doesn't really matter, we just build the examples
## directly. or we could do a "gnatmake -u", followed by "gprmake"

all_text: obj/python_support.o
	gnatmake -q -u -Pgnatlib_scripts
all_gtk: obj/python_support_gtk.o
	gprmake -Pgnatlib_scripts_gtk

ifeq ($(WITH_PYTHON),yes)
obj/python_support.o: src/python/python_support.c
	gcc -c ${PYTHON_CFLAGS} $< -o $@
obj/python_support_gtk.o: src_gtk/python/python_support_gtk.c
	gcc -c ${PYTHON_CFLAGS} ${PYGTK_INCLUDE} ${GTK_INCLUDE} $< -o $@
else
obj/python_support.o:
obj/python_support_gtk.o:
endif


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
