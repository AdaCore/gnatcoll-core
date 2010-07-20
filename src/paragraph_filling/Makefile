.PHONY: all
all: all-exes test #???big-test

################
# Executables

.PHONY: all-exes
all-exes: fill_ada_comments fill_text

.PHONY: fill_ada_comments
fill_ada_comments:
	gnatmake -p -P paragraph_filling.gpr fill_ada_comments.adb

.PHONY: fill_text
fill_text:
	gnatmake -p -P paragraph_filling.gpr fill_text.adb

################
# Tests

# Note: For the -none method tests, we can use the input file as the known-good
# file, because -none is not supposed to do anything.

.PHONY: test
test: all-exes
	./fill_ada_comments -none sem_ch3.adb sem_ch3.adb.out
	diff sem_ch3.adb sem_ch3.adb.out
	./fill_ada_comments -greedy sem_ch3.adb sem_ch3.adb.out
	diff sem_ch3.adb.greedy.known-good sem_ch3.adb.out
	./fill_ada_comments -pretty sem_ch3.adb sem_ch3.adb.out
	diff sem_ch3.adb.pretty.known-good sem_ch3.adb.out
	./fill_ada_comments -knuth sem_ch3.adb sem_ch3.adb.out
	diff sem_ch3.adb.knuth.known-good sem_ch3.adb.out
#	./fill_text -greedy basic_fill_input basic_fill_output
#	diff greedy_output.known_good basic_fill_output
#	./fill_text -pretty basic_fill_input basic_fill_output
#	diff pretty_output.known_good basic_fill_output
#	time ./fill_text -slow slow_input slow_output
#	./fill_text -knuth slow_input knuth_output
#	diff slow_output knuth_output

.PHONY: big-test
big-test: fill_ada_comments
	time ./fill_ada_comments -none gnat-sources.ada gnat-sources.ada.out
	diff gnat-sources.ada gnat-sources.ada.out
	time ./fill_ada_comments -greedy gnat-sources.ada gnat-sources.ada.out
	diff gnat-sources.ada.greedy.known-good gnat-sources.ada.out
	time ./fill_ada_comments -pretty gnat-sources.ada gnat-sources.ada.out
	diff gnat-sources.ada.pretty.known-good gnat-sources.ada.out
	time ./fill_ada_comments -knuth gnat-sources.ada gnat-sources.ada.out
	diff gnat-sources.ada.knuth.known-good gnat-sources.ada.out

################
# Misc

.PHONY: clean
clean:
	rm -rf obj
	rm -f *.ali *.o *.exe fill_ada_comments fill_text b~*.ads b~*.adb *.out
