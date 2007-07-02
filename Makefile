.PHONY: all examples tests valgrind clean docs

all tests examples valgrind:
	${MAKE} -C src $@
	${MAKE} -C scripts $@

clean:
	${MAKE} -C src $@
	${MAKE} -C scripts $@
	${MAKE} -C docs $@

docs:
	${MAKE} -C docs
