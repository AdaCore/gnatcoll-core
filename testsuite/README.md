Running GNATcoll Testsuite
==========================

`The testsuite is currently under construction !`

To run it you need to have Python installed along with the package
e3-testsuite.

To install e3-testsuite:

```sh
pip install git+https://github.com/AdaCore/e3-testsuite.git
```

Then do

```sh
./run-tests
```

In order to have coverage information with gcov, just add `--gcov`. In that
case a summary of the coverage information is displayed at the end of the
testsuite. Full coverage information can be found in `gcov/results`
subdirectory.
