The GNAT Components Collection (GNATcoll) - Core packages
=========================================================

This is the core module of the GNAT Components Collection. Please refer to the
documentation in the `docs/` directory.

Code status
===========

Build status with GNAT GPL 2017

Platform | Status
---------|-------
Linux    | [![Build Status](https://travis-ci.org/AdaCore/gnatcoll-core.svg?branch=master)](https://travis-ci.org/AdaCore/gnatcoll-core)
Windows  | [![Build status](https://ci.appveyor.com/api/projects/status/31a7dh523xto7b9f/branch/master?svg=true)](https://ci.appveyor.com/project/github-integration-adacore/gnatcoll-core/branch/master)

Dependencies
------------

This module depends on the following external components, that should be
available on your system:

- GPRbuild

Configuring the build process
-----------------------------

The following variables can be used to configure the build process:

General:

* `prefix`: location of the installation, the default is the running GNAT
  installation root.

* `BUILD`: control the build options: `PROD` (default) or `DEBUG`

* `PROCESSORS` : parallel compilation (default is 0, which uses all available
  cores)

* `TARGET`: for cross-compilation, auto-detected for native platforms

* `SOURCE_DIR`: for out-of-tree build

* `INTEGRATED`: treat prefix as compiler installation (yes/no) this is so that
  installed gnatcoll project can later be referenced as predefined project of
  this compiler; this adds a normalized target subdir to prefix default is "no"

Module-specific:

* `GNATCOLL_MMAP`: whether MMAP is supported (yes/no) default is "yes"; has no
  effect on Windows
* `GNATCOLL_MADVISE`: whether MADVISE is supported (yes/no) default is "yes";
  has no effect on Windows
* `GNATCOLL_ATOMICS`: atomic model (intrinsic/mutex) default is "intrinsic"

To use the default options:

```sh
$ make setup
```

For example, to setup GNATcoll to install a debug version in
`/opt/libgnatcoll`:

```sh
$ make prefix=/opt/libgnatcoll BUILD=DEBUG install
```


Building
--------

GNATcoll Core Module is built using a GPR project file, to build it is as
simple as:

```sh
$ gprbuild gnatcoll.gpr
```

Though, to build all versions of the library (static, relocatable and
static-pic) it is simpler to use the provided Makefile:

```sh
$ make
```

Then, to install it:

```sh
$ make install
```


Bug reports
-----------

Please send questions and bug reports to report@adacore.com following
the same procedures used to submit reports with the GNAT toolset itself.
