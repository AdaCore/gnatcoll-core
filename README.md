The GNAT Components Collection (GNATcoll) - Core packages
=========================================================

This is the core module of the GNAT Components Collection. Please refer to the
documentation in the `docs/` directory.

Code status
===========

TBD

Dependencies
------------

GNATCOLL requires a recent GNAT compiler. Building it also requires
GPRbuild. GNATCOLL.Projects depends on LIBGPR library which is part of
GPRbuild project.

Configuring the build process
-----------------------------

The following variables can be used to configure the build process:

General:

* `prefix`: location of the installation, the default is the running GNAT
  installation root.

* `ENABLE_SHARED`: Use relocatable and static-pic libraries in addition
  to static ones (default is 'yes').

* `BUILD`: control the build options: `PROD` (default) or `DEBUG`

* `PROCESSORS`: parallel compilation (default is 0, which uses all available
  cores)

* `TARGET`: for cross-compilation, auto-detected for native platforms

* `SOURCE_DIR`: for out-of-tree build

* `INTEGRATED`: if `yes` (default is `no`), consider that `prefix` is where the
  toolchain is installed and install GNATcoll in a target-dependent
  subdirectory. This makes it possible to install GNATcoll multiple times for
  the various compilers in the same prefix. Enable this only for cross
  compilers.

Module-specific:

* `GNATCOLL_MMAP`: whether MMAP is supported (yes/no) default is "yes"; has no
  effect on Windows
* `GNATCOLL_MADVISE`: whether MADVISE is supported (yes/no) default is "yes";
  has no effect on Windows

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

Building all versions of the GNATCOLL Core Packages (static, relocatable and
static-pic) is as easy as running `make` in the top directory. Then, to install
it:

```sh
$ make install
```

Note that underneath, this Makefile uses a GPR project file: `gnatcoll.gpr`.
You can build GNATCOLL using it with GPRbuild, but make sure to use the same
command-line options.


Bug reports
-----------

Please send questions and bug reports to report@adacore.com following
the same procedures used to submit reports with the GNAT toolset itself.
