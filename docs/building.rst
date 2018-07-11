.. _Building_GNATColl:

*****************
Building GNATColl
*****************

In the instructions detailed below, it is assumed that you have
unpacked the GNATColl package in a temporary directory and that
`installdir` is the directory in which you
would like to install the selected components.

It is further assumed that you have recent functional GNAT compiler, as well
as gprbuild.

.. _Configuring_the_build_environment:

Configuring the build environment
=================================

The first step is to configure the build environment. This is done by
running the `make setup` command in the root directory of the
GNATColl tree. This step is optional if you are satisfied with default values.

On Windows, this requires a properly setup Unix-like environment, to provide
Unix-like tools.

The following variables can be used to configure the build process:

General:

*prefix*
  Location of the installation, the default is the running GNAT installation root.

*INTEGRATED*
  Treat prefix as compiler installation: yes or no (default). This is so that installed gnatcoll project can later be referenced as a predefined project of this compiler; this adds a normalized target subdir to prefix.

*BUILD*
  Controls the build options : PROD (default) or DEBUG

*PROCESSORS*
  Parallel compilation (default is 0, which uses all available cores)

*TARGET*
  For cross-compilation, auto-detected for native platforms

*SOURCE_DIR*
  For out-of-tree build

*ENABLE_SHARED*
  Controls whether shared and static-pic library variants should be built: yes (default) or no. If you only intend to use static libraries, specify 'no'.

Module-specific:

*GNATCOLL_MMAP*
  Whether MMAP is supported: yes (default) or no; this has no effect on Windows where embedded MMAP implementation is always provided.

*GNATCOLL_MADVISE*
  Whether MADVISE: yes (default) or no; this has no effect on Windows where MADVISE functionality is unavailable


.. _Building_GNATColl:

Building GNATColl
=================

GNATCOLL Core Module can be built using a GPR project file, to build it is as
simple as:

  $ gprbuild gnatcoll.gpr

Though, to build all versions of the library (static, relocatable and
static-pic) it is simpler to use the provided Makefile:

  $ make


.. _Installing_GNATColl:

Installing GNATColl
===================

Installing the library is done with the following command::

  make install
  
Note that this command does not try to recompile GNATColl, so you must build
it first. This command will install all library variants that were built.

Your application can now use the GNATColl code through a project file, by
adding a ``with`` clause to :file:`gnatcoll.gpr`.

If you wish to install in a different location than was specified at
configure time, you can override the "prefix" variable from the command line,
for instance::

    make prefix=/alternate/directory install

This does not require any recompilation.
