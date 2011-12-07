.. _Building_the_GNAT_Reusable_Components:

*****************
Building GNATColl
*****************

The compilation process tries to be as flexible as possible. You can choose
what modules to build, what features they should have,... This
flexibility comes at the cost of a certain complexity in the build
architecture, but that should be mostly transparent to you.

|Important| GNATColl requires a fairly recent Ada05 compatible compiler.
If you do not have such a compiler, please contact `sales@adacore.com <mail:sales@adacore.com>`_

Since you are reading this documentation, it is assumed you have been able
to unpack the package in a temporary directory. In the following instructions,
we will assume the following: `prefix` is the directory in which you
would like to install GNATColl.

.. _Configuring_the_build_environment:

Configuring the build environment
=================================

The first step is to configure the build environment. This is done by
running the `configure` command in the root directory of the
GNATColl tree.

.. index:: GNATCOLL.Projects

.. index:: projects

.. index:: gnat sources

.. index:: gnat_util

Some parts of GNATColl need access to a subset of the GNAT sources.
This is in particular the case for `GNATCOLL.Projects`, which reuses
the same parser as the GNAT tools.

GNATColl will look for those sources in two different ways:

* If you have a copy of the GNAT sources, create a
  link called :file:`gnat_src` that points to the directory containing those
  sources. This link should be created in the toplevel GNATColl
  directory.

* Otherwise, recent versions of GNAT come with an additional `gnat_util.gpr`
  project file, installed along with it. This project contains the required
  subset of the sources. If you have an older version of GNAT, you could
  also chose to install `gnat_util` independently.

* If none of the above is satisfied, GNATColl will not include
  support for `GNATCOLL.Projects`.

`configure` accepts lots of arguments, among which
the following ones are most useful:


*--prefix=`prefix`*
  This specifies the directory in which GNATColl should be installed.

*--enable-shared* and *--disable-shared*
  If none of these switches is specified, GNATColl will try to build
  both static and shared libraries (if the latter are supported on your
  system). The compilation needs to be done twice, since the compilation options
  might not be the same in both cases.

  If you only intend to ever use static libraries, you can use
  `--disable-shared` to only build static libraries.

  When you link GNATColl with your own application, the default is
  to link with the static libraries. You can change this default, which becomes
  the shared libraries if you explicitly specify `--enable-shared`.
  However, even if the default are the static libraries, you can still override
  that (see below the `LIBRARY_TYPE` variable).

*--with-python=`directory`* and *--without-python*
  This specifies where GNATColl should find python. If for instance
  the python executable is in :file:`/usr/bin`, the `directory` to
  specify is :file:`/usr`. In most cases, however, `configure` will be
  able to detect this automatically, so this is only useful if python is
  installed in unusual directories. If you specify the second option,
  support for python will not be build in.

*--enable-shared-python*
  This specifies that the python library should be searched directly
  in `directory`/lib, and thus will in general by the shared library.
  By default, configure will search in a different directory of the python
  installation, and is more likely to find the static library instead (which
  makes distributing your application easier). There is no guarantee though
  that either the shared or the static will be used, since it depends on how
  python was installed on your system.

*--disable-gtk*
  If this switch is specified, then no package depending on the gtk+ graphical
  toolkit will be built.

*--disable-pygtk*
  If this switch is specified, then support for pygtk (:ref:`The_Python_language`) will not be build. The support for this python module will also
  be automatically disabled if python was not found or if you configured with
  `--without-python`.

*--disable-syslog*
  If this switch is specified, then support for syslog (:ref:`Logging_to_syslog`)
  will not be build. This support allows sending the traces from all or part of
  your application to the system logger, rather than to files or stdout.

*--with-postgresql=<dir>* and *--without-postgresql*
  GNATColl embeds a set of packages to query a database engine.
  `configure` attempts to find which systems are installed on your
  system, and build support for those. But you can also explicitly disable
  for those if you need.

  If the directory in which PostgreSQL is installed contains spaces, you
  should use a syntax like

  ::

    ./configure --with-postgres="/Program Files/PostgreSQL/8.4"
    

  Generally speaking, we do not recommend using paths with spaces since there
  are often more difficulties in such a setup.


Special support exists in GNATColl for the gtk+ graphical toolkit.
`configure` will attempt to find the installation directory for this
toolkit by using the `pkg-config` command, which must therefore be
available through your `PATH` environment variable. It also needs to
find the :file:`gtkada.gpr` project file either because it is part of the
implicit search path for project files, or because you have put the
corresponding directory in the environment variable `GPR_PROJECT_PATH`.
If either of these two requirements fail, the modules of GNATColl
that depend on GtkAda will not be built.

::

  ./configure --prefix=/usr/local/gnatcoll --without-python
  

If all goes well (i.e. all required dependencies are found on the system),
configure will generate a number of files, including :file:`Makefile`,
:file:`Makefile.conf` and :file:`gnatcoll_shared.gpr`.

.. _Building_GNATColl:

Building GNATColl
=================

If `configure` has run successfully, it generates a `Makefile`
to allow you to build the rest of GNATColl.
This is done by simply typing the following command:

::

  make
  

Depending on the switches passed to `configure`, this will either
build both static and shared libraries, or static only (see the
`--disable-shared` configure switch).

Optionally, you can also build the examples and/or the automatic test suite,
with the following commands:

::

  make examples
  make test
  

The latter will do a local installation of gnatcoll in a subdirectory called
:file:`local_install`, and use this to run the tests. This ensures that the
installation process of gnatcoll works properly.

.. _Installing_GNATColl:

Installing GNATColl
===================

Installing the library is done with the following command:

::

  make install
  

Note that this makefile target does not try to recompile GNATColl,
so you must build it first.
This will install both the shared and the static libraries if both were
build.

As mentioned in the description of the `configure` switches, your
application will by default be linked with the static library, unless
you specified the `--enable-shared` switch.

However, you can always choose later on which kind of library to use for
GNATColl by setting the environment variable `LIBRARY_TYPE`
to either `"relocatable"` or `"static"`.

Your application can now use the GNATColl code through a project
file, by adding a with clause
to :file:`gnatcoll.gpr`, :file:`gnatcoll_gtk.gpr` or :file:`gnatcoll_python.gpr`.
The second one will also force your application to be linked with the
gtk+ libraries, but provides additional capabilities as documented in each
of the modules.

