Building GNATColl Core Libraries
********************************

Build Requirements
==================

* A recent GNAT compiler
* GPRBuild
* Python (>=3.8) to configure, build and install
* (optional) LIBGPR library if you are building the ``gnatcoll_project.gpr``
* (optional) GNU Make to use the legacy Makefile
* (optional) Alire use Alire as build tool

As stated in the introduction, GNATColl Core has been split into three distinct
libraries in order to improve overall architecture: ``gnatcoll_minimal``,
``gnatcoll_core`` and ``gnatcoll_projects``. Each library is located in a separate
sub-directory of the GNATColl sources.

In the instructions detailed below, it is assumed that ``SRC_DIR`` is the location
of the GNATColl sources, ``BUILD_DIR`` the directory in which a library build is
done and ``INSTALL_DIR`` the final location in which a library is installed. Note
that ``BUILD_DIR`` may be equal to ``SRC_DIR``.

Configure and Build using Python helper scripts
===============================================

The simplest way to build one of the libraries part of GNATColl Core is to do:

.. code-block:: console

    $ cd $BUILD_DIR
    $ $SRC_DIR/minimal/gnatcoll_minimal.gpr.py build --prefix=$INSTALL_DIR --install

This configures the library using default options and install it in
``INSTALL_DIR``. By default both shared and static version of the libraries are
built and installed. To debug how a given library was configured by the helper
script, check the JSON file generated in the ``BUILD_DIR``. For example for
``gnatcoll_minimal.gpr`` project the JSON is ``BUILD_DIR/gnatcoll_core.json``.

The helper scripts provide various options that impact the way the library is
configured. The list of option can easily be retrieved by calling
``PROJECT.py build --help``. For example for the ``gnatcoll_minimal.gpr`` project do:

.. code-block:: console

    $ SRC_DIR/minimal/gnatcoll_minimal.gpr.py build --help

    usage: gnatcoll_minimal.gpr.py build [-h] [--gpr-opts ...]
                                         [--add-gpr-path ADD_GPR_PATH]
                                         [--jobs JOBS]
                                         [--target TARGET] [--prefix PREFIX]
                                         [--integrated] [--install]
                                         [--gnatcov | --symcc]
                                         [--configure-only]
                                         [--enable-constant-updates]
                                         [--build {DEBUG,PROD}]
                                         [--enable-shared {yes,no}]

    options:
      -h, --help            show this help message and exit
      --gpr-opts ...        pass remaining arguments to gprbuild
      --add-gpr-path ADD_GPR_PATH
                            prepend a path to look for GPR files
      --jobs, -j JOBS       gprbuild parallelism
      --target TARGET       target
      --prefix PREFIX       installation prefix
      --integrated          installation in platform specific subdir
      --install             proceed with install automatically after the build
      --gnatcov             build project with gnatcov instrumentation
      --symcc               build project with symcc intrumentation (works only
                            with LLVM)
      --configure-only      only perform configuration (i.e: update of project
                            constants and creation of json file).
                            Can be used to integrate with Alire
      --enable-constant-updates
                            Update constants in GPR files in order to pass conviently
                            the result of the
                            configuration to tools such as IDE and Alire.

    project specific options:
      --build {DEBUG,PROD}
      --enable-shared {yes,no}

To build and install the three libraries do:

.. code-block:: console

    $ cd $BUILD_DIR
    $ $SRC_DIR/minimal/gnatcoll_minimal.gpr.py build --prefix=$INSTALL_DIR --install
    $ $SRC_DIR/core/gnatcoll_core.gpr.py build --prefix=$INSTALL_DIR --install
    $ $SRC_DIR/projects/gnatcoll_projects.gpr.py build --prefix=$INSTALL_DIR --install

Note that order matters because of the dependencies between each libraries

Once built, you can make the libraries visible by adding ``INSTALL_DIR/share/gpr``
to your ``GPR_PROJECT_PATH`` environment variable.

Note that if you still rely on the ``gnatcoll.gpr`` project file you can find it
in ``SRC_DIR/gnatcoll.gpr``. The project is there for backward compatibility and just
do a with of the three GNATColl libraries. To make it available in your installation
just do:

.. code-block:: console

    $ cp $SRC_DIR/gnatcoll.gpr $INSTALL_DIR/share/gpr

Configure and Build using Makefile
==================================

For backward compatibility purpose, a ``Makefile`` is still provided.

To perform an in-place build, run:

.. code-block:: console

    $ cd $SRC_DIR
    $ make
    $ make install prefix=$INSTALL_DIR

To build outside the source tree, run:

.. code-block:: console

    $ cd $BUILD_DIR
    $ make -f $SRC_DIR/Makefile
    $ make -f $SRC_DIR/Makefile install prefix=$INSTALL_DIR

Aside for the ``prefix`` Makefile variable, a few other variables are available. Check
the headers of ``SRC_DIR/Makefile`` to get the full documentation.

Advanced Configuration and Build Topics
=======================================

Building Manually
-----------------

All the libraries part of GNATColl Core can be built manually using directly GPRBuild.
When doing so we will lost the auto-configuration part performed by the Python
scripts. As a consequence, the value of those variables should be passed manually in
order to have the adequate build. Check the distinct project files in order to have a
list of available scenario variables.

Updating scenario variables default values
------------------------------------------

The python helper scripts provided with each library include an option
``--enable-constant-update``, which updates the default values for the project
scenario variables. The default scenario variables values are contained in
subprojects (one for each library) located in the respective ``config``
subdirectories.

For example for ``gnatcoll_core.gpr`` libraries in ``core`` subdirectories, there
is a subproject ``config/gnatcoll_core_constants.gpr``::

    abstract project GNATCOLL_Core_Constants is
       GNATCOLL_VERSION_DEFAULT     := "0.0";
       GNATCOLL_MMAP_DEFAULT        := "yes";
       GNATCOLL_MADVISE_DEFAULT     := "yes";
       GNATCOLL_BLAKE3_ARCH_DEFAULT := "generic";
       GNATCOLL_XXHASH_ARCH_DEFAULT := "generic";
       GNATCOLL_BUILD_MODE_DEFAULT  := "PROD";
       GNATCOLL_OS_DEFAULT          := "unix";
    end GNATCOLL_Core_Constants;

If on an ARM64 Linux platform, you launch from ``SRC_DIR/core`` directory:

.. code-block:: console

    $ ./gnatcoll_core.gpr.py build --configure-only --enable-constant-update

Then the ``config/gnatcoll_core_constants.gpr`` project is updated to::

    abstract project GNATCOLL_Core_Constants is
       GNATCOLL_VERSION_DEFAULT     := "25.0";
       GNATCOLL_MMAP_DEFAULT        := "yes";
       GNATCOLL_MADVISE_DEFAULT     := "yes";
       GNATCOLL_BLAKE3_ARCH_DEFAULT := "aarch64-linux";
       GNATCOLL_XXHASH_ARCH_DEFAULT := "generic";
       GNATCOLL_BUILD_MODE_DEFAULT  := "PROD";
       GNATCOLL_OS_DEFAULT          := "unix";
    end GNATCOLL_Core_Constants;

The consequence on ``gnatcoll_core.gpr`` is that for example the scenario variable
GNATCOLL_BLAKE3_ARCH will now have a default value of ``aarch64-linux`` rather then
``generic``.

This system can be useful in some contexts such as:

* Integration with other build systems such as Alire (as a prebuild stage)
* Easier loading of the project in IDEs (which usually set the scenario variables
  to their default values)

