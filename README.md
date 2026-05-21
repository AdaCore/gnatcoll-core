# The GNAT Components Collection (GNATcoll) - Core packages

This is the core module of the **GNAT Components Collection (GNATcoll)**.
For detailed documentation, refer to the `docs/` directory.

## Structure

The repository contains three projects:

* `minimal/gnatcoll_minimal.gpr`: packages that may work on more restrictive cross
  environments.
* `core/gnatcoll_core.gpr`: packages that can be used in POSIX or Win32
  environments.
* `projects/gnatcoll_projects.gpr`: high level binding to libgpr project

For backward compatibility a project called `gnatcoll.gpr` is also provided

## Dependencies

GNATCOLL requires:

* A recent GNAT compiler
* GPRBuild tools
* (optional) LIBGPR library (only if using `gnatcoll_project.gpr`)
* (optional) Python (>3.8) to easily configure, build, install each projects.
* (optional) GNU Make to use the legacy makefile that builds the three projects.

## Building and Installing

Each project has a helper Python script to handle automatic configuration
and launch of gpr tools. The script is located next to each project and
called `PROJECT_NAME.gpr.py`.

## Building

The simplest way to build the three projects is to use the Python helper scripts.
Below is an example build process assuming `SRC_DIR` is the repository location,
`BUILD_DIR` is the build directory, and `INSTALL_DIR` is the target installation
location:

```console
$ cd $BUILD_DIR
$ $SRC_DIR/minimal/gnatcoll_minimal.gpr.py build --install --prefix=$INSTALL_DIR
$ $SRC_DIR/core/gnatcoll_core.gpr.py build --install --prefix=$INSTALL_DIR
$ $SRC_DIR/projects/gnatcoll_projects.gpr.py build --install --prefix=$INSTALL_DIR
```

To use the projects, add `INSTALL_DIR/share/gpr` to your `GPR_PROJECT_PATH`
environment variable.

For more information on how to configure or build GNATColl, check the `docs`
subdirectory.

## Bug reports

Please send questions and bug reports to support@adacore.com following
the same procedures used to submit reports with the GNAT toolset itself.
