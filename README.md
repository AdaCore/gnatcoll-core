# The GNAT Components Collection (GNATcoll) - Core packages

This is the core module of the GNAT Components Collection. Please refer to the
documentation in the `docs/` directory.

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
* Gprbuild tools
* (optional) LIBGPR library (only if using `gnatcoll_project.gpr`)
* (optional) Python (>3.8) to easily configure, build, install each projects.
* (optional) GNU Make to use the legacy makefile that builds the three projects.

## Building and Installing

Each project has a helper Python script to handle automatic configuration
and launch of gpr tools. The script is located next to each project and
called `PROJECT_NAME.gpr.py`.

## Building

The simplest way to build a given project is to use the Python helper script.

For example to build gnatcoll_core.gpr project:

```sh
$ ./gnatcoll_core.gpr.py build
```

For each project the script provides the following commands:

* `build`: to build and configure the project
* `install`': to install the built project (gprinstall step)
* `clean`': to clean all object files (gprclean step)
* `uninstall`': to uninstall the project (gprinstall --uninstall step)

### Build Command

Use `--help` switch to get the full help on a command. For the build command
the results is:

```sh
$ ./gnatcoll_core.gpr.py build --help
usage: gnatcoll_core.gpr.py build [-h] [--gpr-opts ...] [--add-gpr-path ADD_GPR_PATH] [--jobs JOBS]
                                  [--target TARGET] [--prefix PREFIX] [--integrated] [--install]
                                  [--gnatcov | --symcc] [--configure-only] [--enable-constant-updates]
                                  [--build {DEBUG,PROD}] [--enable-shared {yes,no}]

options:
  -h, --help            show this help message and exit
  --gpr-opts ...        pass remaining arguments to gprbuild
  --add-gpr-path ADD_GPR_PATH
                        prepend a path to look for GPR files
  --jobs JOBS, -j JOBS  gprbuild parallelism
  --target TARGET       target
  --prefix PREFIX       installation prefix
  --integrated          installation in platform specific subdir
  --install             proceed with install automatically after the build
  --gnatcov             build project with gnatcov instrumentation
  --symcc               build project with symcc intrumentation (works only with LLVM)
  --configure-only      only perform configuration (i.e: update of project constants and creation of
                        json file). Can be used to integrate with Alire
  --enable-constant-updates
                        Update constants in GPR files in order to pass conviently the result of the
                        configuration to tools such as IDE and Alire.

project specific options:
  --build {DEBUG,PROD}
  --enable-shared {yes,no}
```

The first set of options is generic, and the second set under `project specific options`
is usually connected to some scenario variables in the project.

On call to the build command a JSON file is generated containing all the complete
configuration of the project. For example for `gnatcoll_minimal.gpr`, if you call from a
directory called `OBJ_DIR` the following command:

```sh
$ SOURCE_DIR/minimal/gnatcoll_minimal.gpr.py build
...
```

This will generate in `OBJ_DIR` the file called `gnatcoll_minimal.json`:

```json
{
  "project_file": "SOURCE_DIR/minimal/gnatcoll_minimal.gpr",
  "object_dir": "OBJ_DIR",
  "target": null,
  "integrated": false,
  "variables": {
    "GNATCOLL_BUILD_MODE": "PROD",
    "GNATCOLL_VERSION": "26.0",
    "GNATCOLL_OS": "unix"
  },
  "jobs": "0",
  "variants_var": "LIBRARY_TYPE",
  "variants_values": [
    "static",
    "relocatable",
    "static-pic"
  ],
  "gnatcov": false,
  "prefix": "COMPILER_PREFIX",
  "gpr_paths": []
}
```

In addition, to the generated JSON file a project file might be updated to
update the default values for the managed scenario variables. In the
context of `gnatcoll_minimal.gpr` the project is located in
`config/gnatcoll_minimal_constants.gpr` and contains the following:

```
abstract project GNATCOLL_Minimal_Constants is
    GNATCOLL_VERSION_DEFAULT     := "26.0";
    GNATCOLL_BUILD_MODE_DEFAULT  := "PROD";
    GNATCOLL_OS_DEFAULT          := "unix";
end GNATCOLL_Minimal_Constants;
```

This update can be enabled by passing `--enable-constant-updates`.

Having a project with the updated defaults ease work with:

* `Alire`: You can use `build --configure-only --enable-constant-update`
  command to as a pre-build step in the `Alire` configuration to generate
  the right values for the scenario variables
* `IDE`: When opening the project the right value is automatically selected
  for all the scenario variables


## Bug reports

Please send questions and bug reports to support@adacore.com following
the same procedures used to submit reports with the GNAT toolset itself.
