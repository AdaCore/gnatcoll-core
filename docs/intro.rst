Introduction to the GNATColl Libraries
**************************************

The **GNATColl Core** library is a comprehensive collection of Ada packages
developed by AdaCore and the open-source community. Originally designed to
serve as the foundation for the GNAT Programming Studio (GPS) and internal
AdaCore projects, it is now widely used across many Ada applications.

For greater modularity, the original ``gnatcoll.gpr`` library project has been
divided **into** three libraries:

* ``gnatcoll_minimal.gpr``: Packages that can be reused in non-native
  contexts or restricted runtimes, such as atomic counters, locks, and promises.
* ``gnatcoll_core.gpr``: Packages that provide a comprehensive set of features,
  including portable OS interfaces for files and processes, JSON data handling,
  and command-line argument parsing.
* ``gnatcoll_projects.gpr``: A higher-level interface to the GPRBuild projects API
  (relies on libgpr1).

Bug reports
-----------

For questions or bug reports, contact support@adacore.com and follow the same
procedures used for the GNAT toolset.
