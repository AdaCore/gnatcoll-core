#!/usr/bin/env python3
from __future__ import annotations
from typing import TYPE_CHECKING
import logging
import sys
import os

# Support code is located in parent directory
SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.dirname(SOURCE_DIR))

from gprproject import BuilderApp

if TYPE_CHECKING:
    import argparse
    from gprproject.gprbuild import GPRTool


class GNATCollCore(BuilderApp):
    project_file = os.path.join(SOURCE_DIR, "gnatcoll_core.gpr")
    description = "GNATCOLL Core library." ""
    constants_project_file = os.path.join(
        SOURCE_DIR, "config", "gnatcoll_core_constants.gpr"
    )
    constants = [
        "GNATCOLL_OS",
        "GNATCOLL_BUILD_MODE",
        "GNATCOLL_VERSION",
        "GNATCOLL_MMAP",
        "GNATCOLL_ADVISE",
        "GNATCOLL_BLAKE3_ARCH",
        "GNATCOLL_XXHASH_ARCH",
    ]

    def add_arguments(self, parser: argparse.ArgumentParser) -> None:
        parser.add_argument("--build", choices=["DEBUG", "PROD"], default="PROD")
        parser.add_argument("--enable-shared", choices=["yes", "no"], default="yes")

    def adjust_config(self, gpr: GPRTool, args: argparse.Namespace) -> None:
        # Compute which implementation should be used for blake3
        if gpr.target in ("x86_64-linux", "aarch64-linux", "x86_64-windows"):
            blake3_arch = gpr.target
        else:
            blake3_arch = "generic"

        logging.debug(f"blake3 implementation: {blake3_arch}")
        gpr.set_variable("GNATCOLL_BLAKE3_ARCH", blake3_arch)

        # Compute which implementation should be used for xxhash
        if gpr.target in ("x86_64-linux", "x86_64-windows"):
            xxhash_arch = "x86_64"
        else:
            xxhash_arch = "generic"

        logging.debug(f"xxhash implementation: {xxhash_arch}")
        gpr.set_variable("GNATCOLL_XXHASH_ARCH", xxhash_arch)

        gpr.set_variable("GNATCOLL_MMAP", "yes")
        gpr.set_variable("GNATCOLL_MADVISE", "yes")

        with open(os.path.join(SOURCE_DIR, "VERSION")) as fd:
            version = fd.read().strip()
        gpr.set_variable("GNATCOLL_VERSION", version)

        if "windows" in gpr.target:
            gnatcoll_os = "windows"
        elif "darwin" in gpr.target:
            gnatcoll_os = "osx"
        else:
            gnatcoll_os = "unix"
        gpr.set_variable("GNATCOLL_OS", gnatcoll_os)
        gpr.set_variable("GNATCOLL_BUILD_MODE", args.build)

        if args.gnatcov:
            gpr.set_variable("LIBRARY_TYPE", "static")
        else:
            gpr.variants_var = "LIBRARY_TYPE"
            if args.enable_shared == "yes":
                gpr.variants_values = ["static", "relocatable", "static-pic"]
            else:
                gpr.variants_values = ["static"]


if __name__ == "__main__":
    app = GNATCollCore()
    sys.exit(app.run())
