#!/usr/bin/env python
import sys
import os



# Get the current directory and add drivers subdirectory to path
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, ROOT_DIR) # needed for drivers imports

from drivers import make_gnatcoll, TESTSUITE_ROOT_DIR, GNATCOLL_ROOT_DIR
sys.path.insert(0, GNATCOLL_ROOT_DIR) # needed for gprproject imports

from gprproject.testsuite.drivers.basic import BasicTestDriver
from drivers.build_and_run import BuildAndRunDriver
from drivers.build_run_diff import BuildRunDiffDriver
from drivers.build_run_assert_and_diff import BuildRunAssertAndDiffDriver
from drivers.json_validation import JSONValidationDriver
from drivers.data_validation import DataValidationDriver
from gprproject.testsuite import LibTestsuite

class CommonTestsuite(LibTestsuite):
    tests_subdir = "tests"
    test_driver_map = {
        "json_validation": JSONValidationDriver,
        "data_validation": DataValidationDriver,
        "default": BasicTestDriver,
        "build_and_run": BuildAndRunDriver,
        "build_run_diff": BuildRunDiffDriver,
        "build_run_assert_and_diff": BuildRunAssertAndDiffDriver
    }

    def add_options(self, parser):
        super().add_options(parser)

        parser.add_argument(
            "--recompile",
            help="recompile debug version of gnatcoll for testing."
            + " Not compatible with the --gnatcov option."
            + " If both are specified, only --recompile is used.",
            default=False,
            action="store_true",
        )
        parser.add_argument(
            "--llvm",
            help="assume that an LLVM-based toolchain is used.",
            default=False,
            action="store_true",
        )

    def set_up(self):
        super().set_up()
        self.env.llvm = self.main.args.llvm

        if self.env.gnatcov and self.main.args.recompile:
            print(
                "WARNING: --gnatcov option is not compatible with the"
                + " --recompile option. --gnatcov is ignored."
            )
            self.env.gnatcov = False

        if self.main.args.recompile:
            work_dir = os.path.join(TESTSUITE_ROOT_DIR, "debug")
            gpr_dir, _, _ = make_gnatcoll(work_dir, debug=True)
            self.env.gnatcoll_debug_gpr_dir = gpr_dir

    @property
    def default_driver(self):
        return "default"