from __future__ import annotations
from e3.fs import rm, mkdir
from e3.testsuite import Testsuite
from .drivers.basic import BasicTestDriver
from .drivers.gnatcov import produce_report
import os
import sys

ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
SUPPORT_DIR = os.path.join(ROOT_DIR, "support")


class LibTestsuite(Testsuite):
    """Main class to derive in order to start a testsuite."""

    enable_cross_support = True
    tests_subdir = "tests"
    test_driver_map = {"default": BasicTestDriver}

    @property
    def default_driver(self):
        return "default"

    @property
    def default_source_dirs(self) -> list[str]:
        result = [SUPPORT_DIR]
        if os.path.isdir(os.path.join(self.root_dir, "support")):
            result.append(os.path.join(self.root_dir, "support"))
        return result

    @property
    def default_project_file(self) -> str:
        return os.path.join(SUPPORT_DIR, "test.gpr")

    @property
    def default_withed_projects(self) -> list[str]:
        return []

    def add_options(self, parser):
        group = parser.add_mutually_exclusive_group()
        group.add_argument(
            "--gnatcov",
            help="enable gnatcov mode. tested library should be built previously "
            "with --gnatcov too.",
            default=False,
            action="store_true"
        )
        parser.add_argument(
            "--source-root",
            help="Option specific to the GNATcoverage Cobertura coverage"
            + " report: remove the specified prefix from the filenames in"
            + " the report. Must be used with the --gnatcov option.",
            default=None,
        )
        group.add_argument(
            "--valgrind",
            help="check memory usage with Valgrind (memcheck tool)",
            action="store_true",
        )

    def set_up(self) -> None:
        # Initialize if necessary gnatcov traces directory
        if self.main.args.gnatcov:
            self.env.gnatcov_dir = os.path.join(
                os.path.abspath(self.output_dir), "gnatcov-traces"
            )
            self.env.source_root = self.main.args.source_root
            rm(self.env.gnatcov_dir, recursive=True)
            mkdir(self.env.gnatcov_dir)
        else:
            self.env.gnatcov_dir = None

        # Whether valgrind should be used or not
        self.env.valgrind = self.main.args.valgrind

        # Pass some global parameters
        self.env.default_project_file = self.default_project_file
        self.env.default_withed_projects = self.default_withed_projects
        self.env.default_source_dirs = self.default_source_dirs

    def tear_down(self) -> None:
        if self.env.gnatcov_dir:
            produce_report(self, self.output_dir, self.env.source_root)
        super().tear_down()

    @classmethod
    def main(cls, testsuite_root_dir: str) -> None:
        testsuite = cls(testsuite_root_dir)
        sys.exit(testsuite.testsuite_main())
