import os

from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.driver.diff import DiffTestDriver, OutputRefiner, Substitute

from drivers import gprbuild, run_test_program


class ToLower(OutputRefiner):
    """Output refiner to switch to lower case."""

    def refine(self, output):
        return output.lower()


class BuildRunDiffDriver(DiffTestDriver):
    """Build and run a project using GNATCOLL.

    Put project and source files in the test directory, in particular
    "test.gpr" in the root directory, whose compilation is supposed to create a
    "test" executable in the same directory.

    This test driver builds the "test.gpr" project file and then executes the
    "test" shell script (test.sh). This script should run the executable that
    was built. It can either pass through all output to stdout, or stdout can
    be piped to files to allow for post processesing first. The test succeeds
    if all of the following items are true:

    * the compilation is successful;
    * the "test" program completes with status code 0.
    * the contents of test.out/regex_test.out match the stdout of the test.sh
    """

    @property
    def baseline(self):
        # Allow a missing test.out or regex_test.out -- treat as empty
        test_out = self.test_dir("test.out")
        regex_test_out = self.test_dir("regex_test.out")
        regex = False
        if os.path.exists(test_out):
            with open(test_out, encoding=self.default_encoding) as f:
                baseline = f.read()
        elif os.path.exists(regex_test_out):
            with open(regex_test_out, encoding=self.default_encoding) as f:
                baseline = f.read()
            regex = True
        else:
            baseline = ""
            test_out = None

        return (test_out, baseline, regex)

    @property
    def output_refiners(self):
        result = super().output_refiners
        if self.test_env.get("fold_casing", False):
            result.append(ToLower())
        if self.test_env.get("canonicalize_backslashes", False):
            result.append(Substitute("\\", "/"))
        return result

    def run(self):
        # Build the test project
        if self.test_env.get('no-coverage'):
            gpr_project_path = self.env.gnatcoll_debug_gpr_dir
        else:
            gpr_project_path = self.env.gnatcoll_gpr_dir
        gprbuild(
            self,
            project_file="test.gpr",
            gpr_project_path=gpr_project_path
        )

        # Run the test program
        if self.env.is_cross:
            p = run_test_program(
                self,
                [self.working_dir("test")],
                self.slot,
                timeout=self.default_process_timeout
            )
        else:
            p = run_test_program(
                self,
                ["bash", self.working_dir("test.sh")],
                self.slot,
                timeout=self.default_process_timeout,
            )
            # Output explicitly to be compared with the expected output.
            self.output += p.out.decode('utf-8')

        if p.status:
            self.output += ">>>program returned status code {}\n".format(
                p.status
            )
