import os

from e3.testsuite.driver.classic import ClassicTestDriver, TestAbortWithFailure

from gprproject.testsuite.drivers import gprbuild, run_test_program


class BuildAndRunDriver(ClassicTestDriver):
    """Build and run a project using GNATCOLL.

    Put project and source files in the test directory, in particular
    "test.gpr" in the root directory, whose compilation is supposed to create a
    "test" executable in the same directory.

    This test driver builds the "test.gpr" project file and then executes the
    "test" executable. The test succeeds if all of the following items are
    true:

    * the compilation is successful;
    * the "test" program completes with status code 0.
    """

    def run(self):
        # Build the test project
        gprbuild(
            self,
            project_file="test.gpr",
        )

        # Run the test program
        process = run_test_program(
            self,
            [self.working_dir("test")],
            self.slot,
            timeout=self.default_process_timeout
        )
        self.output += process.out.decode('utf-8')

        if process.status != 0:
            raise TestAbortWithFailure("test program failed")
