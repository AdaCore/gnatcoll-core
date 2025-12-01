import os

from e3.fs import cp
from e3.testsuite.driver.classic import TestAbortWithFailure
from e3.testsuite.driver.diff import DiffTestDriver, OutputRefiner, Substitute
from e3.sys import interpreter

from drivers import gprbuild, run_test_program


class ToLower(OutputRefiner):
    """Output refiner to switch to lower case."""

    def refine(self, output):
        return output.lower()


class BuildRunAssertAndDiffDriver(DiffTestDriver):
    """Build and run a project using GNATCOLL.

    This driver supports both assertions and test output checking.

    In order to declare a test:

    1- Create a directory with a test.yaml inside
    2- Add test sources in that directory
    3- Add a main called test.adb that use support/test_assert.ads package.
    4- Do not put test.gpr there, it breaks the test, if you need a project
       file for testing, name it something else.
    5- If you need additional files for you test, list them in test.yaml:
       data:
           - "your_file1"
           - "your_file2"
           - "your_dir1"
    6- If you need to do some operation before compiling and running the test
       just create Python file called pre_test.py that will be executed in the
       test working dir.
    7- If you need to do some operation after running the test just create
       Python file called post_test.py that will be executed in the
       test working dir.
    8- Add the expected output in test.out or regex_test.out.

    Note that in cases of cross-testing, pre- and post-operations will not be
    done from the target environment but only from the host. However, files
    specified in the "data" field in test.yaml will be copied to the target.
    It is then possible to create files from a pre_test.py script and make them
    usable by the cross-test.

    The test succeeds if all of the following items are true:

    * the compilation is successful;
    * all assertions in the code pass;
    * the contents of test.out/regex_test.out match the test stdout
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
        if self.test_env.get("no-coverage"):
            gpr_project_path = self.env.gnatcoll_debug_gpr_dir
        else:
            gpr_project_path = None

        scenario = {}
        for source_dir in self.test_env.get("source_dirs", []):
            # Ensure that relative path given in the YAML are relative from the
            # test source directory, and not from the default testsuite project
            # file directory.
            if not os.path.isabs(source_dir):
                source_dir = os.path.join(self.test_env["test_dir"], source_dir)

            if "TEST_SOURCES" in scenario:
                scenario["TEST_SOURCES"] += "," + source_dir
            else:
                scenario["TEST_SOURCES"] = source_dir

        # We do not specify a gpr project file, so the testsuite's default one
        # is used.
        gprbuild(self, scenario=scenario, gpr_project_path=gpr_project_path)

        pre_test_py = os.path.join(self.test_env["test_dir"], "pre_test.py")
        if os.path.isfile(pre_test_py):
            self.shell([interpreter(), pre_test_py])

        # Copy the requested data files
        copy_files_on_target = []

        for data in self.test_env.get("data", []):
            if os.path.exists(os.path.join(self.test_env["test_dir"], data)):
                cp(
                    os.path.join(self.test_env["test_dir"], data),
                    self.test_env["working_dir"],
                    recursive=True,
                )

            # Pre-test script may have produced data. If it not the case,
            # raise an exception.
            elif not os.path.exists(os.path.join(self.test_env["working_dir"], data)):
                raise TestAbortWithFailure(
                    data
                    + " not found nor in the test directory "
                    + self.test_env["test_dir"]
                    + " neither in the working directory "
                    + self.test_env["working_dir"]
                )

            copy_files_on_target.append(
                os.path.join(self.test_env["working_dir"], data)
            )

        # Run the test program
        test_exe = self.test_env.get("test_exe", "obj/test")
        process = run_test_program(
            self,
            [os.path.join(self.test_env["working_dir"], test_exe)],
            self.slot,
            copy_files_on_target=copy_files_on_target,
            timeout=self.default_process_timeout,
        )

        # Output explicitly to be compared with the expected output.
        self.output += process.out.decode("utf-8")

        if process.status:
            self.output += ">>>program returned status code {}\n".format(process.status)

        post_test_py = os.path.join(self.test_env["test_dir"], "post_test.py")
        if os.path.isfile(post_test_py):
            self.shell([interpreter(), post_test_py])
