from e3.fs import rm
from e3.testsuite.result import TestStatus, TestResult
from e3.testsuite.utils import CleanupMode
from drivers import GNATcollTestDriver, gprbuild
import os


class DataValidationDriver(GNATcollTestDriver):
    """Data validation driver.

    For each test program call the program with data file defined in
    data_files key of the test. If the program returns 0 assume that
    the test passed.
    """

    def add_test(self, dag):
        self.add_fragment(dag, 'build')

        tear_down_deps = []
        for data_file, description in self.test_env['data_files'].items():
            tear_down_deps.append(data_file)
            self.add_fragment(
                dag,
                data_file,
                fun=self.run_subtest_for(data_file, description),
                after=['build'])
        self.add_fragment(dag, 'tear_down', after=tear_down_deps)

    def run_subtest_for(self, data_file, description):

        def run_subtest(previous_values, slot):
            test_name = self.test_name + '.' + data_file
            result = TestResult(test_name, env=self.test_env)

            if not previous_values['build']:
                return TestStatus.FAIL

            process = self.run_test_program(
                [os.path.join(self.test_env['working_dir'],
                              self.test_env.get('validator', 'obj/test')),
                 os.path.join(self.test_env['test_dir'], data_file)],
                slot,
                test_name=test_name,
                result=result,
                timeout=self.process_timeout)

            return self.validate_result(process, data_file, result)

        return run_subtest

    def validate_result(self, process, data_file, result):
        # Test passes as soon as there is this magic string is its output. Note
        # that we push a test result only on failure.
        if b'<=== TEST PASSED ===>' not in process.out:
            result.set_status(TestStatus.FAIL)
            self.push_result(result)
        return True

    def tear_down(self, previous_values, slot):
        # If the build failed in the "build" fragment, we already pushed the
        # result.
        failed = True
        if previous_values.get('build'):
            failures = [v for v in previous_values.values() if
                        not isinstance(v, TestStatus) or v != TestStatus.PASS]
            if failures:
                self.result.set_status(
                    TestStatus.FAIL,
                    msg="%s subtests failed" % len(failures)
                )
            else:
                failed = False
                self.result.set_status(TestStatus.PASS)

            self.push_result()

        # Cleanup temporaries if requested
        if (
            self.env.cleanup_mode == CleanupMode.ALL
            or (self.env.cleanup_mode == CleanupMode.PASSING and not failed)
        ):
            rm(self.test_env['working_dir'], recursive=True)

    def build(self, previous_values, slot):
        return gprbuild(self, gpr_project_path=self.env.gnatcoll_gpr_dir)
