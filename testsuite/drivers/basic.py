from e3.fs import cp
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus
from drivers import gprbuild
import os


class BasicTestDriver(TestDriver):
    """Default GNATcoll testsuite driver.

    In order to declare a test:

    1- Create a directory with a test.yaml inside
    2- Add test sources in that directory
    3- Add a main called test.adb that use support/test_assert.ads package.
    """

    def add_test(self, dag):
        """Declare test workflow.

        The workflow is the following::

            build --> check status

        :param dag: tree of test fragment to amend
        :type dag: e3.collection.dag.DAG
        """
        self.add_fragment(dag, 'build')
        self.add_fragment(dag, 'check_run', after=['build'])

        if 'test_exe' not in self.test_env:
            self.test_env['test_exe'] = 'obj/test'

    def build(self, previous_values):
        """Build fragment."""
        return gprbuild(self, gcov=self.env.gcov)

    def check_run(self, previous_values):
        """Check status fragment."""
        if not previous_values['build']:
            return

        for data in self.test_env.get('data', []):
            cp(os.path.join(self.test_env['test_dir'], data),
               self.test_env['working_dir'], recursive=True)

        process = check_call(self, [self.test_env['test_exe']])
        if '<=== TEST PASSED ===>' not in process.out:
            self.result.set_status(TestStatus.FAIL)
        else:
            self.result.set_status(TestStatus.PASS)
        self.push_result()
