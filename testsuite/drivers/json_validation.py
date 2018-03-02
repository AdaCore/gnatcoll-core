from e3.fs import rm
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus, TestResult
from drivers import gprbuild
import json
import logging
import os


class JSONValidationDriver(TestDriver):
    """JSON validation driver."""

    def add_test(self, dag):
        self.add_fragment(dag, 'build')

        tear_down_deps = []
        for data_file, description in self.test_env['data_files'].iteritems():
            tear_down_deps.append(data_file)
            self.add_fragment(
                dag,
                data_file,
                fun=lambda x, d=data_file, m=description:
                self.run_subtest(d, m, x),
                after=['build'])
        self.add_fragment(dag, 'tear_down', after=tear_down_deps)

    def run_subtest(self, data_file, description, previous_values):
        test_name = self.test_name + '.' + data_file
        result = TestResult(test_name, env=self.test_env)

        if not previous_values['build']:
            result.set_status(
                TestStatus.FAIL,
                "validation program compilation failure")
            self.push_result(result)
            return

        process = check_call(
            self,
            [self.test_env.get('validator', 'obj/test'),
             os.path.join(self.test_env['test_dir'], data_file)],
            result=result)

        # Read data file
        with open(os.path.join(self.test_env['test_dir'], data_file)) as fd:
            expected = json.load(fd)

        got = json.loads(process.out)
        if got != expected:
            logging.debug('%s\n<=>\n%s', got, expected)
            result.set_status(TestStatus.FAIL)
        else:
            result.set_status(TestStatus.PASS)
        self.push_result(result)

    def tear_down(self, previous_values):
        rm(self.test_env['working_dir'], recursive=True)

    def build(self, previous_values):
        return gprbuild(self, gcov=self.env.gcov)
