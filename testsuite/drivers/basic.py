import os

from e3.sys import interpreter
from e3.fs import cp
from e3.os.fs import df
from e3.testsuite.driver.classic import ClassicTestDriver
from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.process import check_call
from drivers import gprbuild, run_test_program


class BasicTestDriver(ClassicTestDriver):
    """Default GNATcoll testsuite driver.

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
    6- If you need to do some operation before compiling and running the test
       just create Python file called pre_test.py that will be executed in the
       test working dir.
    7- If you need to do some operation after running the test just create 
       Python file called post_test.py that will be executed in the
       test working dir.
    """

    # We want to copy only specific files (files referenced by the "data" key
    # in test.yaml).
    copy_test_directory = False

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator({
            'env': self.env,
            'test_env': self.test_env,
            'disk_space': lambda: df(self.env.working_dir)
        })

    def run(self):
        # Build the test program
        if self.test_env.get('no-coverage'):
            gpr_project_path = self.env.gnatcoll_debug_gpr_dir
        else:
            gpr_project_path = self.env.gnatcoll_gpr_dir
        gprbuild(self, gpr_project_path=gpr_project_path)

        # Copy the requested data files
        copy_files_on_target = []

        for data in self.test_env.get('data', []):
            cp(os.path.join(self.test_env['test_dir'], data),
               self.test_env['working_dir'], recursive=True)
            copy_files_on_target.append(os.path.join(self.test_env['working_dir'], data))

        pre_test_py = os.path.join(self.test_env['test_dir'], 'pre_test.py')
        if os.path.isfile(pre_test_py):
            check_call(self, [interpreter(), pre_test_py],
                       cwd=self.test_env['working_dir'],
                       timeout=self.default_process_timeout)

        # Run the test program
        test_exe = self.test_env.get('test_exe', 'obj/test')
        process = run_test_program(
            self,
            [os.path.join(self.test_env['working_dir'], test_exe)],
            slot=self.slot,
            copy_files_on_target=copy_files_on_target,
            timeout=self.default_process_timeout)
        self.output += process.out.decode('utf-8')

        post_test_py = os.path.join(self.test_env['test_dir'], 'post_test.py')
        if os.path.isfile(post_test_py):
            check_call(self, [interpreter(), post_test_py],
                       cwd=self.test_env['working_dir'],
                       timeout=self.default_process_timeout)

    def compute_failures(self):
        return (['Success marker not found']
                if '<=== TEST PASSED ===>' not in self.result.log.log else [])
