import logging
import os
import subprocess

from e3.fs import mkdir
from e3.os.process import Run, get_rlimit
from e3.testsuite import TestAbort
from e3.testsuite.driver import TestDriver
from e3.testsuite.driver.classic import (
    ClassicTestDriver, ProcessResult, TestAbortWithFailure
)
from e3.testsuite.process import check_call
from e3.testsuite.result import Log, TestStatus

# Root directory of respectively the testsuite and the gnatcoll
# repository.
TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))
GNATCOLL_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)

DEFAULT_TIMEOUT = 5 * 60  # 5 minutes


def make_gnatcoll(work_dir, debug=False):
    """Build gnatcoll core. 

    :param str work_dir: Working directory. GNATcoll is built in `build` subdir
        and installed in `install` subdir.

    :param bool debug: Whether to build GNATCOLL in debug mode. Otherwise, use
        the prod mode.

    :return: A triplet (project path, source path, object path).
    :rtype: (str, str, str)
    :raise AssertError: In case compilation of installation fails.
    """
    logging.info('Compiling gnatcoll')

    # Create build tree structure
    build_dir = os.path.join(work_dir, 'build')
    install_dir = os.path.join(work_dir, 'install')
    mkdir(build_dir)
    mkdir(install_dir)

    # Compute make invocation
    make_gnatcoll_cmd = [
        'make', '-f', os.path.join(GNATCOLL_ROOT_DIR, 'Makefile'),
        'ENABLE_SHARED=no',
        'BUILD={}'.format('DEBUG' if debug else 'PROD')]

    # Build & Install
    p = Run(make_gnatcoll_cmd, cwd=build_dir, timeout=DEFAULT_TIMEOUT)
    assert p.status == 0, "gnatcoll build failed:\n%s" % p.out

    p = Run(make_gnatcoll_cmd + ['prefix=%s' % install_dir, 'install'],
            cwd=build_dir, timeout=DEFAULT_TIMEOUT)
    assert p.status == 0, "gnatcoll installation failed:\n%s" % p.out

    return (os.path.join(install_dir, 'share', 'gpr'),
            os.path.join(install_dir, 'include', 'gnatcoll'),
            os.path.join(build_dir, 'obj', 'gnatcoll', 'static'))


def gprbuild(driver,
             project_file=None,
             cwd=None,
             scenario=None,
             gpr_project_path=None,
             timeout=DEFAULT_TIMEOUT,
             **kwargs):
    """Launch gprbuild.

    :param project_file: project file to compile. If None, we looks first for
        a test.gpr in the test dir and otherwise fallback on the common
        test.gpr project of the support subdir of the testsuite.
    :type project_file: str
    :param cwd: directory in which to run gprbuild. If None the gprbuild build
        is run in the default working dir for the test.
    :type cwd: str | None
    :param scenario: scenario variable values
    :type scenario: dict
    :param gpr_project_path: if not None prepent this value to GPR_PROJECT_PATH
    :type gpr_project_path: None | str
    :param kwargs: additional keyword arguements are passed to
        e3.testsuite.process.check_call function
    :return: True on successful completion
    :rtype: bool
    """
    if scenario is None:
        scenario = {}

    if project_file is None:
        project_file = os.path.join(driver.test_env['test_dir'],
                                    'test.gpr')
        if not os.path.isfile(project_file):
            project_file = os.path.join(TESTSUITE_ROOT_DIR,
                                        'support', 'test.gpr')
            scenario['TEST_SOURCES'] = driver.test_env['test_dir']

    scenario_cmd = []
    for k, v in scenario.items():
        scenario_cmd.append('-X%s=%s' % (k, v))

    if cwd is None:
        cwd = driver.test_env['working_dir']
    mkdir(cwd)

    # Adjust process environment
    env = kwargs.pop('env', None)
    ignore_environ = kwargs.pop('ignore_environ', True)
    if env is None:
        env = {}
        ignore_environ = False
    if gpr_project_path:
        new_gpr_path = gpr_project_path
        if 'GPR_PROJECT_PATH' in os.environ:
            new_gpr_path += os.path.pathsep + os.environ['GPR_PROJECT_PATH']
        env['GPR_PROJECT_PATH'] = new_gpr_path

    # Determine the gprbuild command line
    gprbuild_cmd = [
        'gprbuild', '--relocate-build-tree', '-p', '-P', project_file
    ] + scenario_cmd

    if driver.env.gnatcov:
        from drivers.gnatcov import COVERAGE_LEVEL

        gnatcov_cmd = [
            "gnatcov", "instrument", "--level", COVERAGE_LEVEL,
            "--relocate-build-tree",
            "--dump-trigger=atexit",
            "--projects", "gnatcoll",
            "--externally-built-projects",
            "-XEXTERNALLY_BUILT=true",
            "--no-subprojects",
            "-P", project_file] + scenario_cmd

        check_call(driver, gnatcov_cmd,
                   cwd=cwd,
                   env=env,
                   ignore_environ=ignore_environ,
                   timeout=timeout,
                   **kwargs)
        gprbuild_cmd += [
            "-margs", "-v", "-g", "-O0", "--src-subdirs=gnatcov-instr", "--implicit-with=gnatcov_rts", "-XLIBRARY_TYPE=static", "-XEXTERNALLY_BUILT=true"]

    if driver.env.is_cross:
        gprbuild_cmd.append("--target={target}".format(target=driver.env.target.triplet))

    check_call(
        driver,
        gprbuild_cmd,
        cwd=cwd,
        env=env,
        ignore_environ=ignore_environ,
        timeout=timeout,
        **kwargs)
    # If we get there it means the build succeeded.
    return True


def bin_check_call(driver, cmd, slot, test_name=None, result=None, timeout=None,
                   env=None, cwd=None, copy_files_on_target=None, **kwargs):
    if cwd is None and "working_dir" in driver.test_env:
        cwd = driver.test_env["working_dir"]
    if result is None:
        result = driver.result
    if test_name is None:
        test_name = driver.test_name

    if driver.env.is_cross:
        # Import pycross only when necessary for cross targets
        from pycross.runcross.main import run_cross
        run = run_cross(
            cmd,
            cwd=cwd,
            mode=None,
            timeout=timeout,
            output=None,
            slot=slot,
            copy_files_on_target=copy_files_on_target,
        )

        # Here process.out holds utf-8 encoded data.
        process = ProcessResult(run.status, run.out.encode('utf-8'))

    else:
        if timeout is not None:
            cmd = [get_rlimit(), str(timeout)] + cmd

        # Use directly subprocess instead of e3.os.process.Run, since the latter
        # does not handle binary outputs.
        subp = subprocess.Popen(
            cmd, cwd=cwd, env=env, stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT
        )
        stdout, _ = subp.communicate()
        # stdout here is bytes
        process = ProcessResult(subp.returncode, stdout)

    result.processes.append(
        {
            "output": Log(process.out),
            "status": process.status,
            "cmd": cmd,
            "timeout": timeout,
            "env": env,
            "cwd": cwd,
        }
    )

    # Append the status code and process output to the log to ease post-mortem
    # investigation.
    result.log += f"Cmd_Line: {' '.join(cmd)}"
    result.log += "Status code: {}\n".format(process.status)
    result.log += "Output:\n"

    try:
        out = process.out.decode('utf-8')
    except UnicodeDecodeError:
        out = str(process.out)
    result.log += out

    if process.status != 0:
        if isinstance(driver, ClassicTestDriver):
            raise TestAbortWithFailure('command call fails')
        else:
            result.set_status(TestStatus.FAIL, "command call fails")
            driver.push_result(result)
            raise TestAbort

    return process


def run_test_program(driver, cmd, slot, test_name=None, result=None, **kwargs):
    """
    Run a test program. This dispatches to running it under Valgrind or
    "gnatcov run", depending on the testsuite options.
    """
    from drivers.gnatcov import gnatcov_call
    from drivers.valgrind import check_call_valgrind

    if driver.env.valgrind:
        wrapper = check_call_valgrind
    elif driver.env.gnatcov:
        wrapper = gnatcov_call
    else:
        wrapper = bin_check_call

    return wrapper(driver, cmd, slot, test_name, result, **kwargs)


class GNATcollTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    DEFAULT_TIMEOUT = 5 * 60  # 5 minutes

    @property
    def process_timeout(self):
        """Timeout (in seconds) for subprocess to launch."""
        return self.test_env.get('timeout', self.DEFAULT_TIMEOUT)

    def run_test_program(self, cmd, slot, test_name=None, result=None, **kwargs):
        return run_test_program(self, cmd, slot, test_name, result, **kwargs)
