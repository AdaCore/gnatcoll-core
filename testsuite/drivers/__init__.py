from e3.env import Env
from e3.fs import mkdir
from e3.os.process import Run
from e3.testsuite.process import check_call
import os
import logging


TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))
GNATCOLL_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)


def make_gnatcoll_for_gcov(work_dir):
    """Build gnatcoll core with gcov instrumentation.

    :param work_dir: working directory. gnatcoll is built in `build` subdir
        and installed in `install` subdir
    :type work_dir: str
    :return: a triplet (project path, source path, object path)
    :rtype: (str, str, str)
    :raise AssertError: in case compilation of installation fails
    """
    logging.info('Compiling gnatcoll with gcov instrumentation')
    build_dir = os.path.join(work_dir, 'build')
    install_dir = os.path.join(work_dir, 'install')
    mkdir(build_dir)
    mkdir(install_dir)

    make_gnatcoll_cmd = [
        'make', '-f', os.path.join(GNATCOLL_ROOT_DIR, 'Makefile'),
        'BUILD=DEBUG',
        'GPRBUILD_OPTIONS=-cargs -fprofile-arcs -ftest-coverage -gargs',
        'ENABLE_SHARED=no']

    p = Run(make_gnatcoll_cmd, cwd=build_dir)
    assert p.status == 0, "gnatcoll build failed:\n%s" % p.out

    p = Run(make_gnatcoll_cmd + ['prefix=%s' % install_dir, 'install'],
            cwd=build_dir)
    assert p.status == 0, "gnatcoll installation failed:\n%s" % p.out

    # Add the resulting library into the GPR path
    Env().add_search_path('GPR_PROJECT_PATH',
                          os.path.join(install_dir, 'share', 'gpr'))
    return (os.path.join(install_dir, 'share', 'gpr'),
            os.path.join(install_dir, 'include', 'gnatcoll'),
            os.path.join(build_dir, 'obj', 'gnatcoll', 'static'))


def gprbuild(driver,
             project_file=None,
             cwd=None,
             gcov=False,
             scenario=None,
             **kwargs):
    """Launch gprbuild.

    :param project_file: project file to compile. If None, we looks first for
        a test.gpr in the test dir and otherwise fallback on the common
        test.gpr project of the support subdir of the testsuite.
    :type project_file: str
    :param cwd: directory in which to run gprbuild. If None the gprbuild build
        is run in the default working dir for the test.
    :type cwd: str | None
    :param gcov: if True link with gcov libraries
    :type gcov: bool
    :param scenario: scenario variable values
    :type scenario: dict
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

    if cwd is None:
        cwd = driver.test_env['working_dir']
    mkdir(cwd)
    gprbuild_cmd = [
        'gprbuild', '--relocate-build-tree', '-p', '-P', project_file]
    for k, v in scenario.iteritems():
        gprbuild_cmd.append('-X%s=%s' % (k, v))
    if gcov:
        gprbuild_cmd += ['-largs', '-lgcov']
    check_call(
        driver,
        gprbuild_cmd,
        cwd=cwd,
        **kwargs)
    # If we get there it means the build succeeded.
    return True
