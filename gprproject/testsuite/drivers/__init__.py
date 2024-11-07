import os
import re
import subprocess

from e3.fs import mkdir
from e3.os.process import Run, get_rlimit
from e3.testsuite import TestAbort
from e3.testsuite.driver import TestDriver
from e3.testsuite.driver.classic import (
    ClassicTestDriver,
    ProcessResult,
    TestAbortWithFailure,
)
from e3.testsuite.process import check_call
from e3.testsuite.result import Log, TestStatus
from random import getrandbits

DEFAULT_TIMEOUT = 5 * 60  # 5 minutes


def gprbuild(
    driver,
    project_file,
    cwd=None,
    scenario=None,
    gpr_project_path: list[str] | None = None,
    timeout=DEFAULT_TIMEOUT,
    **kwargs,
) -> None:
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
    """
    if scenario is None:
        scenario = {}

    scenario_cmd = []
    for k, v in scenario.items():
        scenario_cmd.append("-X%s=%s" % (k, v))

    if cwd is None:
        cwd = driver.test_env["working_dir"]
    mkdir(cwd)

    # Adjust process environment
    if gpr_project_path:
        new_gpr_project_path = os.path.pathsep.join(gpr_project_path)
        if "GPR_PROJECT_PATH" in os.environ:
            new_gpr_project_path += os.path.pathsep + os.environ["GPR_PROJECT_PATH"]
        kwargs["env"] = kwargs.get("env", {}).update(
            {"GPR_PROJECT_PATH", new_gpr_project_path}
        )
        kwargs["ignore_env"] = False

    if driver.env.gnatcov_dir:
        # In gnatcov mode instrument the project
        gnatcov_cmd = [
            "gnatcov",
            "instrument",
            "--level",
            "stmt+decision",
            "--relocate-build-tree",
            "--dump-trigger=atexit",
            "--projects",
            driver.env.default_withed_projects[0],
            "--restricted-to-languages=Ada",
            "--full-slugs",
            "--externally-built-projects",
            "-XEXTERNALLY_BUILT=true",
            "--no-subprojects",
            "-P",
            project_file,
        ] + scenario_cmd
        check_call(driver, gnatcov_cmd, cwd=cwd, timeout=timeout, **kwargs)

    # Determine the gprbuild command line
    gprbuild_cmd = [
        "gprbuild",
        "--relocate-build-tree",
        "-p",
        "-P",
        project_file,
    ] + scenario_cmd

    if driver.env.gnatcov_dir:
        gprbuild_cmd += [
            "-margs",
            "-v",
            "-g",
            "-O0",
            "--src-subdirs=gnatcov-instr",
            "--implicit-with=gnatcov_rts.gpr",
            "-XLIBRARY_TYPE=static",
            "-XEXTERNALLY_BUILT=true",
        ]

    if driver.env.is_cross:
        gprbuild_cmd.append(
            "--target={target}".format(target=driver.env.target.triplet)
        )

    check_call(
        driver,
        gprbuild_cmd,
        cwd=cwd,
        timeout=timeout,
        **kwargs,
    )


def bin_check_call(
    driver,
    cmd,
    slot,
    test_name=None,
    result=None,
    timeout=None,
    env=None,
    cwd=None,
    copy_files_on_target=None,
    **kwargs,
):
    if cwd is None and "working_dir" in driver.test_env:
        cwd = driver.test_env["working_dir"]
    if result is None:
        result = driver.result
    if test_name is None:
        test_name = driver.test_name

    if driver.env.is_cross:
        if driver.env.target.os.name == "windows":
            os.environ["WINEDEBUG"] = "-all"
            cmd = ["wine"] + cmd
            if timeout is not None:
                cmd = [get_rlimit(), str(timeout)] + cmd

            # Use directly subprocess instead of e3.os.process.Run, since the latter
            # does not handle binary outputs.
            subp = subprocess.Popen(
                cmd,
                cwd=cwd,
                env=env,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
            )
            stdout, _ = subp.communicate()

            # Dismiss some wine message
            stdout = stdout.replace(
                b"it looks like wine32 is missing, you should install it.\n", b""
            )
            stdout = stdout.replace(
                b'as root, please execute "apt-get install wine32"\n', b""
            )
            stdout = re.sub(
                b"....:err:msvcrt:_invalid_parameter (null):0 (null): (null) 0\n",
                b"",
                stdout,
            )
            # stdout here is bytes
            process = ProcessResult(subp.returncode, stdout)
        else:
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
            process = ProcessResult(run.status, run.out.encode("utf-8"))
    else:
        if timeout is not None:
            cmd = [get_rlimit(), str(timeout)] + cmd

        # Use directly subprocess instead of e3.os.process.Run, since the latter
        # does not handle binary outputs.
        subp = subprocess.Popen(
            cmd,
            cwd=cwd,
            env=env,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
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
        out = process.out.decode("utf-8")
    except UnicodeDecodeError:
        out = str(process.out)
    result.log += out

    if process.status != 0:
        if isinstance(driver, ClassicTestDriver):
            raise TestAbortWithFailure("command call fails")
        else:
            result.set_status(TestStatus.FAIL, "command call fails")
            driver.push_result(result)
            raise TestAbort

    return process


def gnatcov_call(driver, cmd, slot, test_name=None, result=None, **kwargs):
    """
    Wrapper for `bin_check_call` that set environment gnatcov trace_file name.
    """
    test_name = test_name or driver.test_env["test_name"]
    trace_file = os.path.join(driver.env.gnatcov_dir, str(getrandbits(128)) + ".strace")

    env = os.environ.copy()
    env["GNATCOV_TRACE_FILE"] = trace_file

    provided_env = kwargs.pop("env", None)
    if provided_env is not None:
        env.update(provided_env)

    return bin_check_call(driver, cmd, slot, test_name, result, env=env, **kwargs)


def check_call_valgrind(driver, cmd, slot, test_name=None, result=None, **kwargs):
    """
    Wrapper for `e3.testsuite.process` that runs the process under Valgrind if
    this is a Valgrind-checked testsuite run. The process exit status will be
    2 if Valgrind finds memory issues.
    """
    cmd = ["valgrind", "-q", "--error-exitcode=2", "--leak-check=full"] + cmd
    return bin_check_call(driver, cmd, slot, test_name, result, **kwargs)


def run_test_program(driver, cmd, slot, test_name=None, result=None, **kwargs):
    """
    Run a test program. This dispatches to running it under Valgrind or
    "gnatcov run", depending on the testsuite options.
    """
    from .gnatcov import gnatcov_call
    from .valgrind import check_call_valgrind

    if driver.env.valgrind:
        wrapper = check_call_valgrind
    elif driver.env.gnatcov_dir:
        wrapper = gnatcov_call
    else:
        wrapper = bin_check_call

    return wrapper(driver, cmd, slot, test_name, result, **kwargs)
