from . import bin_check_call
import os

def check_call_valgrind(driver, cmd, slot, test_name=None, result=None, **kwargs):
    """
    Wrapper for `e3.testsuite.process` that runs the process under Valgrind if
    this is a Valgrind-checked testsuite run. The process exit status will be
    2 if Valgrind finds memory issues.
    """

    if driver.env.valgrind:
        cmd = [
            "valgrind",
            "-q",
            "--error-exitcode=2",
            "--leak-check=full",
            "--suppressions="
            + os.path.join(os.path.dirname(os.path.abspath(__file__)), "valgrind.supp"),
        ] + cmd
    return bin_check_call(driver, cmd, slot, test_name, result, **kwargs)
