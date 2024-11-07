import glob
import logging
import os
from random import getrandbits

from e3.os.process import Run

from . import bin_check_call


COVERAGE_LEVEL = "stmt+decision"


def list_to_file(str_list, filename):
    """Write a list of strings to a text file.

    :param list[str] str_list: List of strings to write.
    :param str filename: Name of the destination file.
    """
    with open(filename, "w") as f:
        for item in str_list:
            f.write(item + "\n")


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


def ensure_clean_dir(dirname):
    """
    If it exists, remove the ``dirname`` directory tree and create an empty
    directory instead.
    """
    if os.path.exists(dirname):
        shutil.rmtree(dirname)
    os.mkdir(dirname)


def produce_report(
    driver, output_dir, source_root=None, formats=["dhtml", "xml", "cobertura"]
):
    "Produce a coverage reports."

    main_project = driver.env.default_withed_projects[0] + ".gpr"
    traces_list = os.path.join(driver.env.gnatcov_dir, "traces.txt")
    with open(traces_list, "w") as fp:
        for t in glob.glob(os.path.join(driver.env.gnatcov_dir, "*.strace")):
            fp.write(f"{t}\n")
    # Produce a checkpoint from them
    checkpoint_file = os.path.join(driver.env.gnatcov_dir, "report.ckpt")
    args = [
        "gnatcov",
        "coverage",
        "--level",
        COVERAGE_LEVEL,
        "-P",
        main_project,
        "--externally-built-projects",
        "--save-checkpoint",
        checkpoint_file,
        f"@{traces_list}",
    ]
    if source_root:
        args.append("--source-root=" + source_root)
    p = Run(args)
    if p.status:
        logging.error(
            f"error creating gnatcov checkpoint:\n$ {' '.join(args)}\n{p.out}"
        )
        return

    # Finally produce the reports
    for fmt in formats:
        report_dir = os.path.join(output_dir, "coverage-" + fmt)
        ensure_clean_dir(report_dir)
        path_opt = []
        args = [
            "gnatcov",
            "coverage",
            "--annotate",
            fmt,
            "--level={}".format(COVERAGE_LEVEL),
            "--output-dir",
            report_dir,
            "-P",
            main_project,
            "--externally-built-projects",
            "--checkpoint",
            checkpoint_file,
        ]
        if source_root:
            args.append("--source-root=" + source_root)
        p = Run(args, output=None)
        if p.status:
            logging.error("could not produce the coverage report:\n" "{}".format(p.out))
        else:
            logging.info(fmt + " coverage report produced in " + report_dir)
