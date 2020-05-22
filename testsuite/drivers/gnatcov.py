import logging
import os

from e3.os.process import Run

from drivers import bin_check_call


COVERAGE_LEVEL = 'stmt+decision'


def list_to_file(str_list, filename):
    """Write a list of strings to a text file.

    :param list[str] str_list: List of strings to write.
    :param str filename: Name of the destination file.
    """
    with open(filename, 'w') as f:
        for item in str_list:
            f.write(item + '\n')


def gnatcov_run(driver, cmd, test_name=None, result=None, **kwargs):
    """
    Wrapper for `bin_check_call` that runs the process under
    "gnatcov run" and that produces a checkpoint in
    `driver.env.checkpoints_dir` for the corresponding partial coverage report.
    """
    test_name = test_name or driver.test_env['test_name']
    trace_file = os.path.join(driver.test_env['working_dir'],
                              '{}.trace'.format(test_name))
    checkpoint_file = os.path.join(driver.env.checkpoints_dir,
                                   '{}.ckpt'.format(test_name))

    cmd = ['gnatcov', 'run', '-o', trace_file, '-eargs'] + cmd
    result = bin_check_call(driver, cmd, test_name, result, **kwargs)

    p = Run(['gnatcov', 'coverage',
             '--level={}'.format(COVERAGE_LEVEL),
             '--scos=@{}'.format(driver.env.ali_files_list),
             '--save-checkpoint={}'.format(checkpoint_file),
             trace_file])
    if p.status:
        logging.error('converting gnatcov trace file to checkpoint failed:\n'
                      '{}'.format(p.out))

    return result


def produce_report(output_dir, checkpoint_list, src_dir):
    """Produce a coverage reports.

    :param str output_dir: Name of the directory to contain the DHTML coverage
        report.
    :param str checkpoint_list: Name of the file that contains the list of
        checkpoints to use.
    :param str src_dir: Name of the directory that contains installed sources.
    """
    args = ['gnatcov', 'coverage', '--annotate=dhtml',
            '--level={}'.format(COVERAGE_LEVEL),
            '--output-dir={}'.format(output_dir),
            '--checkpoint=@{}'.format(checkpoint_list),

            # TODO: GNATcoverage is not be able to find the source file for a
            # unit that is not used by any test program. This is a problem for
            # units that are not tested at all. Let it know where to find the
            # source file to avoid spurious warnings. Note that these units are
            # reported as uncovered in any case.
            '--source-search={}'.format(src_dir)]
    p = Run(args, output=None)
    if p.status:
        logging.error('could not produce the coverage report:\n'
                      '{}'.format(p.out))
    elif p.out:
        logging.info('output of "gnatcov coverage" is not empty:\n'
                     '{}'.format(p.out))
