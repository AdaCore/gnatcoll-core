import os
from stat import *
import sys
import filecmp
import difflib


def compare_test_stdout(expected_out_file_name):
    results_f = open("test_results.out", "r")
    expected_f = open(expected_out_file_name, "r")

    res_content = results_f.read()
    exp_content = expected_f.read()

    if exp_content != res_content:
        print("Expected output is different from the one obtained from test:")
        diff = list(
            difflib.Differ().compare(res_content.splitlines(), exp_content.splitlines())
        )
        diff = "\n".join(diff)
        print(diff)
        return False
    return True


def report_recursive(dcmp, shallow, common_only):
    ret = True
    for name in dcmp.common_files:
        equal = filecmp.cmp(
            os.path.join(dcmp.left, name),
            os.path.join(dcmp.right, name),
            shallow=shallow,
        )
        if not equal:
            print("DIFF file %s found in %s and %s" % (name, dcmp.left, dcmp.right))
            ret = False

    for name in dcmp.common_funny:
        diff_file_nature = True
        try:
            st = os.stat(os.path.join(dcmp.left, name))
            st = os.stat(os.path.join(dcmp.right, name))
        except OSError:
            # Issue opening the file, not on file nature itself. For example:
            # symbolic link with wrong target;
            diff_file_nature = False

        if diff_file_nature:
            print(
                "DIFF file nature %s found in %s and %s" % (name, dcmp.left, dcmp.right)
            )
            ret = False

    if not common_only:
        for name in dcmp.left_only:
            print("ONLY LEFT file %s found in %s" % (name, dcmp.left))
            ret = False
        for name in dcmp.right_only:
            print("ONLY RIGHT file %s found in %s" % (name, dcmp.right))
            ret = False

    for sub_dcmp in dcmp.subdirs.values():
        if report_recursive(sub_dcmp, shallow, common_only) == False:
            ret = False
    return ret


def compare_directories(first_dir, second_dir, shallow=False, common_only=False):
    c = filecmp.dircmp(first_dir, second_dir)

    return report_recursive(c, shallow, common_only)
