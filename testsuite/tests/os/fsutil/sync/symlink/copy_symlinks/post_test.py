import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../"))
sys.path.insert(1, os.path.join(sys.path[0], "../../"))
from test_sorting import sort_test_stdout
from post_test_functions import compare_directories, compare_test_stdout
from settings import src_top_dir_name, dst_top_dir_name

failed = False

sort_test_stdout()

if not compare_test_stdout("expected.out"):
    print("Test output is incorrect")
    failed = True

if not failed and not compare_directories(src_top_dir_name, dst_top_dir_name):
    print("Directories should be similar.")
    failed = True

if failed:
    print("Test assertion passed, but post-check failed.")
    sys.exit(1)
