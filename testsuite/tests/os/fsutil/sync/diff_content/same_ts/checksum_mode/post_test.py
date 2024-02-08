import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../../.."))
from post_test_functions import compare_directories
from settings import src_top_dir_name, dst_top_dir_name

if not compare_directories(src_top_dir_name, dst_top_dir_name):
    print("Test passed, but post-test check failed.")
    sys.exit(1)
