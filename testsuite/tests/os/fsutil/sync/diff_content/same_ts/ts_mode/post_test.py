import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../../.."))
from post_test_functions import compare_directories
from settings import src_top_dir_name, dst_top_dir_name

# We expect different src and dst trees, as timestamp are the same, and
# files are compared based on the latter.
if compare_directories(src_top_dir_name, dst_top_dir_name):
    print(
        "Test passed, but post-test check failed. Directory should not be "
        & "similar as timestamps and file size are the same, in contrary to "
        & " their content."
    )
    sys.exit(1)
