from e3.fs import mkdir
from e3.os.fs import touch
import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../../.."))
from settings import src_top_dir_name, dst_top_dir_name

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
for f in range(3):
    fd = open(os.path.join(src_top_dir_name, str(f"file-{f}")), "w")
    fd.write("Same length, but different content for file n." + str(f))
    fd.close()
    fd = open(os.path.join(dst_top_dir_name, str(f"file-{f}")), "w")
    fd.write("Different content, but same length for file n." + str(f))
    fd.close()

    # Set the timestamps
    stats = os.stat(os.path.join(src_top_dir_name, str(f"file-{f}")))
    time_info = (stats.st_atime, stats.st_mtime)
    os.utime(os.path.join(src_top_dir_name, str(f"file-{f}")), time_info)
    os.utime(os.path.join(dst_top_dir_name, str(f"file-{f}")), time_info)


for f in range(3):
    mkdir(os.path.join(src_top_dir_name, str(f"dir-{f}")))
    mkdir(os.path.join(dst_top_dir_name, str(f"dir-{f}")))
    for g in range(3):
        fd = open(
            os.path.join(src_top_dir_name, str(f"dir-{f}"), str(f"file-{g}")), "w"
        )
        fd.write("Same length, but different content for file n." + str(g))
        fd.close()
        fd = open(
            os.path.join(dst_top_dir_name, str(f"dir-{f}"), str(f"file-{g}")), "w"
        )
        fd.write("Different content, but same length for file n." + str(g))
        fd.close()

        # Set the timestamps
        stats = os.stat(
            os.path.join(src_top_dir_name, str(f"dir-{f}"), str(f"file-{g}"))
        )
        time_info = (stats.st_atime, stats.st_mtime)
        os.utime(
            os.path.join(src_top_dir_name, str(f"dir-{f}"), str(f"file-{g}")), time_info
        )
        os.utime(
            os.path.join(dst_top_dir_name, str(f"dir-{f}"), str(f"file-{g}")), time_info
        )
