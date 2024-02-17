from e3.fs import mkdir
from e3.os.fs import touch
import os
import time
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../.."))
from settings import src_top_dir_name, dst_top_dir_name

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
for f in range(3):
    touch(os.path.join(src_top_dir_name, str(f"file-{f}")))
    time.sleep(0.01)
    fd = open(os.path.join(dst_top_dir_name, str(f"file-{f}")), "w")
    fd.write("More content for file n." + str(f))
    fd.close()

for f in range(3):
    mkdir(os.path.join(src_top_dir_name, str(f"dir-{f}")))
    mkdir(os.path.join(dst_top_dir_name, str(f"dir-{f}")))
    for g in range(3):
        fd = open(
            os.path.join(src_top_dir_name, str(f"dir-{f}"), str(f"file-{g}")), "w"
        )
        fd.write("Also more content for file n." + str(g))
        fd.close()
        time.sleep(0.01)
        touch(os.path.join(dst_top_dir_name, str(f"dir-{f}"), str(f"file-{g}")))
