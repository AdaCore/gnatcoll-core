from e3.fs import mkdir
from e3.os.fs import touch
import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], ".."))
from settings import src_top_dir_name, dst_top_dir_name

mkdir(src_top_dir_name)
for f in range(3):
    touch(os.path.join(src_top_dir_name, str(f"file-{f}")))
for f in range(3):
    mkdir(os.path.join(src_top_dir_name, str(f"dir-{f}")))
    for g in range(3):
        mkdir(os.path.join(src_top_dir_name, str(f"dir-{f}"), str(f"dir-{g}")))

mkdir(dst_top_dir_name)
