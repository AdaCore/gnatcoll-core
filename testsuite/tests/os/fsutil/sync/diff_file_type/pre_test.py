from e3.fs import mkdir
from e3.os.fs import touch
import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], ".."))
from settings import src_top_dir_name, dst_top_dir_name

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
for f in range(10):
    touch(os.path.join(src_top_dir_name, str(f"{f}")))
    mkdir(os.path.join(dst_top_dir_name, str(f"{f}")))
