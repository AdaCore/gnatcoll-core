from e3.fs import mkdir
import os
import sys

sys.path.insert(1, os.path.join(sys.path[0], "../../.."))
from settings import src_top_dir_name, dst_top_dir_name

mkdir(src_top_dir_name)
mkdir(dst_top_dir_name)
for f in range(10):
    fd = open(os.path.join(src_top_dir_name, str(f"file-{f}")), "w")
    fd.write("Same length, but different content for file n." + str(f))
    fd.close()
    fd = open(os.path.join(dst_top_dir_name, str(f"file-{f}")), "w")
    fd.write("Different content, but same length for file n." + str(f))
    fd.close()

for f in range(10):
    mkdir(os.path.join(src_top_dir_name, str(f"dir-{f}")))
    mkdir(os.path.join(dst_top_dir_name, str(f"dir-{f}")))
    for g in range(5):
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

# Sync_Trees large files. Append different content at the end, which will also
# set same timestamps for both files.
fileSizeInBytes = 2 * 1024 * 1024 * 1024 + 1024 * 1024
with open(os.path.join(src_top_dir_name, "huge_file"), "wb") as fout:
    fout.write(os.urandom(fileSizeInBytes))

with open(os.path.join(dst_top_dir_name, "huge_file"), "wb") as fout:
    fout.write(os.urandom(fileSizeInBytes))

with open(os.path.join(src_top_dir_name, "huge_file"), "a") as fout:
    fout.write("OK")

with open(os.path.join(dst_top_dir_name, "huge_file"), "a") as fout:
    fout.write("KO")
