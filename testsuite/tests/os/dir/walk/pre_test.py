from e3.fs import mkdir
from e3.os.fs import touch
import os

mkdir("test1")
for f in range(1000):
    touch(os.path.join("test1", str(f"file-{f}")))
for f in range(1000):
    mkdir(os.path.join("test1", str(f"dir-{f}")))
    for g in range(100):
        mkdir(os.path.join("test1", str(f"dir-{f}"), str(f"dir-{g}")))
