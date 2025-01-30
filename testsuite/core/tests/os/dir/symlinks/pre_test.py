from e3.fs import mkdir
from e3.os.fs import touch
import os

mkdir("test1")
touch(os.path.join("test1", "file1.txt"))
os.symlink("file1.txt", os.path.join("test1", "file2.txt"))
