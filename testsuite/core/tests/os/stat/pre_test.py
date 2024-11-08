#!/usr/bin/env python
import e3.fs
import sys
import os

e3.fs.mkdir("directory")
e3.os.fs.touch("regular_file")
if sys.platform != "win32":
    os.symlink("regular_file", "sym_link")
e3.os.fs.touch("é.txt")

