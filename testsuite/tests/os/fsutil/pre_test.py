#!/usr/bin/env python
import os
from e3.os.fs import touch

# Sendfile is used by Copy_File in Linux implementation. It has a 2GiB limit.
# A file which weights more than this limit shall work properly, as several
# call to Sendfile will be made. Moreover, we want also to test 32 bits systems
# with a file whose weight overcomes 32 bits.
fileSizeInBytes = 4*1024*1024*1024 + 1024*1024
with open('huge_file', 'wb') as fout:
    fout.write(os.urandom(fileSizeInBytes))

os.mkdir("already_existing_dir")

os.mkdir("directory_with_files");
touch("directory_with_files/file1");
touch("directory_with_files/file2");
