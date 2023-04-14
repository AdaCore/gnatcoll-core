#!/usr/bin/env python
import os

# Sendfile is used by Copy_File in Linux implementation. It has a 2GiB limit.
# A file which weights more than this limit shall work properly, as several
# call to Sendfile will be made.
# Ini
fileSizeInBytes = 2*1024*1024*1024 + 1024*1024
with open('huge_file', 'wb') as fout:
    fout.write(os.urandom(fileSizeInBytes))
