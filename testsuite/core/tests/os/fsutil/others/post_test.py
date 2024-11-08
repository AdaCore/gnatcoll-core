#!/usr/bin/env python

import sys
import filecmp

if not filecmp.cmp("huge_file", "huge_file_copy", shallow=False):
    print("Test is OK, but post-test script failed. " +
          "Files do not have the same content.")
    sys.exit(1)
