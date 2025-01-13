#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    # The biggest whitespace should be the three characters before optional
    # arguments as "--help, -h" and "--char, -C" are the same lenght, and
    # while they are less than 25 characters, the column number the help
    # text starts at should be limited to 2 after the longest arg text.
    assert "    " not in content, "no more than 3 consecutive whitespaces expected"
    print("<=== TEST PASSED ===>")
    sys.exit(0)
