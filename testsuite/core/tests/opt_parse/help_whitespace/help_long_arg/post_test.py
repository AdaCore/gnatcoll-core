#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    # Opposite to help_short_80. There should be more than 20 spaces, which
    # was the previous limit after a single character arg like "-c" and a
    # fixed 25 character column
    assert " " * 20 in content, "at least 20 spaces expected"
    print("<=== TEST PASSED ===>")
    sys.exit(0)
