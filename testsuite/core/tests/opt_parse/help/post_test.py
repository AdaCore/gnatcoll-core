#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    # Don't try to precisely match the output. This makes the test more robust
    assert "usage: test" in content
    assert "--help" in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
