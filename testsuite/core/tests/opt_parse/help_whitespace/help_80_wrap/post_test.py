#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read().splitlines()
    for line in content:
        assert len(line) <= 80, f"line too long:{line}"
    print("<=== TEST PASSED ===>")
    sys.exit(0)
