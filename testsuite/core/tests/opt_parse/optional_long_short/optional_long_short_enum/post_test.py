#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    assert "MON" in content
    assert "TUE" in content
    assert "WED" in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
