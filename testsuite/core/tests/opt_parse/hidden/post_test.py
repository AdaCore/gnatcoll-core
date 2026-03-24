#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    assert "usage: test" in content
    assert "--flag" not in content
    assert "--option" not in content
    assert "--option-list" not in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
