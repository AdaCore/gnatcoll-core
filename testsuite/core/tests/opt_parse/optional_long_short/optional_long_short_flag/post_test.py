#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    assert "Flag 1 TRUE" in content
    assert "Flag 2 TRUE" in content
    assert "Flag 3 TRUE" in content
    assert "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" not in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
