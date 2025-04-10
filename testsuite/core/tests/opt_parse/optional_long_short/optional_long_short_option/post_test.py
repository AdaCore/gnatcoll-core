#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    assert "latin-1" in content
    assert "latin-2" in content
    assert "latin-3" in content
    assert "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" not in content
    assert "raised CONSTRAINT_ERROR" not in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
