#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    assert "file1 file11 file111" in content
    assert "file2 file22 file222" in content
    assert "file3 file33 file333" in content
    assert "raised GNATCOLL.OPT_PARSE.OPT_PARSE_ERROR" not in content
    assert "raised CONSTRAINT_ERROR" not in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
