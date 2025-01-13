#!/usr/bin/env python3
import sys

EXPECTED_OUTPUT = """
"", ':', IF_EMPTY
=================

"bar": abs("bar")
"foo": ""
"dir1/bar": ""
"dir1/foo": abs("dir1/foo")
"dir2/foo": abs("dir2/foo")
abs("dir1/foo"): abs("dir1/foo")

"dir1|dir2", '|', IF_EMPTY
==========================

"bar": abs("dir2/bar")
"foo": abs("dir1/foo")
"dir1/bar": ""
"dir1/foo": ""
"dir2/foo": ""
abs("dir1/foo"): abs("dir1/foo")

"dir2|dir1", '|', IF_EMPTY
==========================

"bar": abs("dir2/bar")
"foo": abs("dir2/foo")
"dir1/bar": ""
"dir1/foo": ""
"dir2/foo": ""
abs("dir1/foo"): abs("dir1/foo")

"dir2", '|', CWD_FIRST
======================

"bar": abs("bar")
"foo": abs("dir2/foo")
"dir1/bar": ""
"dir1/foo": abs("dir1/foo")
"dir2/foo": abs("dir2/foo")
abs("dir1/foo"): abs("dir1/foo")

"dir2", '|', CWD_LAST
=====================

"bar": abs("dir2/bar")
"foo": abs("dir2/foo")
"dir1/bar": ""
"dir1/foo": abs("dir1/foo")
"dir2/foo": abs("dir2/foo")
abs("dir1/foo"): abs("dir1/foo")

Done.
"""

if __name__ == "__main__":
    # It would be better to use directly assertions in the test itself
    content = sys.stdin.read().replace("\r", "").strip()
    assert content == EXPECTED_OUTPUT.strip()
    print("<=== TEST PASSED ===>")
    sys.exit(0)
