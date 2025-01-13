#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read().splitlines()
    content = " ".join(line.strip() for line in content)

    assert "[--charset|-C <charset name>]" in content
    assert "[--day|-D <three letter day of week>]" in content
    assert "[--files|-F <list of filepaths to parse>]" in content
    assert "[--charset2|-C2 CHARSET2]" in content
    assert "[--day2|-D2 DAY2]" in content
    assert "[--files2|-F2 FILES2 [FILES2...]]" in content
    print("<=== TEST PASSED ===>")
    sys.exit(0)
