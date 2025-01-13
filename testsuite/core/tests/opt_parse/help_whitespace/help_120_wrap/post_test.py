#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read().splitlines()
    # Confirm that when permitted, the help text can exceed the previous 80
    # character limit without wrapping.
    for line in content:
        if len(line) > 80:
            print("<=== TEST PASSED ===>")
            sys.exit(0)
    print("there should be at least one line with more than 80 characters")
    sys.exit(1)
