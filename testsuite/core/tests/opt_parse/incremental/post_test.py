#!/usr/bin/env python3
import sys

if __name__ == "__main__":
    content = sys.stdin.read()
    assert "for option value - switch is present multiple times" in content
    sys.exit(0)
