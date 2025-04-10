#!/usr/bin/env python3
import sys
from e3.diff import diff

EXPECTED = """
[PKG] Entering Test.Foo:test.adb:13 (loc:test.adb:13)
   [PKG] A = 3 (loc:test.adb:15)
   [PKG] Entering Test.Foo:test.adb:13 (loc:test.adb:13)
      [PKG] A = 2 (loc:test.adb:15)
      [PKG] Entering Test.Foo:test.adb:13 (loc:test.adb:13)
         [PKG] A = 1 (loc:test.adb:15)
      [PKG] Leaving Test.Foo:test.adb:13 (loc:test.adb:13)
   [PKG] Leaving Test.Foo:test.adb:13 (loc:test.adb:13)
[PKG] Leaving Test.Foo:test.adb:13 (loc:test.adb:13)
[PKG] [Test success msg] (loc:test.adb:25)
[PKG.EXCEPTIONS] [Test error msg] (loc:test.adb:33)
[PKG] Entering Test.Foo:test.adb:13 (entity:Test.Foo)
   [PKG] A = 3 (entity:Test.Foo)
   [PKG] Entering Test.Foo:test.adb:13 (entity:Test.Foo)
      [PKG] A = 2 (entity:Test.Foo)
      [PKG] Entering Test.Foo:test.adb:13 (entity:Test.Foo)
         [PKG] A = 1 (entity:Test.Foo)
      [PKG] Leaving Test.Foo:test.adb:13 (entity:Test.Foo)
   [PKG] Leaving Test.Foo:test.adb:13 (entity:Test.Foo)
[PKG] Leaving Test.Foo:test.adb:13 (entity:Test.Foo)
[PKG] [Test success msg] (entity:Test)
[PKG.EXCEPTIONS] [Test error msg] (entity:Test)
[PKG] Entering Test.Foo:test.adb:13
   [PKG] A = 3
   [PKG] Entering Test.Foo:test.adb:13
      [PKG] A = 2
      [PKG] Entering Test.Foo:test.adb:13
         [PKG] A = 1
      [PKG] Leaving Test.Foo:test.adb:13
   [PKG] Leaving Test.Foo:test.adb:13
[PKG] Leaving Test.Foo:test.adb:13
[PKG] [Test success msg]
[PKG.EXCEPTIONS] [Test error msg]
"""

if __name__ == "__main__":
    content = sys.stdin.read()
    assert EXPECTED.strip() in content, diff(EXPECTED, content)
    print("<=== TEST PASSED ===>")
    sys.exit(0)
