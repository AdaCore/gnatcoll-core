Contributing to GNATcoll
========================

Thank you for taking the time to contribute!

If this is your first contribution, we invite you to read our [list of
guidelines](https://github.com/AdaCore/contributing-howto), common to all
AdaCore repositories.

Below are specific guidelines to contribute to GNATcoll.

Coding style
------------

Please follow [GNAT's coding style](https://gcc.gnu.org/onlinedocs/gnat-style/)
for Ada code, and [PEP8](https://www.python.org/dev/peps/pep-0008/) for Python
code.

Commits
-------

Organize your work into separated, atomic commits. A commit should
ideally contain the single smallest unit of change possible without breaking
anything. A change should include any tests that were added or modified for it.

Testing
-------

Every change you add to the code should be tested. If this is a bug fix, add
regression test(s). If it's a change of functionality, add functional test(s).
The available tests should provide 100% coverage of the lines added or modified
by the change; if this is not achievable, provide justification why some lines
can't be covered (such as defensive code, etc).

Please refer to [GNATcoll testsuite documentation](testsuite/README.md) for
technical details.
