.. _GNATCOLL_OS:

*****************************************
**OS**: Low-level operating system access
*****************************************

.. highlight:: ada

The `GNATCOLL.OS` hierarchy gathers thin, portable wrappers around
operating-system primitives. Compared to the higher-level GNATColl packages
(such as :doc:`GNATCOLL.VFS </core/gnatcoll-vfs>` for files, or
:doc:`GNATCOLL.Random </refs/core/ref-gnatcoll-random-A>` for random numbers),
these units stay close to the underlying system calls: they do little more
than hide the differences between Unix, macOS, and Windows behind a single Ada
API. They are useful when you need predictable, low-overhead access to a
specific OS facility, or as building blocks for higher-level abstractions.

Many operations report failure by raising `GNATCOLL.OS.OS_Error`; others use
package-specific status returns instead, such as an invalid file descriptor, a
`Boolean` result, or a file-attributes value.

The main user-facing packages are:

* `GNATCOLL.OS`: root of the hierarchy. Defines the `OS_Error` exception
  shared by all the child units, together with common types such as `OS_Type`
  and `Filename_Casing_Policy`.

* `GNATCOLL.OS.Constants`: compile-time constants describing the target
  operating system, such as directory and path separators, executable and
  shared-library extensions, and the default filename casing policy.

* `GNATCOLL.OS.Dir`: portable directory iteration that returns, for each
  entry, both its name and its `stat` information.

* `GNATCOLL.OS.FS`: low-level, file-descriptor based access to the filesystem
  (open, read, write, close, ...).

* `GNATCOLL.OS.FSUtil`: utilities operating on file contents on top of
  `GNATCOLL.OS.FS`, such as copying files or computing SHA1/SHA256 hashes.

* :ref:`GNATCOLL.OS.Lock <gnatcoll-os-lock>`: inter-process advisory locks.

* `GNATCOLL.OS.Process`: portable spawning and control of child processes.

* `GNATCOLL.OS.Random`: low-level portable access to the OS
  cryptographically-secure pseudo-random number generator (CSPRNG). For general
  use, prefer the higher-level `GNATCOLL.Random`.

* `GNATCOLL.OS.Stat`: retrieve information about a file (existence, kind,
  size, timestamps, ...).

* `GNATCOLL.OS.Temp`: creation of temporary paths, files and directories using
  hard-to-guess names.

.. index:: locks

.. _gnatcoll-os-lock:

**Lock**: Cross-process locks
=============================

`GNATCOLL.OS.Lock` provides an exclusive lock that is honoured *across
processes*: two processes that lock the same file are guaranteed to exclude
one another. It is backed by the operating-system file-locking primitive
(`flock` on Unix, `_locking` on Windows), which gives it two convenient
properties:

* the lock is released automatically when the holding process terminates, so
  it can never be left stale after a crash; and

* it is purely *advisory*: it coordinates processes that cooperate by taking
  the lock, but does not prevent other code from reading or writing the
  underlying file.

The lock is represented by a `Cross_Process_Lock` object. The type is limited
and controlled, so finalizing it releases the lock; a lock taken in a
declarative block is dropped automatically on leaving the block. A given object
holds at most one lock at a time: calling `Lock` or `Try_Lock` again releases
the lock it currently holds before acquiring the new one. Re-locking a path the
object already holds can therefore lose the lock: it is released before the new
attempt is made, so another process can acquire it in between, and a `Try_Lock`
that then fails would leave the object unlocked.

A process acquires the lock with a blocking call::

  declare
     L : Cross_Process_Lock;
  begin
     --  Block until no other process holds the lock on this file
     L.Lock ("my_app.lock");

     --  Critical section: at most one process runs this at a time.

     --  L.Unlock would release the lock explicitly; here it is released
     --  automatically when L is finalized on leaving the block.
  end;

`Lock` blocks indefinitely until the lock becomes available. On Unix the
process blocks in the kernel; on Windows, which cannot block on a file lock,
`Lock` polls at the interval given by its `Retry_Delay` parameter.

When blocking is undesirable, use `Try_Lock`. With its default arguments it
makes a single, immediate attempt and returns whether it succeeded::

  declare
     L : Cross_Process_Lock;
  begin
     if L.Try_Lock ("my_app.lock") then
        --  Got the lock; do the exclusive work and release it.
        L.Unlock;
     else
        --  Another process is holding the lock; do something else.
        null;
     end if;
  end;

`Try_Lock` can also retry a bounded number of times before giving up, polling
every `Retry_Delay` seconds up to `Retries` times::

  --  Retry every 0.5s, up to 10 times (so up to about five seconds)
  if L.Try_Lock
       (Path        => "my_app.lock",
        Retry_Delay => 0.5,
        Retries     => 10)
  then
     --  Got the lock within the timeout.
     L.Unlock;
  end if;

The path may be relative or absolute; a relative path is resolved against the
current working directory. The lock file is used only as a target: its contents
are never read or modified. It is created if it does not already exist; `Lock`
and `Try_Lock` raise `GNATCOLL.OS.OS_Error` if it cannot be created or opened
(for example because its parent directory does not exist). An existing file is
left untouched, so a path that already holds data can be used safely. The file
is never deleted, even by `Unlock`: removing it while another process holds or
waits on the lock could let two processes end up locking different files under
the same name.

On Unix the lock is held through an open file descriptor that is marked
close-on-exec, so a child created with the usual fork-then-`exec` sequence (for
example through `GNATCOLL.OS.Process`) does not inherit it. A bare `fork` is the
exception: the child inherits the descriptor and shares the lock, so the lock is
not released until the child closes it or exits as well.

.. note::

   These locks are **process-safe but not task-safe**. They reliably exclude
   other *processes*, but not necessarily other *tasks* of the same process:
   where the operating system backs `flock` with per-process `fcntl` locks (for
   example over NFS, or on any platform that implements `flock` that way), two
   tasks of one process are not excluded even when each uses its own lock
   object. Rely on these locks only for inter-process locking, and coordinate
   the tasks within a single process using an in-process mechanism such as a
   protected object.
