--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

--  Inter-process advisory locks backed by the operating system file-locking
--  primitive (flock on Unix, _locking on Windows).
--
--  The lock file is used purely as a lock target: its contents are never read
--  or modified. It is created if it does not exist, but an existing file is
--  left untouched, so a path that already holds data may be used safely.
--
--  The lock is released automatically when the holding process terminates, so
--  it cannot be left stale.
--
--  As these are advisory locks, they do not prevent other access to the file.
--
--  Process-safe but not task-safe. It reliably excludes other processes, but
--  whether it also excludes other tasks of the same process depends on the
--  platform and filesystem (for example, it does not hold over NFS on unix).
--  Rely on it only for inter-process locking. Coordinate tasks within a
--  process with an in-process mechanism such as a protected object.

with Ada.Strings.UTF_Encoding;
private with Ada.Finalization;
private with GNATCOLL.OS.FS;

package GNATCOLL.OS.Lock is

   package UTF8 renames Ada.Strings.UTF_Encoding;

   type Cross_Process_Lock is tagged limited private;
   --  An exclusive lock. Two processes locking the same file exclude each
   --  other. The lock is released by Unlock or by finalization of the object.

   procedure Lock
     (Self        : in out Cross_Process_Lock;
      Path        : UTF8.UTF_8_String;
      Retry_Delay : Duration := 1.0);
   --  Acquire the lock on the file at Path, creating it if needed; if Self
   --  already holds a lock it is released first. Blocks indefinitely until
   --  the lock is acquired.
   --
   --  Retry_Delay is ignored on Unix, where the process blocks in the kernel;
   --  on Windows, which cannot block, it is the interval between polls.
   --
   --  Raises OS_Error if the lock file cannot be created or opened.

   function Try_Lock
     (Self        : in out Cross_Process_Lock;
      Path        : UTF8.UTF_8_String;
      Retry_Delay : Duration := 1.0;
      Retries     : Natural := 0) return Boolean;
   --  Acquire the lock on the file at Path, creating it if needed; if Self
   --  already holds a lock it is released first.
   --
   --  One attempt is made immediately. If the lock is held elsewhere, Try_Lock
   --  retries every Retry_Delay seconds, up to Retries times (so for up to
   --  Retry_Delay * Retries seconds). The default Retries => 0 makes a single,
   --  non-blocking attempt.
   --
   --  Return True if the lock was acquired, False otherwise.
   --  Raises OS_Error if the lock file cannot be created or opened.

   procedure Unlock (Self : in out Cross_Process_Lock);
   --  Release the lock. Has no effect if Self does not hold a lock.
   --
   --  The lock file is left in place; it is never deleted. Removing it while
   --  another process holds or waits on the lock could let two processes lock
   --  different files under the same name.

   function Is_Locked (Self : Cross_Process_Lock) return Boolean;
   --  Return True if Self currently holds the lock.

private

   type Cross_Process_Lock is new Ada.Finalization.Limited_Controlled
   with record
      FD : GNATCOLL.OS.FS.File_Descriptor := GNATCOLL.OS.FS.Invalid_FD;
   end record;

   overriding
   procedure Finalize (Self : in out Cross_Process_Lock);

end GNATCOLL.OS.Lock;
