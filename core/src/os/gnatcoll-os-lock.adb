--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with Ada.Real_Time; use Ada.Real_Time;

package body GNATCOLL.OS.Lock is

   package FS renames GNATCOLL.OS.FS;
   use type FS.File_Descriptor;

   type Lock_Result is (Acquired, Contended, Failed);
   --  Outcome of one attempt to take the OS lock:
   --    Acquired  - the lock was taken
   --    Contended - the lock is held elsewhere; retrying may succeed
   --    Failed    - the lock operation failed for some other reason

   function Lock_FD
     (FD : FS.File_Descriptor; Blocking : Boolean) return Lock_Result;
   --  Attempt the operating system exclusive lock on FD. Implemented per OS
   --  as a subunit selected by the project file (see the Naming package).

   function Attempt_Lock
     (Self     : in out Cross_Process_Lock;
      Path     : UTF8.UTF_8_String;
      Blocking : Boolean := False) return Boolean;
   --  One attempt at the lock on Self.FD, blocking until acquired when
   --  Blocking is True (on Unix; Windows never blocks, see Lock_FD).
   --  Returns True if the lock was acquired and False if it is contended.
   --  On any other failure, closes the descriptor and raises OS_Error.

   procedure Unlock_FD (FD : FS.File_Descriptor);
   --  Release the operating system lock taken by Lock_FD. Implemented
   --  per OS as a subunit.

   procedure Open_For_Locking
     (Self : in out Cross_Process_Lock; Path : UTF8.UTF_8_String);
   --  Release any lock Self holds, then open the lock file at Path, creating
   --  it if needed, and store the descriptor in Self.FD. Raises OS_Error if
   --  the file could not be opened.
   --
   --  Append_Mode is used because it neither fails when the file already
   --  exists nor resets its content, and it opens the descriptor in
   --  close-on-exec mode so the lock is not inherited (and thus kept alive)
   --  by subprocesses spawned while it is held. This covers fork+exec only; a
   --  bare fork while the lock is held duplicates the descriptor into the
   --  child, which then shares the lock.
   --
   --  The lock file is intentionally never removed: unlinking it while
   --  another process holds or waits on the lock would let two processes
   --  lock two different inodes under the same name.
   --
   --  Raises OS_Error if the lock file cannot be created or opened.

   ----------------------
   -- Open_For_Locking --
   ----------------------

   procedure Open_For_Locking
     (Self : in out Cross_Process_Lock; Path : UTF8.UTF_8_String) is
   begin
      Unlock (Self);
      Self.FD := FS.Open (Path, Mode => FS.Append_Mode);
      if Self.FD = FS.Invalid_FD then
         raise OS_Error with "cannot open lock file: " & String (Path);
      end if;
   end Open_For_Locking;

   ------------------
   -- Attempt_Lock --
   ------------------

   function Attempt_Lock
     (Self     : in out Cross_Process_Lock;
      Path     : UTF8.UTF_8_String;
      Blocking : Boolean := False) return Boolean is
   begin
      case Lock_FD (Self.FD, Blocking) is
         when Acquired  =>
            return True;

         when Contended =>
            return False;

         when Failed    =>
            FS.Close (Self.FD);
            Self.FD := FS.Invalid_FD;
            raise OS_Error with "cannot lock file: " & String (Path);
      end case;
   end Attempt_Lock;

   ----------
   -- Lock --
   ----------

   procedure Lock
     (Self        : in out Cross_Process_Lock;
      Path        : UTF8.UTF_8_String;
      Retry_Delay : Duration := 1.0)
   is
      Wait_Span : constant Time_Span :=
        To_Time_Span (Duration'Max (Retry_Delay, 0.0));

      Next : Time := Clock;
      --  Deadline of the next attempt, advanced by Wait_Span on each retry

   begin
      Open_For_Locking (Self, Path);

      loop
         exit when Attempt_Lock (Self, Path, Blocking => True);
         Next := Next + Wait_Span;
         delay until Next;
      end loop;
   end Lock;

   --------------
   -- Try_Lock --
   --------------

   function Try_Lock
     (Self        : in out Cross_Process_Lock;
      Path        : UTF8.UTF_8_String;
      Retry_Delay : Duration := 1.0;
      Retries     : Natural := 0) return Boolean
   is
      Wait_Span : constant Time_Span :=
        To_Time_Span (Duration'Max (Retry_Delay, 0.0));

      Next : Time := Clock;
      --  Deadline of the next attempt, advanced by Wait_Span on each retry

   begin
      Open_For_Locking (Self, Path);

      --  The first attempt is immediate, with delays before each retry
      if Attempt_Lock (Self, Path) then
         return True;
      end if;

      for J in 1 .. Retries loop
         Next := Next + Wait_Span;
         delay until Next;

         if Attempt_Lock (Self, Path) then
            return True;
         end if;
      end loop;

      FS.Close (Self.FD);
      Self.FD := FS.Invalid_FD;
      return False;
   end Try_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Self : in out Cross_Process_Lock) is
   begin
      if Self.FD /= FS.Invalid_FD then
         Unlock_FD (Self.FD);
         FS.Close (Self.FD);
         Self.FD := FS.Invalid_FD;
      end if;
   end Unlock;

   ---------------
   -- Is_Locked --
   ---------------

   function Is_Locked (Self : Cross_Process_Lock) return Boolean is
   begin
      return Self.FD /= FS.Invalid_FD;
   end Is_Locked;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Cross_Process_Lock) is
   begin
      Unlock (Self);
   end Finalize;

   function Lock_FD
     (FD : FS.File_Descriptor; Blocking : Boolean) return Lock_Result
   is separate;

   procedure Unlock_FD (FD : FS.File_Descriptor) is separate;

end GNATCOLL.OS.Lock;
