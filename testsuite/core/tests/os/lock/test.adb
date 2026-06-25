--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

--  Test GNATCOLL.OS.Lock by re-executing this program as a child process. The
--  child takes the lock and exits (deliberately without releasing it), so the
--  parent can observe cross-process exclusion and, crucially, that the
--  operating system releases the lock when the child terminates.

with Ada.Command_Line;
with Ada.Real_Time;
with GNAT.IO;
with GNAT.OS_Lib;

with GNATCOLL.OS.Constants;
with GNATCOLL.OS.Lock;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package CL renames Ada.Command_Line;
   package IO renames GNAT.IO;
   package Lock renames GNATCOLL.OS.Lock;

   procedure Child_Mode;
   --  Child mode: this program is re-executed with "<mode> <path>" arguments.
   --  The child tries to take the lock and exits with a status that tells the
   --  parent whether it succeeded (0) or not (2). OS_Exit is used on purpose:
   --  it terminates the process without running any finalization, so the lock
   --  is never released by GNATCOLL.OS.Lock. Only the operating system can
   --  release it, exactly as would happen if the child had crashed.

   function Run_Child (Mode : String) return Integer;
   --  Spawn this program as a child in the given mode and return its exit
   --  status.

   Lock_Path : constant String := GNAT.OS_Lib.Normalize_Pathname ("test.lock");
   --  Lock file in the test working directory, shared by parent and children

   Self : constant String := GNAT.OS_Lib.Normalize_Pathname (CL.Command_Name);

   Held     : Lock.Cross_Process_Lock;
   Acquired : Boolean;

   procedure Child_Mode is
      Mode : constant String := CL.Argument (1);
      Path : constant String := CL.Argument (2);

      --  Per-mode retry budget (polling every 0.1 s): "wait" outlasts the
      --  holder, "timeout" expires while the lock stays held, "crash" takes
      --  a free lock and "try" attempts once without blocking. "block" uses
      --  the blocking Lock instead and ignores this budget.
      Retry_Count : constant Natural :=
        (if Mode = "crash"
         then 50
         elsif Mode = "wait"
         then 100
         elsif Mode = "timeout"
         then 5
         else 0);

      Child    : Lock.Cross_Process_Lock;
      Acquired : Boolean;
   begin
      if Mode = "block" then
         Child.Lock (Path => Path, Retry_Delay => 0.1);
         Acquired := True;
      else
         Acquired :=
           Child.Try_Lock
             (Path => Path, Retry_Delay => 0.1, Retries => Retry_Count);
      end if;
      GNAT.OS_Lib.OS_Exit (if Acquired then 0 else 2);
   end Child_Mode;

   function Run_Child (Mode : String) return Integer is
      Args : Argument_List;
   begin
      Args.Append (Self);
      Args.Append (Mode);
      Args.Append (Lock_Path);
      return Wait (Start (Args));
   end Run_Child;

begin
   --  Child mode.
   if CL.Argument_Count >= 2 then
      Child_Mode;
      return 0;
   end if;

   --  Parent mode.
   IO.Put_Line ("GNATCOLL.OS.Lock cross-process lock");

   --  The lock can be acquired on a fresh file.

   Acquired := Held.Try_Lock (Path => Lock_Path);
   A.Assert (Acquired, "the lock should be acquired");
   A.Assert (Held.Is_Locked, "Is_Locked should be True once held");

   --  A second lock on the same file is refused even within this process:
   --  each Try_Lock opens its own descriptor, so two locks contend just as
   --  two processes would.

   declare
      Other     : Lock.Cross_Process_Lock;
      Got_Other : Boolean;
   begin
      Got_Other := Other.Try_Lock (Path => Lock_Path, Retries => 0);
      A.Assert
        (not Got_Other,
         "a second in-process lock on a held file must be refused");
   end;

   --  Retrying against a held lock must take the whole Retry_Delay * Retries
   --  budget before giving up, not return early.

   declare
      use Ada.Real_Time;
      Other   : Lock.Cross_Process_Lock;
      Got     : Boolean;
      Started : constant Time := Clock;
   begin
      Got :=
        Other.Try_Lock (Path => Lock_Path, Retry_Delay => 0.1, Retries => 4);
      A.Assert
        (not Got, "a held lock must not be acquired within the retries");
      A.Assert
        (Clock >= Started + Milliseconds (400),
         "Try_Lock must keep retrying for the whole Retry_Delay * Retries");
   end;

   --  A non-positive Retry_Delay is clamped to zero, so the retries run back
   --  to back with no delay. Against the held lock this must fail, and must
   --  return promptly rather than waiting.

   declare
      use Ada.Real_Time;
      Other   : Lock.Cross_Process_Lock;
      Got     : Boolean;
      Started : constant Time := Clock;
   begin
      Got :=
        Other.Try_Lock (Path => Lock_Path, Retry_Delay => -1.0, Retries => 4);
      A.Assert
        (not Got,
         "a held lock must not be acquired with a negative Retry_Delay");
      A.Assert
        (Clock < Started + Milliseconds (400),
         "a non-positive Retry_Delay must retry immediately, not delay");
   end;

   --  Another process cannot acquire the lock the parent holds, whether it
   --  attempts once (Retries 0) or retries and times out (Retries 5). The
   --  parent keeps holding throughout, as Run_Child blocks until the child
   --  exits.

   A.Assert
     (Run_Child ("try") = 2, "a second process must not acquire a held lock");
   A.Assert
     (Run_Child ("timeout") = 2,
      "a waiter must time out while the lock stays held");

   Held.Unlock;
   A.Assert (not Held.Is_Locked, "Is_Locked should be False once released");

   --  Retrying success: a waiter blocked on a held lock must acquire it
   --  once the holder releases. The parent re-takes the lock, starts a
   --  waiter, holds for half a second so the waiter retries under
   --  contention, then releases; the waiter must then succeed.

   Acquired := Held.Try_Lock (Path => Lock_Path);
   A.Assert (Acquired, "the parent should re-acquire the lock");
   declare
      Args : Argument_List;
      H    : Process_Handle;
   begin
      Args.Append (Self);
      Args.Append ("wait");
      Args.Append (Lock_Path);
      H := Start (Args);
      delay 0.5;
      Held.Unlock;
      A.Assert
        (Wait (H) = 0, "a waiter must acquire the lock once it is released");
   end;

   --  Blocking Lock across processes: a child blocked in Lock on a held lock
   --  must acquire it as soon as the parent releases, like the retrying
   --  Try_Lock waiter above but without a bounded retry budget. The parent
   --  guarantees a release shortly, so the blocking child cannot hang.

   Acquired := Held.Try_Lock (Path => Lock_Path);
   A.Assert
     (Acquired, "the parent should re-acquire the lock for the Lock test");
   declare
      Args : Argument_List;
      H    : Process_Handle;
   begin
      Args.Append (Self);
      Args.Append ("block");
      Args.Append (Lock_Path);
      H := Start (Args);
      delay 0.5;
      Held.Unlock;
      A.Assert
        (Wait (H) = 0,
         "a process blocked in Lock must acquire the lock once released");
   end;

   --  Lock (blocking variant), in-process: it acquires a free file
   --  immediately, and like Try_Lock it releases any lock already held by
   --  Self before taking the new one (so re-locking the same object does not
   --  self-deadlock).

   declare
      L         : Lock.Cross_Process_Lock;
      Lock_File : constant String :=
        GNAT.OS_Lib.Normalize_Pathname ("blocking.lock");
   begin
      L.Lock (Path => Lock_File);
      A.Assert (L.Is_Locked, "Lock should acquire a free file");
      L.Lock (Path => Lock_File);
      A.Assert
        (L.Is_Locked, "Lock should re-acquire after releasing its own lock");
      L.Unlock;
      A.Assert (not L.Is_Locked, "Is_Locked should be False after Unlock");
   end;

   --  Lock raises OS_Error (rather than blocking) when the lock file cannot
   --  be opened, e.g. because its parent directory does not exist.

   declare
      L        : Lock.Cross_Process_Lock;
      Bad_Path : constant String :=
        "no_such_dir" & GNATCOLL.OS.Constants.Dir_Sep & "x.lock";
      Raised   : Boolean := False;
   begin
      begin
         L.Lock (Path => Bad_Path);
      exception
         when GNATCOLL.OS.OS_Error =>
            Raised := True;
      end;
      A.Assert
        (Raised,
         "Lock must raise OS_Error when the lock file cannot be opened");
      A.Assert (not L.Is_Locked, "a failed Lock must not leave the lock held");
   end;

   --  Crash safety: the child acquires the lock then terminates without
   --  releasing it. The operating system must release it on the child's
   --  death, so the parent can take it afterwards.

   A.Assert
     (Run_Child ("crash") = 0, "the child should have acquired the free lock");
   Acquired := Held.Try_Lock (Path => Lock_Path);
   A.Assert
     (Acquired, "a lock held by a terminated process must be acquirable");
   Held.Unlock;

   --  A pre-existing, non-empty lock file must still be lockable. On
   --  Windows the lock is a byte range relative to the file position, so a
   --  non-empty file checks that contenders still agree on the same byte
   --  (the lock/unlock primitives seek to the start). On Unix flock is
   --  whole-file, so this just confirms the content is left untouched.

   declare
      Nonempty_Path : constant String :=
        GNAT.OS_Lib.Normalize_Pathname ("nonempty.lock");
      Data          : constant String := "stale lock file content" & ASCII.LF;
      WFD           : GNAT.OS_Lib.File_Descriptor;
      Written       : Integer;
      pragma Unreferenced (Written);

      use type GNAT.OS_Lib.File_Descriptor;

      First_Lock  : Lock.Cross_Process_Lock;
      Second_Lock : Lock.Cross_Process_Lock;
      Got_First   : Boolean;
      Got_Second  : Boolean;
   begin
      WFD := GNAT.OS_Lib.Create_File (Nonempty_Path, GNAT.OS_Lib.Binary);
      A.Assert
        (WFD /= GNAT.OS_Lib.Invalid_FD,
         "the non-empty lock file should be created");
      Written := GNAT.OS_Lib.Write (WFD, Data'Address, Data'Length);
      GNAT.OS_Lib.Close (WFD);

      Got_First := First_Lock.Try_Lock (Path => Nonempty_Path);
      A.Assert (Got_First, "a non-empty lock file must still be lockable");

      Got_Second := Second_Lock.Try_Lock (Path => Nonempty_Path);
      A.Assert
        (not Got_Second,
         "a second lock on a non-empty held file must be refused");

      First_Lock.Unlock;

      --  Check that the file content was left untouched.
      declare
         RFD    : GNAT.OS_Lib.File_Descriptor;
         Buffer : String (1 .. Data'Length);
         Read   : Integer;
      begin
         RFD := GNAT.OS_Lib.Open_Read (Nonempty_Path, GNAT.OS_Lib.Binary);
         Read := GNAT.OS_Lib.Read (RFD, Buffer'Address, Buffer'Length);
         GNAT.OS_Lib.Close (RFD);
         A.Assert
           (Read = Data'Length and then Buffer = Data,
            "locking must leave the file content untouched");
      end;
   end;

   return A.Report;
end Test;
