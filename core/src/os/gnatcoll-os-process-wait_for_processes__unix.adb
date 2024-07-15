------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with GNATCOLL.OS.Libc; use GNATCOLL.OS.Libc;
with GNATCOLL.OS.FS;
with GNAT.Task_Lock;

separate (GNATCOLL.OS.Process)

function Wait_For_Processes
  (Processes : Process_Array;
   Timeout   : Duration := INFINITE_TIMEOUT)
   return Integer
is
   package FS renames GNATCOLL.OS.FS;
   package Cal renames Ada.Calendar;

   use all type Cal.Time;

   --  C Implementation is located in src/os/unix/process-wrappers.c

   function Add_Monitoring_Fd (Fd : FS.File_Descriptor) return Libc_Status
   with Import => True,
        Convention => C,
        External_Name => "__gnatcoll_add_monitoring_fd";
   --  Add a file descriptor on which a byte should be written each time a
   --  SIGCHLD signal is received.

   function Remove_Monitoring_Fd (Fd : FS.File_Descriptor) return Libc_Status
   with Import => True,
        Convention => C,
        External_Name => "__gnatcoll_remove_monitoring_fd";
   --  Remove a file descriptor added with the previous function.

   function Init_Sigchld_Monitoring return Libc_Status
   with Import => True,
        Convention => C,
        External_Name => "__gnatcoll_init_sigchld_monitoring";
   --  Set signal handler for SIGCHLD.

   function Wait_For_Sigchld
      (Fd      : FS.File_Descriptor;
       Timeout : Sint_64)
      return Libc_Status
   with Import => True,
        Convention => C,
        External_Name => "__gnatcoll_wait_for_sigchld";
   --  Wait during timeout (in microseconds) for a write on Fd. If Timeout is
   --  < 0 then infinite is assumed.

   function Get_First_Waitable_Process return Integer;
   --  Loop other the monitored processes and returned the first process in the
   --  WAITABLE state. If no process is in WAITABLE state, return WAIT_TIMEOUT
   --  if at least one is in RUNNING state. Otherwise return WAIT_NO_PROCESS.

   procedure Init_SIGCHLD_Monitoring
   with Inline => True;
   --  Init SIGCHLD monitoring

   procedure Finalize_SIGCHLD_Monitoring
   with Inline => True;
   --  Finalize SIGCHLD monitorig

   Pipe_Read, Pipe_Write : OS.FS.File_Descriptor;
   Status                : Libc_Status;
   Result                : Integer := WAIT_NO_PROCESS;
   End_Time              : Cal.Time;
   Is_Infinite_Timeout   : Boolean := False;
   --  Maximum end time

   --------------------------------
   -- Get_First_Waitable_Process --
   --------------------------------

   function Get_First_Waitable_Process return Integer is
      Result : Integer := WAIT_NO_PROCESS;
   begin
      for Index in Processes'Range loop
         if State (Processes (Index)) = WAITABLE then
            return Index;
         elsif State (Processes (Index)) = RUNNING then
            Result := WAIT_TIMEOUT;
         end if;
      end loop;

      return Result;
   end Get_First_Waitable_Process;

   -----------------------------
   -- Init_SIGCHLD_Monitoring --
   -----------------------------

   procedure Init_SIGCHLD_Monitoring is
   begin
      GNAT.Task_Lock.Lock;
      Status := Init_Sigchld_Monitoring;

      if Status /= Success then
         GNAT.Task_Lock.Unlock;
         raise OS_Error with "cannot set SIGCHLD signal handler";
      end if;

      Status := Add_Monitoring_Fd (Pipe_Write);

      if Status /= Success then
         GNAT.Task_Lock.Unlock;
         raise OS_Error
            with "cannot call more than 256 concurrent wait_for_processes";
      end if;
      GNAT.Task_Lock.Unlock;
   end Init_SIGCHLD_Monitoring;

   ---------------------------------
   -- Finalize_SIGCHLD_Monitoring --
   ---------------------------------

   procedure Finalize_SIGCHLD_Monitoring is
   begin
      GNAT.Task_Lock.Lock;
      Status := Remove_Monitoring_Fd (Pipe_Write);

      if Status /= Success then
         GNAT.Task_Lock.Unlock;
         raise OS_Error with "invalid SIGCHLD monitoring Fd";
      end if;

      GNAT.Task_Lock.Unlock;
      FS.Close (Pipe_Read);
      FS.Close (Pipe_Write);
   end Finalize_SIGCHLD_Monitoring;

begin
   --  Handle case in which process list is empty
   if Processes'Length = 0 then
      return Result;
   end if;

   --  Perform a first check that will avoid need for locks, ...
   Result := Get_First_Waitable_Process;
   if Result /= WAIT_TIMEOUT then
      return Result;
   end if;

   if Timeout <= 0.0 then
      --  No need to wait
      return Result;
   elsif Timeout >= INFINITE_TIMEOUT then
      Is_Infinite_Timeout := True;
   else
      End_Time := Cal.Clock + Timeout;
   end if;

   --  Put in place the monitoring infrastructure
   FS.Open_Pipe (Pipe_Read, Pipe_Write);

   Init_SIGCHLD_Monitoring;

   --  As some SIGCHLD might have been missed during the setup, perform another
   --  check
   Result := Get_First_Waitable_Process;

   if Result /= WAIT_TIMEOUT then
      Finalize_SIGCHLD_Monitoring;
      return Result;
   end if;

   --  Start looping
   loop
      declare
         --  Remaining max waiting time in microseconds
         Microsecond_Timeout : constant Sint_64 :=
            (if Is_Infinite_Timeout
             then -1
             else Sint_64 ((End_Time - Cal.Clock) * 1_000_000));
      begin
         --  Exit when timeout is reached and Timeout is not infinite
         --  (i.e < 0.0)
         if Microsecond_Timeout < 0 and then not Is_Infinite_Timeout then
            exit;
         end if;

         --  Wait
         Status := Wait_For_Sigchld (Pipe_Read, Microsecond_Timeout);

         if Status = Success then
            --  A SIGCHLD has been received. Check if the process is in our
            --  list.
            Result := Get_First_Waitable_Process;
            if Result /= WAIT_TIMEOUT then
               exit;
            end if;
         end if;
      end;
   end loop;

   Finalize_SIGCHLD_Monitoring;

   return Result;

end Wait_For_Processes;
