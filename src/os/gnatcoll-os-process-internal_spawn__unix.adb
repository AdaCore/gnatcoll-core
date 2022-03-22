------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with GNATCOLL.OS.Libc;         use GNATCOLL.OS.Libc;
with GNATCOLL.OS.Libc.Spawn;   use GNATCOLL.OS.Libc.Spawn;
with GNATCOLL.String_Builders; use GNATCOLL.String_Builders;
with Interfaces.C;
with System;

--------------------
-- Internal_Spawn --
--------------------

separate (GNATCOLL.OS.Process)
function Internal_Spawn
     (Args     : Process_Types.Arguments;
      Cwd      : UTF8.UTF_8_String;
      Env      : Process_Types.Environ;
      Stdin    : FS.File_Descriptor;
      Stdout   : FS.File_Descriptor;
      Stderr   : FS.File_Descriptor;
      Priority : Priority_Class)
     return Process_Handle
is
   use type FS.File_Descriptor;
   use type System.Address;
   use type Interfaces.C.size_t;
   Pid              : Process_Id := 0;
   Posix_Prio       : GNATCOLL.OS.Libc.Priority;
   FA               : constant File_Actions := Init;
   C_Cwd            : Static_String_Builder (Cwd'Length + 1);
   Old_Cwd          : aliased Static_String_Builder (4096 + 1);
   Status           : Integer with Unreferenced;
   C_Status         : Libc_Status;
   Spawn_Status     : Integer;

   --  FD to close after dup2 calls. It's important to call the close
   --  otherwise some pipe might not be closed leading to hanging processes.

   FD_To_Close : array (1 .. 3) of FS.File_Descriptor :=
      (others => FS.Invalid_FD);

   procedure Add_Potential_Close (FD : FS.File_Descriptor);
   pragma Inline (Add_Potential_Close);
   --  If necessary add a FD to the list of fd to close after the calls to
   --  dup2.

   -------------------------
   -- Add_Potential_Close --
   -------------------------

   procedure Add_Potential_Close (FD : FS.File_Descriptor) is
   begin
      if FD = FS.Standin
         or else FD = FS.Standout
         or else FD = FS.Standerr
      then
         --  Never close standard descriptors
         return;
      end if;

      for Index in FD_To_Close'Range loop
         if FD = FD_To_Close (Index) then
            return;
         elsif FD_To_Close (Index) = FS.Invalid_FD then
            FD_To_Close (Index) := FD;
            return;
         end if;
      end loop;
   end Add_Potential_Close;

begin
   --  Internal_Spawn assume we have valid file descriptors passed as input
   --  (see gnatcoll-os-process.adb). Stdin, Stdout, Stderr cannot be
   --  Invalid_FD, To_Stdout or Null_FD (special values have been resolved
   --  by the caller). Nevertheless Stdin, Stdout and Stderr values might not
   --  all be distincts.

   --  ??? Should we ignore the contents of Status below?

   --  First issue dup2 directives
   if Stdin /= FS.Standin then
      Status := Add_Dup2 (FA, Stdin, FS.Standin);
      Add_Potential_Close (Stdin);
   end if;

   if Stdout /= FS.Standout then
      Status := Add_Dup2 (FA, Stdout, FS.Standout);
      Add_Potential_Close (Stdout);
   end if;

   if Stderr /= FS.Standerr then
      Status := Add_Dup2 (FA, Stderr, FS.Standerr);
      Add_Potential_Close (Stderr);
   end if;

   --  Then the close directives
   for Index in FD_To_Close'Range loop
      if FD_To_Close (Index) /= FS.Invalid_FD then
         Status := Add_Close (FA, FD_To_Close (Index));
      end if;
   end loop;

   --  Compute final priority
   case Priority is
      when INHERIT      => Posix_Prio := Inherit_Priority;
      when IDLE         => Posix_Prio := Idle_Priority;
      when BELOW_NORMAL => Posix_Prio := Below_Normal_Priority;
      when NORMAL       => Posix_Prio := Normal_Priority;
      when ABOVE_NORMAL => Posix_Prio := Above_Normal_Priority;
      when HIGH         => Posix_Prio := High_Priority;
   end case;

   --  posix_spawn does not handle chdir, thus we need to call it from the
   --  parent. Note that there is no threading issue as the call to that
   --  function is protected by a global lock (see gnatcoll.os.process)
   if Cwd'Length > 0 then
      Append (C_Cwd, Cwd);
      if Getcwd (As_C_String (Old_Cwd), 4096 + 1) = System.Null_Address
      then
         raise OS_Error
         with "cannot get current directory";
      end if;
      C_Status := Chdir (As_C_String (C_Cwd));
      if C_Status = Error then
         raise OS_Error
         with "cannot change directory before process spawning";
      end if;
   end if;

   --  Spawn the process
   Spawn_Status := Posix_Spawn
      (Pid,
       Process_Types.Program (Args),
       FA,
       Spawn_Attributes (System.Null_Address),
       Process_Types.As_C (Args),
       Process_Types.As_C (Env));
   Destroy (FA);
   if Spawn_Status /= 0 then
      raise OS_Error
      with "cannot spawn process (status:" & Spawn_Status'Img & ")";
   end if;

   --  Should we raise an error if the priority cannot be set ?
   if Posix_Prio /= Inherit_Priority then
      C_Status := Setpriority (Process_Priority, Integer (Pid), Posix_Prio);
   end if;

   if Cwd'Length > 0 then
      C_Status := Chdir (As_C_String (Old_Cwd));
      if C_Status = Error then
         raise OS_Error
         with "cannot reset directory after process spawning";
      end if;
   end if;
   return Process_Handle (Pid);

end Internal_Spawn;
