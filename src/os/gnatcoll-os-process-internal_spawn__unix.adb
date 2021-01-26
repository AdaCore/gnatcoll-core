------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
   Pid          : Process_Id := 0;
   Posix_Prio   : GNATCOLL.OS.Libc.Priority;
   FA           : constant File_Actions := Init;
   C_Cwd        : Static_String_Builder (Cwd'Length + 1);
   Old_Cwd      : aliased Static_String_Builder (4096 + 1);
   Status       : Integer;
   C_Status     : Libc_Status;
   Spawn_Status : Integer;
begin
   --  Create file descriptors
   if Stdin /= FS.Standin then
      Status := Add_Dup2 (FA, Stdin, FS.Standin);
      Status := Add_Close (FA, Stdin);
   end if;
   if Stdout /= FS.Standout then
      Status := Add_Dup2 (FA, Stdout, FS.Standout);
      Status := Add_Close (FA, Stdout);
   end if;
   if Stderr /= FS.Standerr then
      Status := Add_Dup2 (FA, Stderr, FS.Standerr);
      Status := Add_Close (FA, Stderr);
   end if;

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
