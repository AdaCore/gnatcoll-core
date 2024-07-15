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

with GNATCOLL.OS.Win32; use GNATCOLL.OS.Win32;
with GNATCOLL.OS.Win32.Files; use GNATCOLL.OS.Win32.Files;
with GNATCOLL.OS.Win32.Process; use GNATCOLL.OS.Win32.Process;
with GNATCOLL.WString_Builders; use GNATCOLL.WString_Builders;

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
   SI            : STARTUPINFO;
   PI            : PROCESS_INFORMATION;
   Result        : BOOL;
   Flags         : Process_Creation_Flags;
   WCwd          : Static_WString_Builder (Cwd'Length + 1);
begin
   Append (WCwd, Cwd);
   SI.cb := STARTUPINFO'Size / 8;
   SI.Flags := STARTF_USESTDHANDLES;
   SI.ShowWindow := SW_HIDE;
   SI.StdInput := GetOSFHandle (Stdin);
   SI.StdOutput := GetOSFHandle (Stdout);
   SI.StdError := GetOSFHandle (Stderr);

   case Priority is
      when IDLE =>
         Flags := IDLE_PRIORITY_CLASS;
      when BELOW_NORMAL =>
         Flags := BELOW_NORMAL_PRIORITY_CLASS;
      when NORMAL =>
         Flags := NORMAL_PRIORITY_CLASS;
      when ABOVE_NORMAL =>
         Flags := ABOVE_NORMAL_PRIORITY_CLASS;
      when HIGH =>
         Flags := HIGH_PRIORITY_CLASS;
      when INHERIT =>
         Flags := 0;
   end case;

   Flags := Flags or CREATE_UNICODE_ENVIRONMENT;

   Result := CreateProcess
      (ApplicationName    => Null_C_WString,
       CommandLine        => Process_Types.As_C (Args),
       ProcessAttributes  => No_Inherit_Handle'Access,
       ThreadAttributes   => null,
       bInheritHandles    => BOOL_TRUE,
       CreationFlags      => Flags,
       Environment        => Process_Types.As_C (Env),
       CurrentDirectory   => As_C_WString (WCwd, Null_If_Empty => True),
       Startup            => SI,
       ProcessInformation => PI);

   if Result = BOOL_TRUE then
      Result := CloseHandle (PI.Thread);
      return Process_Handle (PI.Process);
   else
      raise OS_Error with "cannot spawn process";
   end if;
end Internal_Spawn;
