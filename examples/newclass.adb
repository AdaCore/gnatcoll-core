-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;
with Common;         use Common;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;
with TextConsole;    use TextConsole;

procedure NewClass is
   Repo   : Scripts_Repository :=
     Common.Register_Scripts_And_Functions;
   Buffer : String (1 .. 1000);
   Last   : Integer;
   Errors : Boolean;
   Console : aliased Text_Console;
begin
   Put_Line ("Please type python commands:");

   Set_Default_Console
     (Lookup_Scripting_Language (Repo, "python"), Console'Unchecked_Access);

   loop
      Get_Line (Buffer, Last);
      Execute_Command
        (Script       => Lookup_Scripting_Language (Repo, "python"),
         Command      => Buffer (1 .. Last),
         Show_Command => False,
         Hide_Output  => False,
         Errors       => Errors);
   end loop;

exception
   when End_Error =>
      Destroy (Repo);
end NewClass;
