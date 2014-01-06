------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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
