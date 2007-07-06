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

with GNAT.IO;       use GNAT.IO;
with GNAT.Scripts;  use GNAT.Scripts;

package body TextConsole is

   procedure Set_Data_Primitive
     (Instance : Class_Instance; Console  : access Text_Console) is
   begin
      Set (Console.Instances, Get_Script (Instance), Instance);
   end Set_Data_Primitive;

   function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Text_Console) return Class_Instance is
   begin
      return Get (Console.Instances, Script);
   end Get_Instance;

   procedure Insert_Text (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Text;

   procedure Insert_Prompt (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Prompt;

   procedure Insert_Error (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Standard_Error, Txt);
   end Insert_Error;

   procedure Insert_Log (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put_Line ("-- log: " & Txt & "--");
   end Insert_Log;

end TextConsole;
