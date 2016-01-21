------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2016, AdaCore                     --
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

with GNAT.IO;       use GNAT.IO;
with GNATCOLL.Scripts;  use GNATCOLL.Scripts;

package body TextConsole is

   procedure Set_Data_Primitive
     (Instance : Class_Instance; Console  : access Text_Console) is
   begin
      Set (Console.Instances, Instance);
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

end TextConsole;
