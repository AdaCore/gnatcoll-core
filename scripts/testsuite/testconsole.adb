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

package body TestConsole is

   procedure Set_Data_Primitive
     (Instance : Class_Instance; Console  : access Test_Console) is
   begin
      Set (Console.Instances, Get_Script (Instance), Instance);
   end Set_Data_Primitive;

   function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Test_Console) return Class_Instance is
   begin
      return Get (Console.Instances, Script);
   end Get_Instance;

   procedure Insert_Text (Console : access Test_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Text;

   procedure Insert_Prompt (Console : access Test_Console; Txt : String) is
      pragma Unreferenced (Console, Txt);
   begin
      null;
   end Insert_Prompt;

   procedure Insert_Error (Console : access Test_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put_Line ("Error: " & Txt);
   end Insert_Error;

   procedure Insert_Log (Console : access Test_Console; Txt : String) is
      pragma Unreferenced (Console, Txt);
   begin
      null;
   end Insert_Log;

   procedure Free (Console : in out Test_Console) is
   begin
      Free (Console.Instances);
   end Free;

   function Read
     (Console    : access Test_Console;
      Size       : Integer;
      Whole_Line : Boolean) return String
   is
      pragma Unreferenced (Console);
      --  At most 20 characters
      Str  : String (1 .. Integer'Min (20, Size));
      Last : Integer := Str'Last;
   begin
      if Whole_Line then
         Str (Last) := ASCII.LF;
         Last := Last - 1;
      end if;

      for S in Str'First .. Last loop
         Str (S) := Character'Val (Character'Pos ('A') + S - Str'First);
      end loop;

      return Str;
   end Read;

end TestConsole;
