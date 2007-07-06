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

with GNAT.Scripts;

package TextConsole is

   type Text_Console is new GNAT.Scripts.Virtual_Console_Record with private;
   overriding procedure Insert_Text
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Prompt
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Error
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Log
     (Console : access Text_Console; Txt : String);
   overriding procedure Set_Data_Primitive
     (Instance : GNAT.Scripts.Class_Instance; Console : access Text_Console);
   overriding function Get_Instance
     (Script  : access GNAT.Scripts.Scripting_Language_Record'Class;
      Console : access Text_Console) return GNAT.Scripts.Class_Instance;

private
   type Text_Console is new GNAT.Scripts.Virtual_Console_Record with record
      Instances : GNAT.Scripts.Instance_List;
   end record;

end TextConsole;
