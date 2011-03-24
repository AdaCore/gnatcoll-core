-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2003-2011, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides subprograms that are used when adding support for
--  new scripting languages. Applications should not typically have a need for
--  these types or subprograms.

package GNATCOLL.Scripts.Impl is

   function From_Instance
     (Script : access Scripting_Language_Record'Class;
      Inst   : access Class_Instance_Record'Class) return Class_Instance;
   --  Return a class instance wrapping Inst.
   --  For internal use by scripting languages only.

   procedure Insert_Text
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String;
      Hide    : Boolean := False);
   procedure Insert_Log
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String);
   procedure Insert_Error
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String);
   procedure Insert_Prompt
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String);
   --  Display txt either on the specified console or on the scripts' default
   --  console if Console is set to null.
   --  If Hide is set to True, the text is not displayed on the console after
   --  all, although it will be displayed in the log instead.

   procedure Register_Console_Class
     (Repo  : access Scripts_Repository_Record'Class;
      Class : Class_Type);
   --  Register the console class, which is used to redirect output of script
   --  languages to a specific Virtual_Console

   procedure Register_Logger_Class
     (Repo  : access Scripts_Repository_Record'Class;
      Class : Class_Type);
   --  Register the logger class, used to interfaces with GNATCOLL.Traces from
   --  python.

end GNATCOLL.Scripts.Impl;
