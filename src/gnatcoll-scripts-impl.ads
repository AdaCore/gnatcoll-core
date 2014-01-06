------------------------------------------------------------------------------
--                             G N A T C O L L                              --
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
