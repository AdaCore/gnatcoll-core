------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
--  Implementation of File class

with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GNATCOLL.VFS;

package GNATCOLL.Scripts.Files is

   procedure Register_Commands
     (Repo : access Scripts_Repository_Record'Class);
   --  Add basic script commands for File class.

   --  Next subprogram could be useful to define new commands

   function Get_File_Class
     (Repo : access Scripts_Repository_Record'Class)
      return Class_Type;
   --  Return the class to use for file types. This encapsulates a File_Info.

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive)
      return GNATCOLL.VFS.Virtual_File;
   procedure Set_Nth_Arg
     (Data : in out Callback_Data'Class;
      N    : Positive;
      File : GNATCOLL.VFS.Virtual_File);
   function Get_Data
     (Instance : Class_Instance) return GNATCOLL.VFS.Virtual_File;
   procedure Set_Data (Instance : Class_Instance; File : Virtual_File);
   --  Retrieve the file information from an instance. This returns No_File
   --  if no instance is passed

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Class_Instance;
   --  Return a new File

   function Get_File_Class (Data : Callback_Data'Class) return Class_Type;
   --  Return Class_Type from File class

end GNATCOLL.Scripts.Files;
