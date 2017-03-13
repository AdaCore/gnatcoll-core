------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with GNAT.Strings;    use GNAT.Strings;
with GNATCOLL.Remote; use GNATCOLL.Remote;

package GNATCOLL.IO.Remote.Windows is

   --  The following methods are equivalent to their native counterparts.
   --  See GNATCOLL.IO for documentation.

   function Current_Dir
     (Exec : access Server_Record'Class) return FS_String;
   function Home_Dir
     (Exec : access Server_Record'Class) return FS_String;
   function Tmp_Dir
     (Exec : access Server_Record'Class) return FS_String;
   function Get_Logical_Drives
     (Exec : access Server_Record'Class) return String_List_Access;
   function Locate_On_Path
     (Exec : access Server_Record'Class;
      Base : FS_String) return FS_String;
   function Is_Regular_File
     (Exec : access Server_Record'Class;
      File : FS_String) return Boolean;
   function Size
     (Exec : access Server_Record'Class;
      File : FS_String) return Long_Integer;
   function Is_Directory
     (Exec : access Server_Record'Class;
      File : FS_String) return Boolean;
   function Is_Symbolic_Link
     (Exec : access Server_Record'Class;
      File : FS_String) return Boolean;
   function File_Time_Stamp
     (Exec : access Server_Record'Class;
      File : FS_String) return Ada.Calendar.Time;
   function Is_Writable
     (Exec : access Server_Record'Class;
      File : FS_String) return Boolean;
   procedure Set_Writable
     (Exec : access Server_Record'Class;
      File  : FS_String;
      State : Boolean);
   procedure Set_Readable
     (Exec : access Server_Record'Class;
      File  : FS_String;
      State : Boolean);
   procedure Rename
     (Exec : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean);
   procedure Copy
     (Exec : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean);
   procedure Delete
     (Exec    : access Server_Record'Class;
      File    : FS_String;
      Success : out Boolean);
   function Read_Whole_File
     (Exec : access Server_Record'Class;
      File : FS_String)
      return GNAT.Strings.String_Access;
   function Read_Whole_File
     (Exec : access Server_Record'Class;
      File : FS_String)
      return GNATCOLL.Strings.XString;
   function Write_File
     (Exec    : access Server_Record'Class;
      File    : FS_String;
      Content : String) return Boolean;
   function Change_Dir
     (Exec : access Server_Record'Class;
      Dir  : FS_String) return Boolean;
   function Read_Dir
     (Exec       : access Server_Record'Class;
      Dir        : FS_String;
      Dirs_Only  : Boolean := False;
      Files_Only : Boolean := False)
      return GNAT.Strings.String_List;
   function Make_Dir
     (Exec      : access Server_Record'Class;
      Dir       : FS_String;
      Recursive : Boolean) return Boolean;
   procedure Copy_Dir
     (Exec    : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean);
   procedure Delete_Dir
     (Exec      : access Server_Record'Class;
      Dir       : FS_String;
      Recursive : Boolean;
      Success   : out Boolean);

end GNATCOLL.IO.Remote.Windows;
