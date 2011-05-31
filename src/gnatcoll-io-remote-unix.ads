-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                Copyright (C) 2009-2011, AdaCore                   --
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

with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.Remote;       use GNATCOLL.Remote;

package GNATCOLL.IO.Remote.Unix is

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
     (Exec  : access Server_Record'Class;
      File  : FS_String;
      State : Boolean);
   procedure Set_Readable
     (Exec  : access Server_Record'Class;
      File  : FS_String;
      State : Boolean);
   procedure Rename
     (Exec    : access Server_Record'Class;
      From    : FS_String;
      Dest    : FS_String;
      Success : out Boolean);
   procedure Copy
     (Exec    : access Server_Record'Class;
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

end GNATCOLL.IO.Remote.Unix;
