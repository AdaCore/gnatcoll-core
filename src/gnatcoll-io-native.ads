-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2009-2011, AdaCore                  --
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

pragma Ada_05;

private with GNATCOLL.Path;

package GNATCOLL.IO.Native is

   type Native_File_Record is new File_Record with private;

   function Create (Path : FS_String) return File_Access;

   ------------------------------
   --  Utilities for native FS --
   ------------------------------

   function Current_Dir return File_Access;
   --  Return the current directory

   function Home_Dir return File_Access;
   --  Return the home directory

   function Get_Tmp_Directory return File_Access;
   --  Return the directory that can be used to store temporary
   --  files on the filesystem.

   function Get_Logical_Drives return File_Array;

   Local_Root_Dir : constant File_Access;

   ---------------------------
   -- Overriden from parent --
   ---------------------------

   overriding function Dispatching_Create
     (Ref : not null access Native_File_Record;
      Full_Path : FS_String) return File_Access;
   overriding function To_UTF8
     (Ref  : not null access Native_File_Record;
      Path : FS_String) return String;
   overriding function From_UTF8
     (Ref : not null access Native_File_Record;
      Path : String) return FS_String;
   overriding function Is_Local
     (File : Native_File_Record) return Boolean;
   overriding function Get_FS
     (File : not null access Native_File_Record) return FS_Type;
   overriding procedure Resolve_Symlinks
     (File : not null access Native_File_Record);
   overriding function Is_Regular_File
     (File : not null access Native_File_Record) return Boolean;
   overriding function Is_Directory
     (File : not null access Native_File_Record) return Boolean;
   overriding function Is_Symbolic_Link
     (File : not null access Native_File_Record) return Boolean;
   overriding function File_Time_Stamp
     (File : not null access Native_File_Record) return Ada.Calendar.Time;
   overriding function Is_Writable
     (File : not null access Native_File_Record) return Boolean;
   overriding procedure Set_Writable
     (File  : not null access Native_File_Record;
      State : Boolean);
   overriding procedure Set_Readable
     (File  : not null access Native_File_Record;
      State : Boolean);
   overriding procedure Rename
     (From    : not null access Native_File_Record;
      Dest    : not null access Native_File_Record;
      Success : out Boolean);
   overriding procedure Copy
     (From    : not null access Native_File_Record;
      Dest    : FS_String;
      Success : out Boolean);
   overriding procedure Delete
     (File    : not null access Native_File_Record;
      Success : out Boolean);
   overriding function Read_Whole_File
     (File : not null access Native_File_Record)
      return GNAT.Strings.String_Access;
   overriding procedure Open_Write
     (File    : not null access Native_File_Record;
      Append  : Boolean := False;
      FD      : out GNAT.OS_Lib.File_Descriptor);
   overriding procedure Close
     (File    : not null access Native_File_Record;
      FD      : GNAT.OS_Lib.File_Descriptor;
      Success : out Boolean);
   overriding function Change_Dir
     (Dir : not null access Native_File_Record) return Boolean;
   overriding function Read_Dir
     (Dir            : not null access Native_File_Record;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False)
      return GNAT.Strings.String_List;
   overriding function Make_Dir
     (Dir : not null access Native_File_Record; Recursive : Boolean)
      return Boolean;
   overriding procedure Remove_Dir
     (Dir       : not null access Native_File_Record;
      Recursive : Boolean;
      Success   : out Boolean);
   overriding procedure Copy_Dir
     (From    : not null access Native_File_Record;
      Dest    : FS_String;
      Success : out Boolean);

   package Codec is
      function To_UTF8 (Path : FS_String) return String;
      function To_UTF8 (Path : Wide_String) return String;
      function From_UTF8 (Path : String) return FS_String;
      function From_UTF8 (Path : String) return Wide_String;
   end Codec;
   --  Codec to translate a path to/from utf-8

private

   type Native_File_Record is new File_Record with null record;

   Local_Root_Dir : constant File_Access := new Native_File_Record'
     (Ref_Count  => 1,
      Full       => new FS_String'
        (GNATCOLL.Path.Path (GNATCOLL.Path.Local_FS, "", "", "")),
      Normalized => null,
      Resolved   => True,
      Kind       => Directory);

end GNATCOLL.IO.Native;
