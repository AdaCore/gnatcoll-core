-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2009-2010, AdaCore                   --
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

with GNATCOLL.Remote;

package GNATCOLL.IO.Remote is

   Remote_Config_Error : exception;

   type Remote_File_Record is new File_Record with private;
   type Remote_File_Access is access all Remote_File_Record'Class;

   function Create
     (Host      : String;
      Path      : FS_String;
      Normalize : Boolean) return File_Access;

   function Current_Dir (Host : String) return File_Access;
   function Home_Dir (Host : String) return File_Access;
   function Get_Tmp_Directory (Host : String) return File_Access;
   function Get_Logical_Drives (Host : String) return File_Array;
   function Locate_On_Path
     (Host : String; Base : FS_String) return File_Access;

   procedure Ensure_Initialized
     (File : not null access Remote_File_Record'Class);
   --  Same as above, raising an exception if the file cannot be initialized.

   function Get_Host (File : not null access Remote_File_Record)
     return String;

   ---------------------------
   -- Overriden from parent --
   ---------------------------

   overriding function Dispatching_Create
     (Ref : not null access Remote_File_Record;
      Full_Path : FS_String) return File_Access;
   overriding function To_UTF8
     (Ref : not null access Remote_File_Record;
      Path : FS_String) return String;
   overriding function From_UTF8
     (Ref : not null access Remote_File_Record;
      Path : String) return FS_String;
   overriding function Is_Local
     (File : Remote_File_Record) return Boolean;
   overriding function Get_FS
     (File : not null access Remote_File_Record) return FS_Type;
   overriding procedure Resolve_Symlinks
     (File : not null access Remote_File_Record);
   overriding function Is_Regular_File
     (File : not null access Remote_File_Record) return Boolean;
   overriding function Is_Directory
     (File : not null access Remote_File_Record) return Boolean;
   overriding function Is_Symbolic_Link
     (File : not null access Remote_File_Record) return Boolean;
   overriding function File_Time_Stamp
     (File : not null access Remote_File_Record) return Ada.Calendar.Time;
   overriding function Is_Writable
     (File : not null access Remote_File_Record) return Boolean;
   overriding procedure Set_Writable
     (File  : not null access Remote_File_Record;
      State : Boolean);
   overriding procedure Set_Readable
     (File  : not null access Remote_File_Record;
      State : Boolean);
   overriding procedure Rename
     (From    : not null access Remote_File_Record;
      Dest    : not null access Remote_File_Record;
      Success : out Boolean);
   overriding procedure Copy
     (From    : not null access Remote_File_Record;
      Dest    : FS_String;
      Success : out Boolean);
   overriding procedure Delete
     (File    : not null access Remote_File_Record;
      Success : out Boolean);
   overriding function Read_Whole_File
     (File : not null access Remote_File_Record)
      return GNAT.Strings.String_Access;
   overriding procedure Open_Write
     (File    : not null access Remote_File_Record;
      Append  : Boolean := False;
      FD      : out GNAT.OS_Lib.File_Descriptor);
   overriding procedure Close
     (File    : not null access Remote_File_Record;
      FD      : GNAT.OS_Lib.File_Descriptor;
      Success : out Boolean);
   overriding function Change_Dir
     (Dir : not null access Remote_File_Record) return Boolean;
   overriding function Read_Dir
     (Dir            : not null access Remote_File_Record;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False)
      return GNAT.Strings.String_List;
   overriding function Make_Dir
     (Dir : not null access Remote_File_Record) return Boolean;
   overriding procedure Remove_Dir
     (Dir       : not null access Remote_File_Record;
      Recursive : Boolean;
      Success   : out Boolean);
   overriding procedure Copy_Dir
     (From    : not null access Remote_File_Record;
      Dest    : FS_String;
      Success : out Boolean);
   --  See parent for documentation

   package Codec is
      function To_UTF8 (Path : FS_String) return String;
      function From_UTF8 (Path : String) return FS_String;
   end Codec;
   --  Codec to translate a path to/from utf-8.

private

   type Remote_File_Record is new File_Record with record
      Tmp_Host  : GNAT.Strings.String_Access;
      --  Host. Saved in case the below server is not resolved immediately.

      Tmp_Path  : FS_String_Access;
      --  Path used at creation, saved in case the below server is not resolved
      --  immediately.

      Tmp_Norm  : Boolean;
      --  Value used at creation to determine if Tmp_Path should be normalized

      Server    : GNATCOLL.Remote.Server_Access;
      --  The server on which the file commands are executed.

      Tmp_Name  : GNAT.OS_Lib.Temp_File_Name;
      --  Saved name for the temporary file used during Write operations.
   end record;

end GNATCOLL.IO.Remote;
