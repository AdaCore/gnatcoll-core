-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.VFS;              use GNATCOLL.VFS;
private with GNATCOLL.Path;

package GNATCOLL.VFS_Utils is

   --------------
   -- Wrappers --
   --------------

   Local_Host_Is_Case_Sensitive : constant Boolean;

   --  These subprograms wrap around their equivalents in System.OS_Lib, and
   --  use Filesystem_String for better type safety.

   function Normalize_Pathname
     (Name           : Filesystem_String;
      Directory      : Filesystem_String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True) return Filesystem_String;

   function Is_Absolute_Path (Name : Filesystem_String) return Boolean;

   function Is_Regular_File (Name : Filesystem_String) return Boolean;

   function Is_Directory (Name : Filesystem_String) return Boolean;

   procedure Copy_File
     (Name     : Filesystem_String;
      Pathname : Filesystem_String;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps);

   procedure Set_Writable (Name : Filesystem_String);

   procedure Set_Non_Writable (Name : Filesystem_String);

   procedure Set_Read_Only (Name : Filesystem_String) renames Set_Non_Writable;

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Filesystem_String_Access);

   function Locate_Exec_On_Path
     (Exec_Name : Filesystem_String) return Filesystem_String_Access;

   function Locate_Regular_File
     (File_Name : Filesystem_String;
      Path      : Filesystem_String) return Filesystem_String_Access;

   --  These subprograms wrap around their equivalents in
   --  GNAT.Directory_Operations, and use Filesystem_String for better type
   --  safety.

   function File_Extension (Path : Filesystem_String) return Filesystem_String;

   function Get_Current_Dir return Filesystem_String;

   function Dir_Name (Path : Filesystem_String) return Filesystem_String;

   function Base_Name
     (Path   : Filesystem_String;
      Suffix : Filesystem_String := "") return Filesystem_String;

   procedure Change_Dir (Dir_Name : Filesystem_String);

   function Format_Pathname
     (Path  : Filesystem_String;
      Style : Path_Style := System_Default) return Filesystem_String;

   function Name_As_Directory
     (Name : Filesystem_String) return Filesystem_String;

   procedure Open (Dir : out Dir_Type; Dir_Name : Filesystem_String);

   --  These subprograms wrap around their equivalents in Ada.Directories, and
   --  use Filesystem_String for better type safety.

   function Compose
     (Containing_Directory : Filesystem_String := "";
      Name                 : Filesystem_String;
      Extension            : Filesystem_String := "") return Filesystem_String;

   ------------------------------------
   -- Remote hosts handling of Files --
   ------------------------------------

   function Is_Case_Sensitive (Host : String) return Boolean;
   --  Tell if host's filesystem is case sensitive

   function File_Equal (F1, F2 : Filesystem_String; Host : String)
                        return Boolean;

private

   Local_Host_Is_Case_Sensitive : constant Boolean :=
                                    GNATCOLL.Path.Is_Case_Sensitive
                                      (GNATCOLL.Path.Local_FS);

end GNATCOLL.VFS_Utils;
