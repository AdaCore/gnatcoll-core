with GNATCOLL.Filesystem; use GNATCOLL.Filesystem;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package GNATCOLL.VFS_Utils is

   --------------
   -- Wrappers --
   --------------

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

   procedure Open (Dir : out Dir_Type; Dir_Name : Filesystem_String);

   --  These subprograms wrap around their equivalents in Ada.Directories, and
   --  use Filesystem_String for better type safety.

   function Compose
     (Containing_Directory : Filesystem_String := "";
      Name                 : Filesystem_String;
      Extension            : Filesystem_String := "") return Filesystem_String;

end GNATCOLL.VFS_Utils;
