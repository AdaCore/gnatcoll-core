with Ada.Unchecked_Conversion;
with Ada.Directories; use Ada.Directories;

package body GNATCOLL.VFS_Utils is

   function Unchecked is new Ada.Unchecked_Conversion
     (String_Access, Filesystem_String_Access);

   ------------------------
   -- Normalize_Pathname --
   ------------------------

   function Normalize_Pathname
     (Name           : Filesystem_String;
      Directory      : Filesystem_String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True)
      return Filesystem_String
   is
   begin
      return +Normalize_Pathname
        (+Name, +Directory, Resolve_Links,
         Case_Sensitive);
   end Normalize_Pathname;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (Name : Filesystem_String) return Boolean is
   begin
      return Is_Absolute_Path (+Name);
   end Is_Absolute_Path;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : Filesystem_String) return Boolean is
   begin
      return Is_Regular_File (+Name);
   end Is_Regular_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Name : Filesystem_String) return Boolean is
   begin
      return Is_Directory (+Name);
   end Is_Directory;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Name     : Filesystem_String;
      Pathname : Filesystem_String;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps)
   is
   begin
      Copy_File (+Name, +Pathname, Success, Mode, Preserve);
   end Copy_File;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (Name : Filesystem_String) is
   begin
      Set_Writable (+Name);
   end Set_Writable;

   ----------------------
   -- Set_Non_Writable --
   ----------------------

   procedure Set_Non_Writable (Name : Filesystem_String) is
   begin
      Set_Non_Writable (+Name);
   end Set_Non_Writable;

   ----------------------
   -- Create_Temp_File --
   ----------------------

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Filesystem_String_Access)
   is
      R : String_Access;
   begin
      Create_Temp_File (FD, R);
      Name := Unchecked (R);
   end Create_Temp_File;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension
     (Path : Filesystem_String)
      return Filesystem_String
   is
   begin
      return +File_Extension (+Path);
   end File_Extension;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir return Filesystem_String is
   begin
      return +Get_Current_Dir;
   end Get_Current_Dir;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (Path : Filesystem_String) return Filesystem_String is
   begin
      return +Dir_Name (+Path);
   end Dir_Name;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (Path   : Filesystem_String;
      Suffix : Filesystem_String := "")
      return Filesystem_String
   is
   begin
      return +Base_Name (+Path, +Suffix);
   end Base_Name;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : Filesystem_String) is
   begin
      Change_Dir (+Dir_Name);
   end Change_Dir;

   ---------------------
   -- Format_Pathname --
   ---------------------

   function Format_Pathname
     (Path  : Filesystem_String;
      Style : Path_Style := System_Default) return Filesystem_String
   is
   begin
      return +Format_Pathname (+Path, Style);
   end Format_Pathname;

   ----------
   -- Open --
   ----------

   procedure Open (Dir : out Dir_Type; Dir_Name : Filesystem_String) is
   begin
      Open (Dir, +Dir_Name);
   end Open;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path
     (Exec_Name : Filesystem_String) return Filesystem_String_Access
   is
   begin
      return Convert (Locate_Exec_On_Path (+Exec_Name));
   end Locate_Exec_On_Path;

   -------------------------
   -- Locate_Regular_File --
   -------------------------

   function Locate_Regular_File
     (File_Name : Filesystem_String;
      Path      : Filesystem_String) return Filesystem_String_Access is
   begin
      return Convert (Locate_Regular_File (+File_Name, +Path));
   end Locate_Regular_File;

   -------------
   -- Compose --
   -------------

   function Compose
     (Containing_Directory : Filesystem_String := "";
      Name                 : Filesystem_String;
      Extension            : Filesystem_String := "")
      return Filesystem_String is
   begin
      return +Compose (+Containing_Directory, +Name, +Extension);
   end Compose;

end GNATCOLL.VFS_Utils;
