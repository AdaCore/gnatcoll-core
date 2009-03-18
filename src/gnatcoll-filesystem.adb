-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2006-2009, AdaCore                  --
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

with Ada.Directories;           use Ada.Directories;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;      use Interfaces.C, Interfaces.C.Strings;

with GNAT.Calendar;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.Filesystem.Windows;
with GNATCOLL.Filesystem.Unix;
with GNATCOLL.Mmap;             use GNATCOLL.Mmap;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

package body GNATCOLL.Filesystem is

   Windows_FS : aliased GNATCOLL.Filesystem.Windows.Windows_Filesystem_Record;
   Unix_FS    : aliased GNATCOLL.Filesystem.Unix.Unix_Filesystem_Record;
   --  The two global variables are used to return the local filesystem.
   --  Rather than allocate memory explicitly when the user queries the local
   --  filesystem, we return an access to one of those two variables. Obviously
   --  there are issues when used in a multitasking application, but this is
   --  general for the API in this package, and all users of Filesystem_Record
   --  must be duly protected anyway.

   --------------------------
   -- Get_Local_Filesystem --
   --------------------------

   function Get_Local_Filesystem return Filesystem_Access is
   begin
      if Directory_Separator = '\' then
         return Windows_FS'Access;
      else
         return Unix_FS'Access;
      end if;
   end Get_Local_Filesystem;

   ----------
   -- Free --
   ----------

   procedure Free (FS : in out Filesystem_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Filesystem_Record'Class, Filesystem_Access);
   begin
      if FS /= null then
         Free (FS.all);
         Unchecked_Free (FS);
      end if;
   end Free;

   ----------------
   -- Is_Subtree --
   ----------------

   function Is_Subtree
     (FS        : Filesystem_Record;
      Directory : Filesystem_String;
      Full_Path : Filesystem_String) return Boolean is
   begin
      --  Path length shall be greater or equal to directory length
      if Directory'Length > Full_Path'Length then
         return False;
      end if;

      --  Do not try to compare last character: on VMS, you will compare
      --  a closing bracket with a dot (disk:[path] with disk:[path.subpath])
      return Equal
        (+Full_Path
           (Full_Path'First .. Full_Path'First + Directory'Length - 1),
         +Directory,
         Is_Case_Sensitive (Filesystem_Record'Class (FS)));
   end Is_Subtree;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension
     (FS   : Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String
   is
      pragma Unreferenced (FS);
   begin
      for J in reverse Path'Range loop
         if Path (J) = '.' then
            return Path (J .. Path'Last);
         end if;
      end loop;
      return "";
   end File_Extension;

   ------------
   -- Concat --
   ------------

   function Concat
     (FS   : Filesystem_Record;
      Root : Filesystem_String;
      Sub  : Filesystem_String) return Filesystem_String is
   begin
      --  If Root is empty, return Sub. Else, a double slash will be used,
      --  which can be badly interpreted (on windows, this may furnish a
      --  \\machine\service kind of path).
      if Root = "" then
         return Sub;
      else
         return Ensure_Directory (Filesystem_Record'Class (FS), Root) & Sub;
      end if;
   end Concat;

   ---------------------------
   -- Multi_Unit_Index_Char --
   ---------------------------

   function Multi_Unit_Index_Char
     (FS : Filesystem_Record) return Character
   is
      pragma Unreferenced (FS);
   begin
      return '~';
   end Multi_Unit_Index_Char;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (FS     : Filesystem_Record;
      Path   : Filesystem_String;
      Suffix : Filesystem_String := "") return Filesystem_String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = Dir_Sep (Filesystem_Record'Class (FS)) then
            if Equal (+Path (Path'Last - Suffix'Length + 1 .. Path'Last),
                      +Suffix,
                      Is_Case_Sensitive (Filesystem_Record'Class (FS))) then
               return Path (J + 1 .. Path'Last - Suffix'Length);

            else
               return Path (J + 1 .. Path'Last);
            end if;
         end if;
      end loop;

      return Path;
   end Base_Name;

   -------------------
   -- Base_Dir_Name --
   -------------------

   function Base_Dir_Name
     (FS   : Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String
   is
      First : Natural := 0;
      Root  : constant Filesystem_String := FS.Get_Root (Path);
   begin
      if Path = Root then
         return Path;

      elsif Path'Length > 1
        and then Path (Path'Last) = Dir_Sep (Filesystem_Record'Class (FS))
      then
         return FS.Base_Dir_Name (Path (Path'First .. Path'Last - 1));

      else
         return Base_Name (FS, Path);
      end if;
   end Base_Dir_Name;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name
     (FS   : Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = Dir_Sep (Filesystem_Record'Class (FS)) then
            return Path (Path'First .. J);
         end if;
      end loop;

      return "";
   end Dir_Name;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (FS   : Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String is
   begin
      if Path (Path'Last) = Dir_Sep (Filesystem_Record'Class (FS)) then
         return Dir_Name (FS, Path (Path'First .. Path'Last - 1));
      else
         return Dir_Name (FS, Path);
      end if;
   end Get_Parent;

   ----------------------
   -- Ensure_Directory --
   ----------------------

   function Ensure_Directory
     (FS   : Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String is
   begin
      if Path'Length = 0
        or else Path (Path'Last) /= Dir_Sep (Filesystem_Record'Class (FS))
      then
         return Path & Dir_Sep (Filesystem_Record'Class (FS));
      else
         return Path;
      end if;
   end Ensure_Directory;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (FS   : Filesystem_Record;
      Path : Filesystem_String) return Filesystem_String
   is
      Last_Dir      : Natural;
      Dir_Separator : constant Character :=
                        Dir_Sep (Filesystem_Record'Class (FS));
   begin
      if Is_Local (Filesystem_Record'Class (FS)) then
         return Normalize_Pathname (Path, Case_Sensitive => False);
      else
         Last_Dir := Path'First;

         for J in Path'Range loop
            if Path (J) = Dir_Separator then
               if J < Path'Last - 3
                 and then Path (J .. J + 3) =
                           Dir_Separator & ".." & Dir_Separator
               then
                  return Normalize
                    (FS,
                     Path
                       (Path'First .. Last_Dir) & Path (J + 4 .. Path'Last));

               elsif J < Path'Last - 2
                 and then Path (J .. J + 2) =
                           Dir_Separator & '.' & Dir_Separator
               then
                  return Normalize
                    (FS, Path (Path'First .. J) & Path (J + 3 .. Path'Last));
               end if;

               Last_Dir := J;
            end if;
         end loop;

         return Path;
      end if;
   end Normalize;

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir (FS : Filesystem_Record) return Filesystem_String is
      HOME : GNAT.Strings.String_Access := Getenv ("HOME");
   begin
      if HOME = null then
         return Get_Root (Filesystem_Record'Class (FS), "");
      else
         declare
            Result : constant Filesystem_String := +HOME.all;
         begin
            Free (HOME);
            return Result;
         end;
      end if;
   end Home_Dir;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Is_Regular_File (Local_Full_Name);
   end Is_Regular_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return GNAT.Strings.String_Access
   is
      pragma Unreferenced (FS);
   begin
      return Read_Whole_File (+Local_Full_Name);
   end Read_File;

   ------------
   -- Delete --
   ------------

   function Delete
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
      Success : Boolean;
   begin
      Delete_File (+Local_Full_Name, Success);
      return Success;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Is_Writable_File (+Local_Full_Name);
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
      D : Dir_Type;

   begin
      if Local_Full_Name (Local_Full_name'First .. Local_Full_name'First + 1)
        = "\\"
      then
      --  ??? Strange enough, it appears that tentatively opening/closing a
      --  directory is more efficient and reliable than calling
      --  System.OS_Lib.Is_Directory who in turn calls stat, buggy at least on
      --  windows as far as network directories are concerned.

      --  Of course, doing so, we take the risk of having a directory with
      --  no proper read rights reported as regular file ...
         GNAT.Directory_Operations.Open (D, +Local_Full_Name);
         GNAT.Directory_Operations.Close (D);

         return True;
      end if;

      return Is_Directory (+Local_Full_Name);

   exception
      when GNAT.Directory_Operations.Directory_Error =>
         return False;
   end Is_Directory;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return Ada.Calendar.Time
   is
      pragma Unreferenced (FS);
      T      : constant OS_Time := File_Time_Stamp (+Local_Full_Name);
      Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type;
   begin
      if T = Invalid_Time then
         return GNATCOLL.Utils.No_Time;
      end if;

      GM_Split (T, Year, Month, Day, Hour, Minute, Second);

      return GNAT.Calendar.Time_Of
        (Year   => Year,
         Month  => Month,
         Day    => Day,
         Hour   => Hour,
         Minute => Minute,
         Second => Second);
   end File_Time_Stamp;

   -----------
   -- Write --
   -----------

   procedure Write
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String;
      Temporary_File  : Filesystem_String;
      Append          : Boolean := False)
   is
      Success : Boolean;
      pragma Unreferenced (FS, Success);
   begin
      if Append then
         Copy_File
           (Temporary_File,
            Pathname => Local_Full_Name,
            Success  => Success,
            Mode     => GNAT.OS_Lib.Append);
      else
         Copy_File
           (Temporary_File,
            Pathname => Local_Full_Name,
            Success  => Success,
            Mode     => Overwrite);
      end if;
   end Write;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String;
      Writable        : Boolean)
   is
      pragma Unreferenced (FS);
   begin
      if Writable then
         Set_Writable (Local_Full_Name);
      else
         Set_Read_Only (Local_Full_Name);
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String;
      Readable        : Boolean)
   is
      pragma Unreferenced (FS);
      procedure Internal (File : Filesystem_String; Set : Integer);
      pragma Import (C, Internal, "__gnatcoll_set_readable");

   begin
      Internal (Local_Full_Name & ASCII.NUL, Boolean'Pos (Readable));
   end Set_Readable;

   ----------------
   -- Remove_Dir --
   ----------------

   function Remove_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : Filesystem_String;
      Recursive      : Boolean) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      Remove_Dir (+Local_Dir_Name, Recursive);
      return True;
   exception
      when Directory_Error =>
         return False;
   end Remove_Dir;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : Filesystem_String;
      Dirs_Only      : Boolean := False;
      Files_Only     : Boolean := False) return GNAT.Strings.String_List
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.Strings.String_List, String_List_Access);

      Nb_Files : Natural := 0;
      Tmp      : String_List_Access;
      F_Array  : String_List_Access;
      D        : Dir_Type;
      F_Name   : String (1 .. 10000);
      Last     : Natural;

   begin
      --  let's start with 128 items
      F_Array := new GNAT.Strings.String_List (1 .. 128);

      Open (D, +Local_Dir_Name);

      loop
         Read (D, F_Name, Last);

         exit when Last = 0;

         declare
            Simple : constant Filesystem_String := +F_Name (1 .. Last);
         begin
            if Simple /= "."
              and then Simple /= ".."
              and then (not Dirs_Only
                         or else FS.Is_Directory (Local_Dir_Name & Simple))
              and then (not Files_Only
                         or else FS.Is_Regular_File (Local_Dir_name & Simple))
            then

               Nb_Files := Nb_Files + 1;

               --  Array too small, let's double it
               if Nb_Files > F_Array'Last then
                  Tmp := F_Array;
                  F_Array := new GNAT.Strings.String_List (1 .. Tmp'Last * 2);
                  F_Array (1 .. Tmp'Last) := Tmp.all;
                  Unchecked_Free (Tmp);
               end if;

               F_Array (Nb_Files) := new String'(+Simple);
            end if;
         end;
      end loop;

      Close (D);

      Tmp := F_Array;

      declare
         F_Array : GNAT.Strings.String_List (1 .. Nb_Files);
      begin
         F_Array := Tmp (1 .. Nb_Files);
         Unchecked_Free (Tmp);
         return F_Array;
      end;

   exception
      when others =>
         Close (D);
         return (1 .. 0 => null);
   end Read_Dir;

   --------------
   -- Make_Dir --
   --------------

   function Make_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      Create_Path (+Local_Dir_Name);
      return True;
   exception
      when others =>
         return False;
   end Make_Dir;

   ------------
   -- Rename --
   ------------

   function Rename
     (FS              : Filesystem_Record;
      From_Local_Name : Filesystem_String;
      To_Local_Name   : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
      Success : Boolean;
   begin
      Rename_File (+From_Local_Name, +To_Local_Name, Success);
      return Success;
   end Rename;

   ----------
   -- Copy --
   ----------

   function Copy
     (FS              : Filesystem_Record;
      From_Local_Name : Filesystem_String;
      To_Local_Name   : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
      Success : Boolean;
   begin
      Copy_File (From_Local_Name, To_Local_Name, Success,
                 Mode => Overwrite, Preserve => Full);
      return Success;
   exception
      when others =>
         return False;
   end Copy;

   --------------
   -- Copy_Dir --
   --------------

   function Copy_Dir
     (FS              : Filesystem_Record;
      From_Local_Name : Filesystem_String;
      To_Local_Name   : Filesystem_String) return Boolean
   is
      From        : constant Filesystem_String :=
                      Ensure_Directory
                        (Filesystem_Record'Class (FS), From_Local_Name);
      Target      : constant Filesystem_String :=
                      Ensure_Directory
                        (Filesystem_Record'Class (FS), To_Local_Name);
      Files_Array : String_List  :=
                      Read_Dir (Filesystem_Record'Class (FS), From);
      Success     : Boolean;
   begin
      Success := Is_Directory (Filesystem_Record'Class (FS), Target)
        or else Make_Dir (Filesystem_Record'Class (FS), Target);

      if Success then
         for F in Files_Array'Range loop
            if Is_Directory (From & (+Files_Array (F).all)) then
               if not Copy_Dir
                 (Filesystem_Record'Class (FS),
                  From & (+Files_Array (F).all),
                  Target & (+Files_Array (F).all))
               then
                  Success := False;
                  exit;
               end if;

            else
               if not Copy
                 (Filesystem_Record'Class (FS),
                  From & (+Files_Array (F).all),
                  Target)
               then
                  Success := False;
                  exit;
               end if;
            end if;
         end loop;
      end if;

      Free (Files_Array);
      return Success;

   exception
      when others =>
         Free (Files_Array);
         return False;
   end Copy_Dir;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link
     (FS              : Filesystem_Record;
      Local_Full_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Is_Symbolic_Link (+Local_Full_Name);
   end Is_Symbolic_Link;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (FS             : Filesystem_Record;
      Local_Dir_Name : Filesystem_String) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      Change_Dir (Local_Dir_Name);
      return True;
   exception
      when others =>
         return False;
   end Change_Dir;

   -----------------------
   -- Get_Tmp_Directory --
   -----------------------

   function Get_Tmp_Directory
     (FS : Filesystem_Record) return Filesystem_String
   is
      function Internal return chars_ptr;
      pragma Import (C, Internal, "__gnatcoll_get_tmp_dir");

      procedure c_free (C : chars_ptr);
      pragma Import (C, c_free, "free");

      C_Str : chars_ptr := Internal;
      Str   : constant Filesystem_String :=
                +GNAT.Directory_Operations.Format_Pathname
                  (To_Ada (Value (C_Str)));
   begin
      --  Since the allocation was done in C (strdup), we use directly the
      --  C version of free. This is probably more reliable, and more
      --  importantly, this works correctly with our own version of
      --  s-memory.adb (when GPS_MEMORY_MONITOR=1)
      c_free (C_Str);
      return Ensure_Directory (Filesystem_Record'Class (FS), Str);
   end Get_Tmp_Directory;

   -----------------------
   -- Locale_To_Display --
   -----------------------

   function Locale_To_Display
     (FS : Filesystem_Record; Name : Filesystem_String) return String is
   begin
      if FS.Locale_To_Display_Encoder /= null then
         return FS.Locale_To_Display_Encoder (Name);
      else
         return +Name;
      end if;
   end Locale_To_Display;

   -----------------------------------
   -- Set_Locale_To_Display_Encoder --
   -----------------------------------

   procedure Set_Locale_To_Display_Encoder
     (FS      : in out Filesystem_Record;
      Encoder : Encoder_Function) is
   begin
      FS.Locale_To_Display_Encoder := Encoder;
   end Set_Locale_To_Display_Encoder;

   ---------
   -- "+" --
   ---------

   function "+" (S : Filesystem_String) return String is
   begin
      return String (S);
   end "+";

   function "+" (S : String) return Filesystem_String is
   begin
      return Filesystem_String (S);
   end "+";

end GNATCOLL.Filesystem;
