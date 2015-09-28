------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Tags;                  use Ada.Tags;
with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with System;

with GNAT.Heap_Sort;            use GNAT.Heap_Sort;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.IO;               use GNATCOLL.IO;
with GNATCOLL.IO.Remote;        use GNATCOLL.IO.Remote;
with GNATCOLL.Path;             use GNATCOLL.Path;
with GNATCOLL.Remote;           use GNATCOLL.Remote;
with GNATCOLL.Remote.Db;        use GNATCOLL.Remote.Db;
with GNATCOLL.VFS_Types;        use GNATCOLL.VFS_Types;

package body GNATCOLL.VFS is

   Empty_String : aliased Filesystem_String := "";

   Handle_Symbolic_Links : Boolean := GNAT.OS_Lib.Directory_Separator /= '\';
   --  If this variable is False, we assume there is never any symbolic link,
   --  and thus we do not spend time resolving them.

   function "+" (S : Filesystem_String) return FS_String;
   function "+" (S : FS_String) return Filesystem_String;
   function "+" (S : FS_String) return String;
   pragma Inline ("+");
   --  FS_String and Filesystem_String are identical in their intent.
   --  We just have a visibility issue as it really should be defined in
   --  VFS's spec, so can't be used by underneath packages (GNATCOLL.IO and
   --  GNATCOLL.Path)

   function "+" is new Ada.Unchecked_Conversion
     (FS_String_Access, Filesystem_String_Access);

   procedure Ensure_Normalized
     (File             : Virtual_File'Class;
      Resolve_Symlinks : Boolean);
   --  Make sure that File.Value.Normalized is filled

   ---------
   -- "+" --
   ---------

   function "+" (S : Filesystem_String) return FS_String is
   begin
      return FS_String (S);
   end "+";

   function "+" (S : FS_String) return Filesystem_String is
   begin
      return Filesystem_String (S);
   end "+";

   function "+" (S : FS_String) return String is
   begin
      return String (S);
   end "+";

   function "+" (S : Filesystem_String) return String is
   begin
      return String (S);
   end "+";

   function "+" (S : String) return Filesystem_String is
   begin
      return Filesystem_String (S);
   end "+";

   ---------
   -- "=" --
   ---------

   function Equal (S1, S2 : Filesystem_String) return Boolean is
   begin
      return Equal (+S1, +S2, Is_Case_Sensitive (Local_FS));
   end Equal;

   ---------
   -- "=" --
   ---------

   function "=" (File1, File2 : Virtual_File) return Boolean is
   begin
      --  Test for the same pointer to actual value (or both null)
      if File1.Value = File2.Value then
         return True;

      --  Test if one of the values is null
      elsif File1.Value = null
        or else File2.Value = null
        or else File1.Value.all'Tag /= File2.Value.all'Tag
      then
         return False;

      --  Finally, we test the normalized paths
      else
         Ensure_Normalized (File1, Resolve_Symlinks => True);
         Ensure_Normalized (File2, Resolve_Symlinks => True);

         --  We also take care of potential trailing dir separator by enforcing
         --  them

         return Equal
           (File1.Value.Get_FS,
            Ensure_Directory
              (File1.Value.Get_FS, File1.Value.Normalized_And_Resolved.all),
            Ensure_Directory
              (File1.Value.Get_FS, File2.Value.Normalized_And_Resolved.all));
      end if;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (File1, File2 : Virtual_File) return Boolean is
      C1, C2         : Character;
      Ind1, Ind2     : Integer;
      Case_Sensitive : Boolean;
   begin
      if File1 = File2 then
         return False;

      elsif File1.Value = null then
         return True;

      elsif File2.Value = null then
         return False;

      elsif Is_Local (File1.Value.all) /= Is_Local (File2.Value.all) then
         return Is_Local (File1.Value.all);

      elsif not Is_Local (File1.Value.all)
        and then Get_Host (File1) /= Get_Host (File2)
      then
         return Get_Host (File1) < Get_Host (File2);

      else
         Case_Sensitive := Is_Case_Sensitive (File1.Value.Get_FS)
           and then Is_Case_Sensitive (File2.Value.Get_FS);

         Ensure_Normalized (File1, Resolve_Symlinks => True);
         Ensure_Normalized (File2, Resolve_Symlinks => True);

         if Case_Sensitive then
            return File1.Value.Normalized_And_Resolved.all <
              File2.Value.Normalized_And_Resolved.all;
         else
            Ind1 := File1.Value.Normalized_And_Resolved'First;
            Ind2 := File2.Value.Normalized_And_Resolved'First;

            for C in 1 .. File1.Value.Normalized_And_Resolved'Length loop
               if Ind2 > File2.Value.Normalized_And_Resolved'Last then
                  return False;
               end if;

               C1 := To_Lower (File1.Value.Normalized_And_Resolved (Ind1));
               C2 := To_Lower (File2.Value.Normalized_And_Resolved (Ind2));

               if C1 < C2 then
                  return True;
               elsif C1 > C2 then
                  return False;
               end if;

               Ind1 := Ind1 + 1;
               Ind2 := Ind2 + 1;
            end loop;

            return True;
         end if;
      end if;
   end "<";

   ------------
   -- Create --
   ------------

   function Create
     (Full_Filename : Filesystem_String;
      Host          : String := Local_Host;
      Normalize     : Boolean := False) return Virtual_File
   is
      function Internal_Get_Path (FS : FS_Type) return FS_String;
      --  Get Full_Filename according to Normalize setting

      -----------------------
      -- Internal_Get_Path --
      -----------------------

      function Internal_Get_Path (FS : FS_Type) return FS_String is
      begin
         if not Normalize then
            return +Full_Filename;
         end if;

         return GNATCOLL.Path.Normalize (FS, +Full_Filename);
      end Internal_Get_Path;

   begin
      if Full_Filename = "" then
         return No_File;
      end if;

      if Host = Local_Host then
         return (Ada.Finalization.Controlled with
                 Value => GNATCOLL.IO.Native.Create
                   (Internal_Get_Path (GNATCOLL.Path.Local_FS)));
      else
         return (Ada.Finalization.Controlled with
                 Value => GNATCOLL.IO.Remote.Create
                   (Host, +Full_Filename, Normalize));
      end if;
   end Create;

   ----------------------
   -- Create_From_UTF8 --
   ----------------------

   function Create_From_UTF8
     (Full_Filename : String;
      Host          : String := Local_Host;
      Normalize     : Boolean := False) return Virtual_File is
   begin
      if Host = Local_Host then
         return Create
           (+GNATCOLL.IO.Native.Codec.From_UTF8 (Full_Filename),
            Normalize => Normalize);
      else
         return Create
           (+GNATCOLL.IO.Remote.Codec.From_UTF8 (Full_Filename), Host,
            Normalize => Normalize);
      end if;
   end Create_From_UTF8;

   ----------------------
   -- Create_From_Base --
   ----------------------

   function Create_From_Base
     (Base_Name : Filesystem_String;
      Base_Dir  : Filesystem_String := "";
      Host      : String := Local_Host) return Virtual_File
   is
      FS : FS_Type;
   begin
      if Host = Local_Host then
         FS := Local_FS;
      else
         FS := Shell_FS (Get_Server (Host).all);
      end if;

      declare
         The_Name : constant FS_String := Path.From_Unix (FS, +Base_Name);
         The_Dir  : constant FS_String := Path.From_Unix (FS, +Base_Dir);
      begin
         if Is_Absolute_Path (FS, The_Name) then
            return Create (+The_Name, Host);

         elsif The_Dir /= "" then
            return Create
              (+(Path.Ensure_Directory (FS, The_Dir) & The_Name), Host);

         else
            return Create_From_Dir
              (Get_Current_Dir (Host), +The_Name);
         end if;
      end;
   end Create_From_Base;

   ---------------------
   -- Create_From_Dir --
   ---------------------

   function Create_From_Dir
     (Dir       : Virtual_File;
      Base_Name : Filesystem_String;
      Normalize : Boolean := False) return Virtual_File
   is
   begin
      if Dir.Value = null then
         raise VFS_Invalid_File_Error;
      end if;

      Dir.Ensure_Directory;

      return
        (Ada.Finalization.Controlled with
         Dispatching_Create
           (Ref       => Dir.Value,
            Full_Path =>
               FS_String (Dir.Full_Name (Normalize).all) &
               From_Unix (Dir.Value.Get_FS, +Base_Name)));
   end Create_From_Dir;

   --------------------
   -- Locate_On_Path --
   --------------------

   function Locate_On_Path
     (Base_Name : Filesystem_String;
      Host      : String := Local_Host) return Virtual_File
   is
      Name : GNAT.OS_Lib.String_Access;
      Ret  : Virtual_File;
      use type GNAT.OS_Lib.String_Access;

   begin
      if Host = Local_Host then
         if Is_Absolute_Path (Local_FS, +Base_Name) then
            return Create (Base_Name);
         end if;

         Name := GNAT.OS_Lib.Locate_Exec_On_Path (+Base_Name);

         if Name = null then
            return No_File;
         else
            Ret := Create (+Name.all);
            GNAT.OS_Lib.Free (Name);
            return Ret;
         end if;

      else
         declare
            Int : constant GNATCOLL.IO.File_Access :=
                    GNATCOLL.IO.Remote.Locate_On_Path (Host, +Base_Name);
         begin
            if Int = null then
               return No_File;
            else
               return (Ada.Finalization.Controlled with Int);
            end if;
         end;
      end if;
   end Locate_On_Path;

   -----------------------
   -- Get_Tmp_Directory --
   -----------------------

   function Get_Tmp_Directory (Host : String := Local_Host) return Virtual_File
   is
   begin
      if Host = Local_Host then
         return (Ada.Finalization.Controlled with
                 GNATCOLL.IO.Native.Get_Tmp_Directory);
      else
         return (Ada.Finalization.Controlled with
                 GNATCOLL.IO.Remote.Get_Tmp_Directory (Host));
      end if;
   end Get_Tmp_Directory;

   ------------------------
   -- Get_Home_Directory --
   ------------------------

   function Get_Home_Directory
     (Host : String := Local_Host) return Virtual_File
   is
   begin
      if Host = Local_Host then
         return (Ada.Finalization.Controlled with
                 GNATCOLL.IO.Native.Home_Dir);
      else
         return (Ada.Finalization.Controlled with
                 GNATCOLL.IO.Remote.Home_Dir (Host));
      end if;
   end Get_Home_Directory;

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   function Get_Logical_Drives
     (Host : String := Local_Host) return File_Array_Access
   is
      function Get_IO_List return GNATCOLL.IO.File_Array;
      --  Get the IO list depending on Host

      -----------------
      -- Get_IO_List --
      -----------------

      function Get_IO_List return GNATCOLL.IO.File_Array is
      begin
         if Host = Local_Host then
            return GNATCOLL.IO.Native.Get_Logical_Drives;
         else
            return GNATCOLL.IO.Remote.Get_Logical_Drives (Host);
         end if;
      end Get_IO_List;

      List : constant GNATCOLL.IO.File_Array := Get_IO_List;
      Ret  : constant File_Array_Access := new File_Array (1 .. List'Length);

   begin
      for J in List'Range loop
         Ret (J - List'First + 1) :=
           (Ada.Finalization.Controlled with
            List (J));
         Ret (J - List'First + 1).Value.Kind := Directory;
      end loop;

      return Ret;
   end Get_Logical_Drives;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Host : String := Local_Host) return Virtual_File
   is
      Ret : Virtual_File;
   begin
      if Host = Local_Host then
         Ret := (Ada.Finalization.Controlled with
                 GNATCOLL.IO.Native.Current_Dir);
      else
         Ret := (Ada.Finalization.Controlled with
                 GNATCOLL.IO.Remote.Current_Dir (Host));
      end if;

      Ret.Value.Kind := Directory;
      return Ret;
   end Get_Current_Dir;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (File      : Virtual_File;
      Suffix    : Filesystem_String := "";
      Normalize : Boolean := False)
      return Filesystem_String is
   begin
      if File.Value = null then
         return "";
      end if;

      if Normalize then
         Ensure_Normalized (File, Resolve_Symlinks => True);
         return +Base_Name
           (File.Value.Get_FS,
            File.Value.Normalized_And_Resolved.all,
            +Suffix);
      else
         return +Base_Name (File.Value.Get_FS, File.Value.Full.all, +Suffix);
      end if;
   end Base_Name;

   -------------------
   -- Base_Dir_Name --
   -------------------

   function Base_Dir_Name (File : Virtual_File) return Filesystem_String is
   begin
      if File.Value = null then
         return "";
      end if;

      return +Base_Dir_Name (File.Value.Get_FS, File.Value.Full.all);
   end Base_Dir_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (File : Virtual_File; Normalize : Boolean := False)
      return Filesystem_String is
   begin
      return File.Full_Name (Normalize).all;
   end Full_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (File          : Virtual_File;
      Normalize     : Boolean := False;
      Resolve_Links : Boolean := False)
      return Cst_Filesystem_String_Access is
   begin
      if File.Value = null then
         return Empty_String'Access;

      elsif File.Value.Full /= null and then Normalize then
         Ensure_Normalized (File, Resolve_Links);
         if Resolve_Links then
            return Cst_Filesystem_String_Access
              (+File.Value.Normalized_And_Resolved.all'Access);
         else
            return Cst_Filesystem_String_Access
              (+File.Value.Normalized.all'Access);
         end if;

      elsif File.Value.Full = null
        and then Get_Host (File) /= Local_Host
      then
         GNATCOLL.IO.Remote.Ensure_Initialized
           (GNATCOLL.IO.Remote.Remote_File_Access (File.Value));
         return Cst_Filesystem_String_Access (+File.Value.Full.all'Access);

      else
         return Cst_Filesystem_String_Access (+File.Value.Full.all'Access);
      end if;
   end Full_Name;

   --------------------
   -- Full_Name_Hash --
   --------------------

   function Full_Name_Hash
     (Key : Virtual_File) return Ada.Containers.Hash_Type is
   begin
      if Key.Value = null then
         return 0;
      end if;

      Ensure_Normalized (Key, Resolve_Symlinks => True);

      if Is_Case_Sensitive (Key.Value.Get_FS) then
         return Ada.Strings.Hash (+Key.Value.Normalized_And_Resolved.all);
      else
         return Ada.Strings.Hash_Case_Insensitive
           (+Key.Value.Normalized_And_Resolved.all);
      end if;
   end Full_Name_Hash;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (File : Virtual_File) return Filesystem_String is
   begin
      if File.Value = null then
         return "";
      end if;

      return +Dir_Name (File.Value.Get_FS, File.Value.Full.all);
   end Dir_Name;

   ---------
   -- Dir --
   ---------

   function Dir (File : Virtual_File) return Virtual_File is
   begin
      if File.Value = null then
         return VFS.No_File;
      end if;

      if Is_Dir_Name (File.Value.Get_FS, File.Value.Full.all) then
         return File;
      else
         return
           (Ada.Finalization.Controlled with
            Value => Dispatching_Create
              (File.Value,
               Dir_Name
                 (File.Value.Get_FS, File.Value.Full.all)));
      end if;
   end Dir;

   -----------------------
   -- Display_Full_Name --
   -----------------------

   function Display_Full_Name
     (File      : Virtual_File;
      Normalize : Boolean := False) return String is
   begin
      if File.Value = null then
         return "";

      else
         return File.Value.To_UTF8
           (FS_String (File.Full_Name (Normalize).all));
      end if;
   end Display_Full_Name;

   -----------------------
   -- Display_Base_Name --
   -----------------------

   function Display_Base_Name
     (File   : Virtual_File;
      Suffix : Filesystem_String := "") return String is
   begin
      if File.Value = null then
         return "";

      else
         return File.Value.To_UTF8 (+File.Base_Name (Suffix));
      end if;
   end Display_Base_Name;

   ---------------------
   -- Locale_Dir_Name --
   ---------------------

   function Display_Dir_Name (File : Virtual_File) return String is
   begin
      if File.Value = null then
         return "";

      else
         return File.Value.To_UTF8 (+File.Dir_Name);
      end if;
   end Display_Dir_Name;

   ---------------------------
   -- Display_Base_Dir_Name --
   ---------------------------

   function Display_Base_Dir_Name (File : Virtual_File) return String is
   begin
      if File.Value = null then
         return "";
      else
         return File.Value.To_UTF8 (+File.Base_Dir_Name);
      end if;
   end Display_Base_Dir_Name;

   --------------------------
   -- Unix_Style_Full_Name --
   --------------------------

   function Unix_Style_Full_Name
     (File         : Virtual_File;
      Cygwin_Style : Boolean := False;
      Normalize    : Boolean := False;
      Casing       : Boolean := False) return Filesystem_String
   is
      FS : FS_Type;

      function Auto_Case (Str : FS_String) return Filesystem_String;
      function Auto_Case (Str : FS_String) return Filesystem_String is
      begin
         if not Casing or else Is_Case_Sensitive (FS) then
            return +Str;
         else
            return +To_Lower (+Str);
         end if;
      end Auto_Case;

   begin
      if File.Value = null then
         return "";
      else
         FS := File.Value.Get_FS;

         if Normalize then
            return Auto_Case (To_Unix
              (FS,
               +Full_Name
                 (File, Normalize => Normalize, Resolve_Links => True).all,
               Cygwin_Style));
         else
            return Auto_Case (To_Unix (FS, File.Value.Full.all, Cygwin_Style));
         end if;
      end if;
   end Unix_Style_Full_Name;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path
     (File : Virtual_File;
      From : Virtual_File) return Filesystem_String
   is
   begin
      --  Obviously, we need from to be a directory...
      if not Is_Directory (From) then
         return File.Full_Name;
      end if;

      if From.Value = null
        or else File.Value = null
        or else File.Value'Tag /= From.Value'Tag
      then
         return File.Full_Name;
      end if;

      return Filesystem_String (Relative_Path
        (File.Value.Get_FS,
         Ref  => FS_String (From.Full_Name.all),
         Path => FS_String (File.Full_Name.all)));
   end Relative_Path;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix
     (File : Virtual_File; Suffix : Filesystem_String) return Boolean is
   begin
      return File.Value /= null
        and then File.Full_Name.all'Length >= Suffix'Length
        and then Equal
          (File.Value.Get_FS,
           File.Value.Full
             (File.Value.Full'Last - Suffix'Length + 1
                .. File.Value.Full'Last),
           +Suffix);
   end Has_Suffix;

   ---------------
   -- To_Remote --
   ---------------

   function To_Remote
     (File : Virtual_File; To_Host : String) return Virtual_File
   is
   begin
      if File.Value = null then
         return No_File;
      end if;

      if File.Get_Host = To_Host then
         return File;
      end if;

      if not Is_Configured (To_Host) then
         raise VFS_Remote_Config_Error;
      end if;

      declare
         Server : constant Server_Access := Get_Server (To_Host);
         Local  : Virtual_File;
         Remote : Virtual_File := No_File;

      begin
         for J in 1 .. Nb_Mount_Points (Server.Nickname) loop
            Local := Create (+Get_Mount_Point_Local_Root (Server.Nickname, J));

            if Local.Is_Parent (File) then
               Remote := Create
                 (+Get_Mount_Point_Host_Root (Server.Nickname, J),
                  To_Host);
               exit;
            end if;
         end loop;

         if Remote = No_File then
            --  Simple conversion
            return Convert (File, To_Host);
         else
            return Convert (File, Local, Remote);
         end if;
      end;
   end To_Remote;

   --------------
   -- To_Local --
   --------------

   function To_Local (File : Virtual_File) return Virtual_File is
   begin
      if File.Value = null then
         return No_File;
      end if;

      if File.Is_Local then
         return File;
      end if;

      if not Is_Configured (File.Get_Host) then
         raise VFS_Remote_Config_Error;
      end if;

      declare
         Server : constant Server_Access := Get_Server (File.Get_Host);
         Local  : Virtual_File := No_File;
         Remote : Virtual_File;

      begin
         for J in 1 .. Nb_Mount_Points (Server.Nickname) loop
            Remote := Create
              (+Get_Mount_Point_Host_Root (Server.Nickname, J),
               File.Get_Host);

            if Remote.Is_Parent (File) then
               Local := Create
                 (+Get_Mount_Point_Local_Root (Server.Nickname, J));
               exit;
            end if;
         end loop;

         if Local = No_File then
            --  Simple conversion
            return Convert (File, Local_Host);
         else
            return Convert (File, Remote, Local);
         end if;
      end;
   end To_Local;

   ------------
   -- To_Arg --
   ------------

   function To_Arg
     (File : Virtual_File;
      Host : String := Local_Host) return GNAT.Strings.String_Access
   is
      Host_File : Virtual_File;
   begin
      if Host /= File.Get_Host then
         if File.Get_Host /= Local_Host then
            Host_File := File.To_Local;
         else
            Host_File := File;
         end if;

         if Host /= Local_Host then
            Host_File := Host_File.To_Remote (Host);
         end if;

         return new String'(String (Host_File.Full_Name.all));
      end if;

      return new String'(String (File.Full_Name.all));
   end To_Arg;

   -------------
   -- Convert --
   -------------

   function Convert
     (File : Virtual_File; To_Host : String) return Virtual_File
   is
   begin
      if File.Value = null then
         return No_File;
      end if;

      --  Create always handles both paths in Unix form, or paths in Host's
      --  form. So we first translate the path to unix (possible whatever the
      --  current path format is), then it's up to Create to correctly format
      --  the path.
      return Create
        (+To_Unix
           (File.Value.Get_FS, FS_String (File.Full_Name.all)), To_Host);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (File     : Virtual_File;
      From_Dir : Virtual_File;
      To_Dir   : Virtual_File) return Virtual_File
   is
   begin
      if File.Value = null
        or else From_Dir.Value = null
        or else To_Dir.Value = null
      then
         return No_File;
      end if;

      if From_Dir.Value.Get_FS /= To_Dir.Value.Get_FS then
         return Create_From_Dir
           (To_Dir,
            +To_Unix (File.Value.Get_FS, +File.Relative_Path (From_Dir)));
      else
         return Create_From_Dir
           (To_Dir,
            File.Relative_Path (From_Dir));
      end if;
   end Convert;

   --------------------
   -- Unchecked_Free --
   --------------------

   procedure Unchecked_Free (Arr : in out File_Array_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (File_Array, File_Array_Access);
   begin
      Internal (Arr);
   end Unchecked_Free;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (File : Virtual_File) return Boolean is
   begin
      return File.Value = null
        or else File.Value.all in GNATCOLL.IO.Native.Native_File_Record'Class;
   end Is_Local;

   ----------
   -- Host --
   ----------

   function Get_Host (File : Virtual_File) return String is
   begin
      if Is_Local (File) then
         return Local_Host;
      else
         return GNATCOLL.IO.Remote.Get_Host
           (Remote_File_Record (File.Value.all)'Access);
      end if;
   end Get_Host;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : Virtual_File) return Boolean is
      Ret : Boolean;
   begin
      if File.Value = null then
         return False;

      elsif File.Value.all not in GNATCOLL.IO.Native.Native_File_Record'Class
        and then File.Value.Kind /= Unknown
      --  Only use cache for remote files
      then
         return File.Value.Kind = GNATCOLL.IO.File;

      else
         Ret := File.Value.Is_Regular_File;

         if Ret then
            File.Value.Kind := GNATCOLL.IO.File;
         end if;

         return Ret;
      end if;
   end Is_Regular_File;

   ----------
   -- Size --
   ----------

   function Size (File : Virtual_File) return Long_Integer is
   begin
      if File.Value = null then
         return 0;
      else
         return File.Value.Size;
      end if;
   end Size;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (File      : Virtual_File;
      Full_Name : Virtual_File;
      Success   : out Boolean) is
   begin
      if File.Value = null
        or else Full_Name.Value = null
        or else File.Value'Tag /= Full_Name.Value'Tag
      then
         Success := False;
      else
         Rename (File.Value, Full_Name.Value, Success);

         if Success then
            Full_Name.Value.Kind := File.Value.Kind;
            File.Value.Kind := Unknown;
         end if;

      end if;
   end Rename;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (File        : Virtual_File;
      Target_Name : Filesystem_String;
      Success     : out Boolean) is
   begin
      if File.Value = null then
         Success := False;
      end if;

      if Is_Directory (File) then
         File.Value.Copy_Dir (+Target_Name, Success);

      else
         File.Value.Copy (+Target_Name, Success);
      end if;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (File    : Virtual_File;
      Success : out Boolean) is
   begin
      if File.Value = null then
         Success := False;
      else
         File.Value.Delete (Success);
      end if;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null and then File.Value.Is_Writable;
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (VF : Virtual_File) return Boolean is
      Ret : Boolean;
   begin
      if VF.Value = null then
         return False;

      elsif VF.Value.all not in GNATCOLL.IO.Native.Native_File_Record'Class
        and then VF.Value.Kind /= Unknown
      --  Only use cache for remote files
      then
         return VF.Value.Kind = Directory;

      else
         Ret := VF.Value.Is_Directory;

         if Ret then
            VF.Ensure_Directory;
            VF.Value.Kind := Directory;
         end if;

         return Ret;
      end if;
   end Is_Directory;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null
        and then File.Value.Is_Symbolic_Link;
   end Is_Symbolic_Link;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (File : Virtual_File) return Boolean is
   begin
      return File.Value /= null
        and then Is_Absolute_Path (File.Value.Get_FS, File.Value.Full.all);
   end Is_Absolute_Path;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension
     (File      : Virtual_File;
      Normalize : Boolean := False) return Filesystem_String is
   begin
      if File.Value = null then
         return "";
      else
         return Filesystem_String
           (File_Extension
              (File.Value.Get_FS, FS_String (File.Full_Name (Normalize).all)));
      end if;
   end File_Extension;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (File : Virtual_File) return GNAT.Strings.String_Access
   is
   begin
      if File.Value = null then
         return null;

      elsif File.Value.Kind = Directory then
         return null;

      else
         return File.Value.Read_Whole_File;
      end if;
   end Read_File;

   ----------------
   -- Write_File --
   ----------------

   function Write_File
     (File   : Virtual_File;
      Append : Boolean := False) return Writable_File
   is
      use type GNAT.OS_Lib.File_Descriptor;
      W  : Writable_File;
   begin
      if File.Value = null then
         return Invalid_File;
      end if;

      W.File    := File;
      W.Append  := Append;
      W.Success := True;

      if not Append or else not File.Is_Regular_File then
         W.Tmp_File := Create
           (File.Full_Name.all & "~",
            Host => File.Get_Host);
         W.Tmp_File.Value.Open_Write (Append => False, FD => W.FD);

      else
         W.Tmp_File := No_File;

         --  append-mode, and the file already exists.
         File.Value.Open_Write (Append => True, FD => W.FD);
      end if;

      if W.FD = GNAT.OS_Lib.Invalid_FD then
         return Invalid_File;
      else
         return W;
      end if;
   end Write_File;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in out Writable_File;
      Str  : String)
   is
      Written : aliased Integer;

   begin
      if File.Success then
         Written := GNAT.OS_Lib.Write (File.FD, Str'Address, Str'Length);
         File.Success := Written = Str'Length;

         if Written > 0 then
            --  File has been overwritten on the disk anyway
            if File.Tmp_File /= No_File then
               File.Tmp_File.Value.Kind := GNATCOLL.IO.File;
            else
               File.File.Value.Kind := GNATCOLL.IO.File;
            end if;
         end if;
      end if;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in out Writable_File;
      Str  : chars_ptr)
   is
      Written : aliased Integer;
      Len     : Integer;

      function To_Address is
        new Ada.Unchecked_Conversion (chars_ptr, System.Address);
   begin
      if File.Success then
         Len := Integer (Strlen (Str));
         Written := GNAT.OS_Lib.Write (File.FD, To_Address (Str), Len);
         File.Success := Written = Len;
         if Written > 0 then
            File.File.Value.Kind := GNATCOLL.IO.File;
         end if;
      end if;
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Writable_File) is
      Norm : Virtual_File;
      Success : Boolean;
      pragma Unreferenced (Success);

      --  We'll need to force the resolution of symbolic links,
      --  since we never want to transform a link into a regular
      --  file (which among other things breaks support for CM Synergy)
      Save_Handle_Symbolic_Links : constant Boolean := Handle_Symbolic_Links;
   begin
      if File.Success then
         if File.Tmp_File /= No_File then
            File.Tmp_File.Value.Close (File.FD, File.Success);
            if File.Success then
               --  Look past symbolic links. We do not want to impact the
               --  normalized name saved in File, so we need to use local
               --  copies.
               Handle_Symbolic_Links := True;
               Norm := Create (File.File.Full_Name.all);
               Norm := Create
                 (Norm.Full_Name
                    (Normalize => True, Resolve_Links => True).all);
               Handle_Symbolic_Links := Save_Handle_Symbolic_Links;

               --  We use to delete explicitly Norm. But in fact this is not a
               --  good idea, since the directory might allow us to delete the
               --  file (or not), but not to recreate it afterwards, as seem to
               --  be the case with CM Synergy.

               File.Tmp_File.Rename (Norm, File.Success);

               if not File.Success then
                  --  Renaming failed. It might be because it could not
                  --  remove the original (read-only directory for instance)
                  --  so let's try with a simple copy instead
                  File.Tmp_File.Copy
                     (Norm.Full_Name (Normalize => True).all, File.Success);
                  if File.Success then
                     File.Tmp_File.Delete (Success);
                     --  ignore Success, that's fine if the temp file is
                     --  still there.
                  end if;
               end if;
            end if;

         else
            File.File.Value.Close (File.FD, File.Success);
         end if;
      end if;

      if not File.Success then
         raise Ada.Text_IO.Use_Error with "Error while writing to the file";
      end if;
   end Close;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (File : VFS.Virtual_File; Writable : Boolean) is
   begin
      if File.Value = null then
         raise VFS_Invalid_File_Error;
      end if;

      File.Value.Set_Writable (Writable);
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable (File : VFS.Virtual_File; Readable : Boolean) is
   begin
      if File.Value = null then
         raise VFS_Invalid_File_Error;
      end if;

      File.Value.Set_Readable (Readable);
   end Set_Readable;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (File : Virtual_File) return Ada.Calendar.Time is
   begin
      if File.Value = null then
         return GNATCOLL.Utils.No_Time;
      end if;

      return File.Value.File_Time_Stamp;
   end File_Time_Stamp;

   ----------------------
   -- Ensure_Directory --
   ----------------------

   procedure Ensure_Directory (Dir : Virtual_File) is
      Full : FS_String_Access;
   begin
      if Dir.Value /= null then
         if not Is_Dir_Name (Dir.Value.Get_FS, Dir.Value.Full.all) then
            Full := new FS_String'
              (Ensure_Directory (Dir.Value.Get_FS, Dir.Value.Full.all));
            Free (Dir.Value.Full);
            Dir.Value.Full := Full;
         end if;

         if Dir.Value.Normalized /= null
           and then not Is_Dir_Name
             (Dir.Value.Get_FS, Dir.Value.Normalized.all)
         then
            Full := new FS_String'
              (Ensure_Directory
                 (Dir.Value.Get_FS, Dir.Value.Normalized.all));

            if Dir.Value.Normalized /= Dir.Value.Normalized_And_Resolved then
               Free (Dir.Value.Normalized_And_Resolved);
            end if;
            Free (Dir.Value.Normalized);
            Dir.Value.Normalized_And_Resolved := null;

            Dir.Value.Normalized := new FS_String'(Full.all);
         end if;
      end if;
   end Ensure_Directory;

   -----------------------
   -- Ensure_Normalized --
   -----------------------

   procedure Ensure_Normalized
     (File             : Virtual_File'Class;
      Resolve_Symlinks : Boolean) is
   begin
      if File.Value = null then
         return;
      end if;

      if File.Value.Normalized = null then
         File.Value.Normalized := new FS_String'
           (Path.Normalize (File.Value.Get_FS, File.Value.Full.all));
      end if;

      if Resolve_Symlinks then
         if Handle_Symbolic_Links then
            GNATCOLL.IO.Resolve_Symlinks (File.Value);
         else
            if File.Value.Normalized_And_Resolved = null then
               File.Value.Normalized_And_Resolved := File.Value.Normalized;
            end if;
         end if;
      end if;
   end Ensure_Normalized;

   --------------------
   -- Normalize_Path --
   --------------------

   procedure Normalize_Path
     (File             : Virtual_File;
      Resolve_Symlinks : Boolean := False)
   is
   begin
      if File.Value = null then
         return;
      end if;

      Ensure_Normalized (File, Resolve_Symlinks);

      Free (File.Value.Full);

      if Resolve_Symlinks then
         File.Value.Full :=
           new FS_String'(File.Value.Normalized_And_Resolved.all);
      else
         File.Value.Full := new FS_String'(File.Value.Normalized.all);
      end if;
   end Normalize_Path;

   --------------
   -- Get_Root --
   --------------

   function Get_Root (File : Virtual_File) return Virtual_File is
   begin
      if File.Value = null then
         return No_File;
      end if;

      return
        (Ada.Finalization.Controlled with
         Dispatching_Create
           (File.Value,
            Get_Root (File.Value.Get_FS, File.Value.Full.all)));
   end Get_Root;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Dir : Virtual_File) return Virtual_File is
   begin
      if Dir.Value = null then
         return No_File;
      end if;

      declare
         Parent : constant FS_String :=
                    Get_Parent (Dir.Value.Get_FS, Dir.Value.Full.all);
      begin
         if Parent = "" then
            return No_File;
         else
            return
              (Ada.Finalization.Controlled with
               Dispatching_Create (Dir.Value, Parent));
         end if;
      end;
   end Get_Parent;

   -------------
   -- Sub_Dir --
   -------------

   function Sub_Dir
     (Dir : Virtual_File; Name : Filesystem_String) return Virtual_File
   is
      New_Dir : Virtual_File;
   begin
      Ensure_Directory (Dir);
      New_Dir :=
        (Ada.Finalization.Controlled with
         Dispatching_Create
           (Dir.Value,
            GNATCOLL.Path.Path
              (Dir.Value.Get_FS, "",
               FS_String (Dir.Full_Name.all), FS_String (Name))));
      Ensure_Directory (New_Dir);

      if Is_Directory (New_Dir) and then True then
         return New_Dir;
      else
         return No_File;
      end if;
   end Sub_Dir;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir : Virtual_File) is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      if Dir.Value = null then
         Raise_Exception (VFS_Directory_Error'Identity, "Dir is No_File");
      end if;

      Success := Dir.Value.Change_Dir;
   end Change_Dir;

   --------------
   -- Make_Dir --
   --------------

   procedure Make_Dir (Dir : Virtual_File; Recursive : Boolean := True) is
      Result : Boolean;
   begin
      if Dir.Value = null then
         Raise_Exception (VFS_Directory_Error'Identity, "Dir is No_File");
      end if;

      --  If Dir already exists and is a directory, then return
      if Is_Directory (Dir) then
         return;
      end if;

      Result := Dir.Value.Make_Dir (Recursive);

      if not Result then
         Dir.Value.Kind := Unknown;
         Raise_Exception
           (VFS_Directory_Error'Identity, "Dir cannot be created");
      else
         Dir.Value.Kind := Directory;
      end if;

   exception
      when E : others =>
         Raise_Exception
           (VFS_Directory_Error'Identity, Exception_Message (E));
   end Make_Dir;

   ----------------
   -- Remove_Dir --
   ----------------

   procedure Remove_Dir
     (Dir       : Virtual_File;
      Recursive : Boolean := False;
      Success   : out Boolean) is
   begin
      if Dir.Value = null then
         raise VFS_Directory_Error;
      end if;

      Dir.Value.Remove_Dir (Recursive, Success);
      if Success then
         Dir.Value.Kind := Unknown;
      end if;
   end Remove_Dir;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (Dir    : Virtual_File;
      Filter : Read_Dir_Filter := All_Files) return File_Array_Access
   is
      F_Array  : File_Array_Access;
      Tmp_File : Virtual_File;

   begin
      if Dir.Value = null then
         Raise_Exception (VFS_Directory_Error'Identity, "Dir is No_File");
      end if;

      Ensure_Directory (Dir);

      if not Is_Directory (Dir) then
         Raise_Exception
           (VFS_Directory_Error'Identity, "Dir is not a directory");
      end if;

      declare
         List : GNAT.Strings.String_List :=
                  Dir.Value.Read_Dir
                    (Dirs_Only  => Filter = Dirs_Only,
                     Files_Only => Filter = Files_Only);
      begin
         F_Array := new File_Array (1 .. List'Length);

         for J in List'Range loop
            Tmp_File := Dir.Create_From_Dir (+List (J).all);

            case Filter is
               when Dirs_Only =>
                  Tmp_File.Value.Kind := Directory;

               when Files_Only =>
                  Tmp_File.Value.Kind := File;

               when others =>
                  null;

            end case;

            F_Array (F_Array'First + J - List'First) := Tmp_File;
            GNAT.Strings.Free (List (J));
         end loop;
      end;

      return F_Array;

   exception
      when E : others =>
         Unchecked_Free (F_Array);
         Raise_Exception
           (VFS_Directory_Error'Identity,
            Exception_Message (E));
   end Read_Dir;

   ------------------------
   -- Read_Dir_Recursive --
   ------------------------

   function Read_Dir_Recursive
      (Dir       : Virtual_File;
       Extension : Filesystem_String := "";
       Filter    : Read_Dir_Filter := All_Files) return File_Array_Access
   is
      Result : File_Array_Access;

      procedure Internal (Directory : Virtual_File);
      --  process a directory recursively

      procedure Internal (Directory : Virtual_File) is
         Files : File_Array_Access := Directory.Read_Dir (Filter => All_Files);
      begin
         if Files = null then
            return;
         end if;

         for F in Files'Range loop
            declare
               B : constant Filesystem_String := Files (F).Base_Name;
            begin
               if B /= "." and then B /= ".." then
                  if Extension = ""
                     or else Files (F).File_Extension = Extension
                  then
                     case Filter is
                     when Dirs_Only =>
                        if Files (F).Is_Directory then
                           Append (Result, Files (F));
                        end if;
                     when Files_Only =>
                        if Files (F).Is_Regular_File then
                           Append (Result, Files (F));
                        end if;
                     when All_Files =>
                        Append (Result, Files (F));
                     end case;
                  end if;

                  if Files (F).Is_Directory then
                     Internal (Files (F));
                  end if;
               end if;
            end;
         end loop;

         Unchecked_Free (Files);
      end Internal;

   begin
      if Dir.Is_Directory then
         Internal (Dir);
      end if;
      return Result;
   end Read_Dir_Recursive;

   --------------------------
   -- Read_Files_From_Dirs --
   --------------------------

   function Read_Files_From_Dirs
      (Dirs : File_Array) return File_Array_Access
   is
      Ret    : File_Array_Access := null;
      Files  : array (Dirs'Range) of File_Array_Access;
      Length : Natural := 0;
      Idx    : Natural;
   begin
      for J in Dirs'Range loop
         begin
            Files (J) := Read_Dir (Dirs (J), Files_Only);

            if Files (J) /= null then
               Length := Length + Files (J)'Length;
            end if;

         exception
            when VFS_Directory_Error =>
               Files (J) := null;
         end;
      end loop;

      if Length = 0 then
         return null;
      else
         Ret := new File_Array (1 .. Length);
         Idx := Ret'First;

         for J in Files'Range loop
            if Files (J) /= null then
               Ret (Idx .. Idx + Files (J)'Length - 1) := Files (J).all;
               Idx := Idx + Files (J)'Length;
               Unchecked_Free (Files (J));
            end if;
         end loop;

         return Ret;
      end if;
   end Read_Files_From_Dirs;

   --------------
   -- Open_Dir --
   --------------

   function Open_Dir (Dir : Virtual_File) return Virtual_Dir is
      VDir : Virtual_Dir;
   begin
      if Dir.Value = null then
         return Invalid_Dir;
      end if;

      VDir.File := Dir;
      VDir.Files_List := Read_Dir (Dir);

      if VDir.Files_List /= null then
         VDir.Current := VDir.Files_List'First - 1;
      end if;

      Dir.Value.Kind := Directory;
      return VDir;

   exception
      when VFS_Directory_Error =>
         return Invalid_Dir;
   end Open_Dir;

   ----------
   -- Read --
   ----------

   procedure Read
     (VDir : in out Virtual_Dir;
      File :    out Virtual_File) is
   begin
      if VDir.Files_List /= null
        and then VDir.Current < VDir.Files_List'Last
      then
         VDir.Current := VDir.Current + 1;
         File := VDir.Files_List (VDir.Current);
      else
         File := No_File;
      end if;
   end Read;

   -----------
   -- Close --
   -----------

   procedure Close (VDir : in out Virtual_Dir) is
   begin
      if VDir.Files_List /= null then
         Unchecked_Free (VDir.Files_List);
      end if;

      VDir := Invalid_Dir;
   end Close;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (File : in out Virtual_File) is
      Value : GNATCOLL.IO.File_Access := File.Value;
   begin
      File.Value := null;  --  Make Finalize idempotent
      if Value /= null then
         Unref (Value);
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (File : in out Virtual_File) is
   begin
      if File.Value /= null then
         Ref (File.Value);
      end if;
   end Adjust;

   ---------------
   -- Is_Parent --
   ---------------

   function Is_Parent (Parent, Child : Virtual_File) return Boolean is
   begin
      if Parent.Value = null
        or else Child.Value = null
        or else Parent.Value'Tag /= Child.Value'Tag
      then
         return False;
      end if;

      Ensure_Normalized (Parent, Resolve_Symlinks => True);
      Ensure_Normalized (Child, Resolve_Symlinks => True);

      if Parent.Value.Normalized_And_Resolved'Length >
        Child.Value.Normalized_And_Resolved'Length
      then
         return False;
      end if;

      return Equal
        (Parent.Value.Get_FS,
         Parent.Value.Normalized_And_Resolved.all,
         Child.Value.Normalized_And_Resolved
           (Child.Value.Normalized_And_Resolved'First ..
              Child.Value.Normalized_And_Resolved'First +
                Parent.Value.Normalized_And_Resolved'Length - 1));
   end Is_Parent;

   ----------
   -- Sort --
   ----------

   procedure Sort (Files : in out File_Array) is
      --  ??? Right now, this sorts only on the full name. Do we want to
      --  provide other choices for sorting ?

      procedure Xchg (Op1, Op2 : Natural);
      --  Exchanges two items in the array

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Return True if the first item is to be sorted before the second

      ----------
      -- Xchg --
      ----------

      procedure Xchg (Op1, Op2 : Natural) is
         Buffer : Virtual_File;
      begin
         Buffer := Files (Files'First - 1 + Op1);
         Files (Files'First - 1 + Op1) := Files (Files'First - 1 + Op2);
         Files (Files'First - 1 + Op2) := Buffer;
      end Xchg;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Files (Files'First - 1 + Op1) <
           Files (Files'First - 1 + Op2);
      end Lt;

   begin
      Sort (Files'Length, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);
   end Sort;

   ------------
   -- Append --
   ------------

   procedure Append (Files : in out File_Array_Access; F : Virtual_File) is
   begin
      Append (Files, File_Array'(1 => F));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (Files : in out File_Array_Access; F : File_Array) is
      Tmp : File_Array_Access;
   begin
      if Files = null then
         Files := new File_Array'(F);
      else
         Tmp := new File_Array (1 .. Files'Length + F'Length);
         Tmp (1 .. Files'Length) := Files.all;
         Tmp (Files'Length + 1 .. Tmp'Last) := F;
         Unchecked_Free (Files);
         Files := Tmp;
      end if;
   end Append;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Files : in out File_Array_Access; F : File_Array) is
      Tmp : File_Array_Access;
   begin
      if Files = null then
         Files := new File_Array'(F);
      else
         Tmp := new File_Array (1 .. Files'Length + F'Length);
         Tmp (1 + F'Length .. Tmp'Length) := Files.all;
         Tmp (1 .. F'Length) := F;
         Unchecked_Free (Files);
         Files := Tmp;
      end if;
   end Prepend;

   ------------
   -- Remove --
   ------------

   procedure Remove (Files : in out File_Array_Access; F : Virtual_File) is
      Tmp : File_Array_Access;
   begin
      for J in Files'Range loop
         if Files (J) = F then
            for K in J + 1 .. Files'Last loop
               Files (K - 1) := Files (K);
            end loop;

            Tmp := new File_Array'(Files (Files'First .. Files'Last - 1));
            Unchecked_Free (Files);
            Files := Tmp;

            return;
         end if;
      end loop;
   end Remove;

   -------------
   -- To_Path --
   -------------

   function To_Path (Paths : File_Array) return Filesystem_String is
      Length : Natural := 0;
   begin
      if Paths'Length = 0 then
         return "";
      end if;

      for J in Paths'Range loop
         Length := Length + Paths (J).Full_Name.all'Length;
      end loop;

      Length := Length + Paths'Length - 1;

      declare
         Ret : Filesystem_String (1 .. Length);
         Idx : Natural := Ret'First;
      begin
         for J in Paths'Range loop
            Ret (Idx .. Idx + Paths (J).Full_Name.all'Length - 1) :=
              Paths (J).Full_Name.all;
            Idx := Idx + Paths (J).Full_Name.all'Length;

            if J /= Paths'Last then
               Ret (Idx) := GNAT.OS_Lib.Path_Separator;
               Idx := Idx + 1;
            end if;
         end loop;

         return Ret;
      end;
   end To_Path;

   ---------------
   -- From_Path --
   ---------------

   function From_Path (Path : Filesystem_String) return File_Array is
      Ret  : File_Array_Access;
      Last : Natural := Path'First;
   begin
      for J in Path'First .. Path'Last loop
         --  ??? Should define Path_Separator in FS (system-dependent)
         if Path (J) = GNAT.OS_Lib.Path_Separator then
            if Last < J then
               Append (Ret, Create (Path (Last .. J - 1)));
            end if;

            Last := J + 1;
         end if;
      end loop;

      if Last <= Path'Last then
         Append (Ret, Create (Path (Last .. Path'Last)));
      end if;

      if Ret = null then
         return (1 .. 0 => <>);
      end if;

      declare
         Final : constant File_Array := Ret.all;
      begin
         Unchecked_Free (Ret);

         return Final;
      end;
   end From_Path;

   --------------------
   -- Locate_On_Path --
   --------------------

   function Locate_On_Path
     (Base_Name : Filesystem_String;
      Path      : File_Array) return Virtual_File
   is
      File : Virtual_File;
   begin
      for J in Path'Range loop
         if Path (J) /= No_File then
            File := Create_From_Dir (Path (J), Base_Name);

            if Is_Regular_File (File) then
               return File;
            end if;

            File := Create_From_Dir
              (Path (J), Base_Name & (+Exe_Extension (Path (J).Value.Get_FS)));

            if Is_Regular_File (File) then
               return File;
            end if;
         end if;
      end loop;

      return No_File;
   end Locate_On_Path;

   --------------------------
   -- Greatest_Common_Path --
   --------------------------

   function Greatest_Common_Path
     (L : GNATCOLL.VFS.File_Array) return Virtual_File
   is
   begin
      if L'Length = 0 then
         return GNATCOLL.VFS.No_File;
      end if;

      declare
         Greatest_Prefix : Virtual_File := L (L'First);
         Root            : constant Virtual_File := Get_Root (Greatest_Prefix);
      begin
         for J in L'First + 1 .. L'Last loop
            --  Loop until GP is a parent of the current File
            while not Greatest_Prefix.Is_Parent (L (J)) loop
               --  If not a parent, and already at root, then there is no
               --  greatest prefix.
               if Greatest_Prefix = Root then
                  return No_File;
               end if;

               Greatest_Prefix := Get_Parent (Greatest_Prefix);
            end loop;
         end loop;

         return Greatest_Prefix;
      end;
   end Greatest_Common_Path;

   -------------------------
   -- Locate_Regular_File --
   -------------------------

   function Locate_Regular_File
     (File_Name : Filesystem_String;
      Path      : File_Array) return Virtual_File
   is
      F : Virtual_File;
   begin
      for J in Path'Range loop
         F := Create_From_Dir (Path (J), File_Name);

         if F.Is_Regular_File then
            return F;
         end if;
      end loop;

      return No_File;
   end Locate_Regular_File;

   ----------------------------
   -- Symbolic_Links_Support --
   ----------------------------

   procedure Symbolic_Links_Support (Active : Boolean) is
   begin
      Handle_Symbolic_Links := Active;
   end Symbolic_Links_Support;

   ----------
   -- Join --
   ----------

   function Join
      (Self : Virtual_File; File : Virtual_File) return Virtual_File is
   begin
      return Create_From_Dir (Self, File.Full_Name.all);
   end Join;

   ----------
   -- Join --
   ----------

   function Join
      (Self : Virtual_File; Path : Filesystem_String) return Virtual_File is
   begin
      return Create_From_Dir (Self, Path);
   end Join;

   ---------
   -- "/" --
   ---------

   function "/"
      (Self : Virtual_File; File : Virtual_File) return Virtual_File is
   begin
      return Create_From_Dir (Self, File.Full_Name.all);
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
      (Self : Virtual_File; Path : Filesystem_String) return Virtual_File is
   begin
      return Create_From_Dir (Self, Path);
   end "/";

   ---------
   -- "/" --
   ---------

   function "/"
      (Dir : Filesystem_String; File : Virtual_File) return Virtual_File is
   begin
      return Create_From_Dir (Create (Dir), File.Full_Name.all);
   end "/";

end GNATCOLL.VFS;
