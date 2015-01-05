------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;

package body GNATCOLL.Path is

   function Dir_Separator (FS : FS_Type) return Character;
   --  return '/' or '\' depending of FS

   -------------------
   -- Dir_Separator --
   -------------------

   function Dir_Separator (FS : FS_Type) return Character is
   begin
      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return '/';
         when FS_Windows =>
            return '\';
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end Dir_Separator;

   function Internal_Local_FS return FS_Type;
   --  We cache this value as it is very often referenced... and is
   --  a constant !

   -----------------------
   -- Internal_Local_FS --
   -----------------------

   function Internal_Local_FS return FS_Type is
      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");
   begin
      if GNAT.OS_Lib.Directory_Separator = '\' then
         return FS_Windows;
      else
         if Get_File_Names_Case_Sensitive = 0 then
            return FS_Unix_Case_Insensitive;
         else
            return FS_Unix;
         end if;
      end if;
   end Internal_Local_FS;

   Loc_FS : constant FS_Type := Internal_Local_FS;

   --------------
   -- Local_FS --
   --------------

   function Local_FS return FS_Type is
   begin
      return Loc_FS;
   end Local_FS;

   -----------------------
   -- Is_Case_Sensitive --
   -----------------------

   function Is_Case_Sensitive (FS : FS_Type) return Boolean is
   begin
      case FS is
         when FS_Unix =>
            return True;
         when FS_Windows | FS_Unix_Case_Insensitive =>
            return False;
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end Is_Case_Sensitive;

   -----------------
   -- Has_Devices --
   -----------------

   function Has_Devices (FS : FS_Type) return Boolean is
   begin
      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return False;
         when FS_Windows =>
            return True;
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end Has_Devices;

   ---------------------------
   -- Multi_Unit_Index_Char --
   ---------------------------

   function Multi_Unit_Index_Char (FS : FS_Type) return Character is
      pragma Unreferenced (FS);
   begin
      return '~';
   end Multi_Unit_Index_Char;

   -------------------
   -- Exe_Extension --
   -------------------

   function Exe_Extension (FS : FS_Type) return FS_String is
   begin
      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return "";
         when FS_Windows =>
            return ".exe";
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end Exe_Extension;

   --------------
   -- Get_Root --
   --------------

   function Get_Root
     (FS   : FS_Type;
      Path : FS_String) return FS_String
   is
      Found_One : Boolean;
   begin
      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return "/";

         when FS_Windows =>
            if Path'Length >= 2
              and then Path (Path'First + 1) = ':'
            then
               return Path (Path'First) & ":\";

            elsif Path'Length > 3
              and then Path (Path'First .. Path'First + 1) = "\\"
            then
               --  We need to return "\\machine\svc\" in this case

               Found_One := False;
               --  Used to determine that we found at least one
               --  '\' after the initial "\\"

               for J in Path'First + 2 .. Path'Last loop
                  if Path (J) = '\' then

                     if not Found_One then
                        Found_One := True;
                     else
                        return Path (Path'First .. J);
                     end if;
                  end if;
               end loop;

               if Found_One then
                  --  Case where we had "\\machine\svc" to analyse. The root
                  --  is then "\\machine\src\"
                  return Path & '\';
               else
                  --  Incomplete ... return the default
                  return "\";
               end if;

            else
               return "\";
            end if;
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end Get_Root;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path
     (FS   : FS_Type;
      Path : FS_String) return Boolean is
   begin
      if Path'Length = 0 then
         return False;
      end if;

      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return Path (Path'First) = '/';
         when FS_Windows =>
            if Path'Length >= 3
              and then Path (Path'First + 1 .. Path'First + 2) = ":\"
            then
               return True;

            elsif Path'Length > 1
              and then (Path (Path'First) = '\'
                         or else Path (Path'First) = '/')
            then
               return True;

            end if;
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;

      return False;
   end Is_Absolute_Path;

   ----------
   -- Path --
   ----------

   function Path
     (FS     : FS_Type;
      Device : FS_String;
      Dir    : FS_String;
      File   : FS_String) return FS_String
   is
      Has_Dirsep : constant Boolean :=
                     Dir'Length >= 1
                      and then Dir (Dir'Last) = Dir_Separator (FS);
   begin
      if FS = FS_Unknown then
         raise Invalid_Filesystem;
      end if;

      if FS in FS_Unix .. FS_Unix_Case_Insensitive or else Device = "" then
         if Has_Dirsep then
            return Dir & From_Unix (FS, File);
         else
            return Dir & Dir_Separator (FS) & From_Unix (FS, File);
         end if;

      else
         if Has_Dirsep then
            return Device & ":" & Dir & From_Unix (FS, File);
         else
            return Device & ":" & Dir & Dir_Separator (FS) &
              From_Unix (FS, File);
         end if;

      end if;
   end Path;

   -----------
   -- Equal --
   -----------

   function Equal
     (FS           : FS_Type;
      Path1, Path2 : FS_String) return Boolean
   is
      function Internal (S1, S2 : FS_String) return Boolean;
      --  Compare, taking care of trailing dir separators

      --------------
      -- Internal --
      --------------

      function Internal (S1, S2 : FS_String) return Boolean is
      begin
         return S1 = S2
           or else (S2'Length > 0
                     and then S2 (S2'Last) = Dir_Separator (FS)
                     and then S1 = S2 (S2'First .. S2'Last - 1))
           or else (S1'Length > 0
                     and then S1 (S1'Last) = Dir_Separator (FS)
                     and then S2 = S1 (S1'First .. S1'Last - 1));
      end Internal;

   begin
      if Is_Case_Sensitive (FS) then
         return Internal (Path1, Path2);
      else
         return Internal (FS_String (To_Lower (String (Path1))),
                          FS_String (To_Lower (String (Path2))));
      end if;
   end Equal;

   -------------
   -- To_Unix --
   -------------

   function To_Unix
     (FS          : FS_Type;
      Path        : FS_String;
      Cygwin_Path : Boolean := False) return FS_String
   is
   begin
      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return Path;

         when FS_Windows =>
            declare
               Ret : FS_String := Path;
            begin
               for J in Ret'Range loop
                  if Ret (J) = '\' then
                     Ret (J) := '/';
                  end if;
               end loop;

               if Cygwin_Path
                 and then Ret'Length > 2
                 and then Ret (Ret'First + 1) = ':'
               then
                  return "/cygdrive/" &
                    Ret (Ret'First) & Ret (Ret'First + 2 .. Ret'Last);
               else
                  return Ret;
               end if;
            end;
         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end To_Unix;

   ---------------
   -- From_Unix --
   ---------------

   function From_Unix
     (FS   : FS_Type;
      Path : FS_String) return FS_String is
   begin
      case FS is
         when FS_Unix | FS_Unix_Case_Insensitive =>
            return Path;

         when FS_Windows =>
            declare
               Ret : FS_String := Path;
            begin
               for J in Ret'Range loop
                  if Ret (J) = '/' then
                     Ret (J) := '\';
                  end if;
               end loop;

               if Ret'Length >= 11 --  "/cygdrive/X"'Length
                 and then Ret (Ret'First .. Ret'First + 9) = "\cygdrive\"
               then
                  return Ret (Ret'First + 10) & ":" &
                    Ret (Ret'First + 11 .. Ret'Last);
               else
                  return Ret;
               end if;
            end;

         when FS_Unknown =>
            raise Invalid_Filesystem;
      end case;
   end From_Unix;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension
     (FS   : FS_Type;
      Path : FS_String) return FS_String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = '.' then
            case FS is
               when FS_Unix | FS_Unix_Case_Insensitive =>
                  return Path (J .. Path'Last);
               when FS_Windows =>
                  return FS_String
                    (To_Lower (String (Path (J .. Path'Last))));
               when FS_Unknown =>
                  raise Invalid_Filesystem;
            end case;
         end if;
      end loop;

      return "";
   end File_Extension;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (FS     : FS_Type;
      Path   : FS_String;
      Suffix : FS_String := "") return FS_String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = Dir_Separator (FS) then
            if Path'Last - J < Suffix'Length
              or else Path (Path'Last - Suffix'Length + 1 .. Path'Last)
                /= Suffix
            then
               return Path (J + 1 .. Path'Last);
            else
               return Path (J + 1 .. Path'Last - Suffix'Length);
            end if;
         end if;
      end loop;

      return Path;
   end Base_Name;

   -------------------
   -- Base_Dir_Name --
   -------------------

   function Base_Dir_Name
     (FS   : FS_Type;
      Path : FS_String) return FS_String
   is
      Root : constant FS_String := Get_Root (FS, Path);
   begin
      if Path = Root then
         return Path;

      elsif Path (Path'Last) = Dir_Separator (FS) then
         return Base_Name (FS, Path (Path'First .. Path'Last - 1));

      else
         return Base_Name (FS, Path);
      end if;
   end Base_Dir_Name;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name
     (FS   : FS_Type;
      Path : FS_String) return FS_String
   is
      Root : constant FS_String := Get_Root (FS, Path);
   begin
      if Root'Length > Path'Length then
         return "";
      end if;

      for J in reverse Path'Range loop
         if Path (J) = Dir_Separator (FS) then
            return Path (Path'First .. J);
         end if;
      end loop;

      return Path;
   end Dir_Name;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (FS : FS_Type;
      Path : FS_String) return FS_String
   is
   begin
      if not Is_Dir_Name (FS, Path) then
         return Dir_Name (FS, Path);
      else
         return Dir_Name (FS, Path (Path'First .. Path'Last - 1));
      end if;
   end Get_Parent;

   -----------------
   -- Is_Dir_Name --
   -----------------

   function Is_Dir_Name
     (FS   : FS_Type;
      Path : FS_String) return Boolean is
   begin
      return Path'Length > 0
        and then Path (Path'Last) = Dir_Separator (FS);
   end Is_Dir_Name;

   ----------------------
   -- Ensure_Directory --
   ----------------------

   function Ensure_Directory
     (FS   : FS_Type;
      Path : FS_String) return FS_String is
   begin
      if not Is_Dir_Name (FS, Path) then
         return Path & Dir_Separator (FS);
      else
         return Path;
      end if;
   end Ensure_Directory;

   -----------------
   -- Device_Name --
   -----------------

   function Device_Name
     (FS   : FS_Type;
      Path : FS_String) return FS_String is
   begin
      if FS = FS_Windows
        and then Path'Length > 2
        and then Path (Path'First + 1) = ':'
      then
         return "" & Path (Path'First);
      else
         return "";
      end if;
   end Device_Name;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (FS   : FS_Type;
      Path : FS_String) return FS_String
   is
      Dest : FS_String := Path;
      Src  : Natural := Path'First;
      Idx  : Natural := Dest'First;
      DS   : Character renames Dir_Separator (FS);

   begin
      while Src <= Path'Last loop
         if Idx > Dest'First and then Dest (Idx - 1) = DS then
            if Path (Src) = '.' then
               if Src >= Path'Last or else Path (Src + 1) = DS then
                  Src := Src + 2;

               elsif Path (Src + 1) = '.' then
                  if Src + 1 >= Path'Last or else Path (Src + 2) = DS then
                     for K in reverse Dest'First .. Idx - 2 loop
                        if Dest (K) = DS then
                           Idx := K;
                           exit;
                        end if;
                     end loop;
                     Src := Src + 2;
                  else
                     Dest (Idx) := '.';
                     Dest (Idx + 1) := '.';
                     Idx := Idx + 2;
                     Src := Src + 2;
                  end if;

               else
                  Dest (Idx) := Path (Src);
                  Idx := Idx + 1;
                  Src := Src + 1;
               end if;
            else
               Dest (Idx) := Path (Src);
               Idx := Idx + 1;
               Src := Src + 1;
            end if;
         else
            Dest (Idx) := Path (Src);
            Idx := Idx + 1;
            Src := Src + 1;
         end if;
      end loop;
      return Dest (Dest'First .. Idx - 1);
   end Normalize;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path
     (FS   : FS_Type;
      Ref  : FS_String;
      Path : FS_String) return FS_String
   is
      Depth : Natural := 0;
      Last  : Natural := Ref'Last;
      Old   : Natural;

      function Depth_Image return FS_String;
      --  Return "../" * Depth

      -----------------
      -- Depth_Image --
      -----------------

      function Depth_Image return FS_String is
         Ret : FS_String (1 .. 3 * Depth);
         Sep : constant FS_String := ".." & Dir_Separator (FS);
      begin
         for J in 1 .. Depth loop
            Ret (J * 3 - 2 .. J * 3) := Sep;
         end loop;

         return Ret;
      end Depth_Image;

   begin
      --  If roots are not identical, then this means that we need to return
      --  the file's full name.
      if not Equal (FS, Get_Root (FS, Ref), Get_Root (FS, Path)) then
         return Path;
      end if;

      if Equal (FS, Ref, Path) then
         return ".";
      end if;

      loop
         exit when Last - Ref'First + 1 <= Path'Length
           and then Equal
             (FS,
              Path (Path'First .. Path'First + Last - Ref'First),
              Ref (Ref'First .. Last));

         Old := Last;
         for J in reverse Ref'First .. Last - 1 loop
            if Ref (J) = Dir_Separator (FS) then
               Depth := Depth + 1;
               Last := J;
               exit;
            end if;
         end loop;

         if Old = Last then
            --  No Dir_Separator in Ref... Let's return Path
            return Path;
         end if;
      end loop;

      return Depth_Image &
        Path (Path'First + Last - Ref'First + 1 .. Path'Last);
   end Relative_Path;

end GNATCOLL.Path;
