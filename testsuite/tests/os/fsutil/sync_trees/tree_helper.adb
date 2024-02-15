with GNAT.IO;
with GNAT.OS_Lib;
with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Dir; use GNATCOLL.OS.Dir;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

package body Tree_Helper is

   package Unbounded_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Unbounded_String);

   use Unbounded_String_Vectors;

   package Unbounded_Sorting is
      new Unbounded_String_Vectors.Generic_Sorting;

   procedure Format_Path_Separators (S : in out String);
   --  Replace directory separators by '/'

   ----------------------------
   -- Format_Path_Separators --
   ----------------------------

   procedure Format_Path_Separators (S : in out String)
   is
      Directory_Separator : constant String (1 .. 1) :=
         (1 => GNAT.OS_Lib.Directory_Separator);

   begin

      if Directory_Separator = "/" then
         return;
      end if;

      declare
         Cnt : constant Natural :=
            Count (Source  => S,
                  Pattern => Directory_Separator);
         Idx : Natural := S'First;
      begin
         for I in 1 .. Cnt loop
            Idx := Index
            (Source  => S,
            Pattern => Directory_Separator,
            From    => Idx);
            S (Idx) := '/';
         end loop;
      end;
   end Format_Path_Separators;

   procedure Display_Trees
   (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) is
      package IO renames GNAT.IO;
      package Dir renames GNATCOLL.OS.Dir;

      Base_Path : Unbounded_String;

      Outputs : Vector;

      function Format_Path (Path_To_Format : String) return String with
      Pre => Path_To_Format'Length > 0;
      --  If the path to format is included in the base path, return only
      --  the exclusive subpart of the path to format.

      function Format_Path (Path_To_Format : String) return String is
         Base_Path_Str : constant String := To_String (Base_Path);

      begin
         --  Absolute path can target base_path, or external path

         if Path_To_Format'Length >= Base_Path_Str'Length
         and then Path_To_Format (Path_To_Format'First .. Base_Path_Str'Last) =
            Base_Path
         then
            declare
               --  + 2 to include separator
               Result : String := Path_To_Format
                    (Base_Path_Str'Last + 2 .. Path_To_Format'Last);
            begin
               Format_Path_Separators (Result);
               return Result;
            end;
         else
            declare
               Result : String := Path_To_Format;
            begin
               Format_Path_Separators (Result);
               return Result;
            end;
         end if;
      end Format_Path;

      function Handle_Dir (H : Dir_Handle; Element : Dir_Entry) return Boolean;

      function Handle_Dir (H : Dir_Handle; Element : Dir_Entry)
      return Boolean is
         Path        : constant UTF8.UTF_8_String    := Dir.Path (H, Element);
      begin
         Outputs.Append (To_Unbounded_String ("DIR " & Format_Path (Path)));

         return True;
      end Handle_Dir;

      procedure Handle_File (H : Dir_Handle; Element : Dir_Entry);

      procedure Handle_File (H : Dir_Handle; Element : Dir_Entry) is
         Path        : constant UTF8.UTF_8_String    := Dir.Path (H, Element);
         FA          : constant Stat.File_Attributes :=
                         Dir.Attributes (Element);
         Target_Path : Unbounded_String;
         Message     : Unbounded_String;
      begin
         if Stat.Is_Symbolic_Link (FA) then
            if not Read_Symbolic_Link (Path, Target_Path) then
               IO.Put_Line ("Failed to read link for " & Path);
               return;
            end if;

            Message :=
              To_Unbounded_String ("SYMLINK " & Format_Path (Path)
               & " -> " & Format_Path (To_String (Target_Path)));

         else
            Message := To_Unbounded_String ("FILE " & Format_Path (Path));
         end if;

         Outputs.Append (Message);
      end Handle_File;

      S : constant String := Path (Open (Src));

   begin

      --  Remove the src directory name from absolute source path, to obtain
      --  the absolute path common to source and destination directories.
      --  +1 is for the missing / present in the absolute source path but not
      --  in the src directory name.

      Base_Path :=
        To_Unbounded_String (S (S'First .. S'Last - (Src'Length + 1)));

      IO.Put_Line ("Source tree");
      IO.Put_Line ("===========");
      Walk
      (Path                 => Src,
       File_Handler         => Handle_File'Unrestricted_Access,
       Dir_Handler          => Handle_Dir'Unrestricted_Access,
       Follow_Symlinks      => False,
       Propagate_Exceptions => True);

      Unbounded_Sorting.Sort (Outputs);
      for Output of Outputs loop
         IO.Put_Line (To_String (Output));
      end loop;

      IO.Put_Line ("");
      Outputs.Clear;

      IO.Put_Line ("Destination tree");
      IO.Put_Line ("================");
      Walk
      (Path                 => Dst,
       File_Handler         => Handle_File'Unrestricted_Access,
       Dir_Handler          => Handle_Dir'Unrestricted_Access,
       Follow_Symlinks      => False,
       Propagate_Exceptions => True);

      Unbounded_Sorting.Sort (Outputs);
      for Output of Outputs loop
         IO.Put_Line (To_String (Output));
      end loop;

      IO.Put_Line ("");
   end Display_Trees;

   procedure Compare_Tree_Files_Content
   (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) is
      package IO renames GNAT.IO;
      package Dir renames GNATCOLL.OS.Dir;

      Base_Path : Unbounded_String;

      Outputs : Vector;

      function Basename (Path : String) return String;
      --  Return the path relative to the dst directory

      function Basename (Path : String) return String is
         Base_Path_Str : constant String := To_String (Base_Path);
      begin
            --  +2 for the separator between base path and dst, and
            --  +1 for the separator between dst and the file relative path
            return
              Path (Base_Path_Str'Last + 2 + Dst'Length + 1 .. Path'Last);
      end Basename;

      function Src_Path (Dst_Path : String) return String;
      --  If the path to format is included in the base path, return only
      --  the exclusive subpart of the path to format.

      function Src_Path (Dst_Path : String) return String is
         Base_Path_Str : constant String := To_String (Base_Path);
         Directory_Separator : constant String (1 .. 1) :=
            (1 => GNAT.OS_Lib.Directory_Separator);
      begin
            return
              Base_Path_Str & Directory_Separator & Src & Directory_Separator
              & Basename (Dst_Path);
      end Src_Path;

      function Handle_Dir (H : Dir_Handle; Element : Dir_Entry) return Boolean;

      function Handle_Dir (H : Dir_Handle; Element : Dir_Entry)
      return Boolean is
         pragma Unreferenced (H);
         pragma Unreferenced (Element);
      begin
         return True;
      end Handle_Dir;

      procedure Handle_File (H : Dir_Handle; Element : Dir_Entry);

      procedure Handle_File (H : Dir_Handle; Element : Dir_Entry) is
         D_Path : constant UTF8.UTF_8_String    := Dir.Path (H, Element);
         S_Path : constant UTF8.UTF_8_String    := Src_Path (D_Path);
         FA     : constant Stat.File_Attributes := Dir.Attributes (Element);
      begin
         if Stat.Is_Symbolic_Link (FA) then
            return;
         end if;

         if SHA1 (D_Path) /= SHA1 (S_Path) then
            declare
               Base : String := Basename (D_Path);
            begin
               Format_Path_Separators (Base);
               Outputs.Append (
                  To_Unbounded_String
                    ("Content differs for the file " & Base));
            end;
         end if;
      end Handle_File;

      S : constant String := Path (Open (Src));
   begin

      IO.Put_Line ("Content checking");
      IO.Put_Line ("================");

      --  Remove the src directory name from absolute source path, to obtain
      --  the absolute path common to source and destination directories.
      --  +1 is for the missing / present in the absolute source path but not
      --  in the src directory name.

      Base_Path :=
        To_Unbounded_String (S (S'First .. S'Last - (Src'Length + 1)));

      Walk
      (Path                 => Dst,
       File_Handler         => Handle_File'Unrestricted_Access,
       Dir_Handler          => Handle_Dir'Unrestricted_Access,
       Follow_Symlinks      => False,
       Propagate_Exceptions => True);

      Unbounded_Sorting.Sort (Outputs);
      for Output of Outputs loop
         IO.Put_Line (To_String (Output));
      end loop;
   end Compare_Tree_Files_Content;

end Tree_Helper;
