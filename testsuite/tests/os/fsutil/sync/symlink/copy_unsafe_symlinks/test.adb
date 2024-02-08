with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.FSUtil;    use GNATCOLL.OS.FSUtil;
with Ada.Strings.UTF_Encoding;
with GNATCOLL.OS.Dir;       use GNATCOLL.OS.Dir;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package UTF8 renames Ada.Strings.UTF_Encoding;
   package Dir renames GNATCOLL.OS.Dir;

   Src : constant UTF8.UTF_8_String := "src_top_dir";
   Dst : constant UTF8.UTF_8_String := "dst_top_dir";

   Base_Path : Unbounded_String;

   function Format_Path (Path_To_Format : String) return String with
     Pre => Path_To_Format'Length > 0;
   --  If the path to format is included in the base path, return only
   --  the exclusive subpart of the path to format.

   function Format_Path (Path_To_Format : String) return String is
      Base_Path_Str : constant String := To_String (Base_Path);
   begin

      --  Absolute path can target base_path, or external path.
      if Path_To_Format'Length >= Base_Path_Str'Length
        and then Path_To_Format (Path_To_Format'First .. Base_Path_Str'Last) =
          Base_Path
      then
         --  + 2 to include separator
         return Path_To_Format (Base_Path_Str'Last + 2 .. Path_To_Format'Last);
      else
         return Path_To_Format;
      end if;
   end Format_Path;

   function Handle_Dir (H : Dir_Handle; Element : Dir_Entry) return Boolean;

   function Handle_Dir (H : Dir_Handle; Element : Dir_Entry) return Boolean is
      Path        : constant UTF8.UTF_8_String    := Dir.Path (H, Element);
   begin
      IO.Put_Line ("DIR " & Format_Path (Path));

      return True;
   end Handle_Dir;

   procedure Handle_File (H : Dir_Handle; Element : Dir_Entry);

   procedure Handle_File (H : Dir_Handle; Element : Dir_Entry) is
      Path        : constant UTF8.UTF_8_String    := Dir.Path (H, Element);
      FA          : constant Stat.File_Attributes := Dir.Attributes (Element);
      Target_Path : Unbounded_String;
   begin
      if Stat.Is_Symbolic_Link (FA) then
         if not Read_Symbolic_Link (Path, Target_Path) then
            IO.Put_Line ("Failed to read link for " & Path);
            return;
         end if;

         IO.Put_Line
           ("SYMLINK " & Format_Path (Path) & " -> " &
            Format_Path (To_String (Target_Path)));
      else
         IO.Put_Line ("FILE " & Format_Path (Path));
      end if;
   end Handle_File;

begin
   declare
      S : constant String := Path (Open (Src));
   begin
      --  Remove the src directory name from absolute source path, to obtain
      --  the absolute path common to source and destination directories.
      --  +1 is for the missing / present in the absolute source path but not
      --  in the src directory name.
      Base_Path :=
        To_Unbounded_String (S (S'First .. S'Last - (Src'Length + 1)));
   end;

   A.Assert (
     Sync_Trees
       (Src, Dst, Mode => TIMESTAMP, Symlink_Mode => COPY_UNSAFE_SYMLINKS));

   IO.Put_Line ("Source tree");
   IO.Put_Line ("===========");
   Walk
     (Path            => Src,
      File_Handler    => Handle_File'Unrestricted_Access,
      Dir_Handler     => Handle_Dir'Unrestricted_Access,
      Follow_Symlinks => False);

   IO.Put_Line ("Destination tree");
   IO.Put_Line ("================");
   Walk
     (Path            => Dst,
      File_Handler    => Handle_File'Unrestricted_Access,
      Dir_Handler     => Handle_Dir'Unrestricted_Access,
      Follow_Symlinks => False);

   return A.Report;
end Test;
