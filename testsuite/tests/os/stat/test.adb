with Ada.Calendar; use Ada.Calendar;
with GNATCOLL.OS.Stat; use GNATCOLL.OS.Stat;
with GNATCOLL.OS.Constants; use GNATCOLL.OS.Constants;
with GNATCOLL.OS;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

   use type GNATCOLL.OS.OS_Type;

begin
   IO.Put_Line ("GNATCOLL.OS.Stat test");
   declare
      FA : File_Attributes;
      D  : Duration;
   begin
      FA := Stat ("directory");
      IO.Put_Line (Image (FA));
      A.Assert (not Is_File (FA), Msg => "check that directory is not a file");
      A.Assert (Is_Directory (FA),
                Msg => "check that directory is a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that directory is not a symbolic link");

      FA := Stat ("regular_file");
      IO.Put_Line (Image (FA));
      A.Assert (Is_File (FA), Msg => "check that regular_file is a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that regular_file is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that regular_file is not a symbolic link");
      D := Clock - Modification_Time (FA);
      A.Assert
         (D < 100.0,
          Msg => "check that modification time is less than 100s in the past: "
          & D'Img);

      IO.Put_Line ("test file with utf-8 name");
      FA := Stat (Character'Val (16#C3#) & Character'Val (16#A9#) & ".txt");
      IO.Put_Line (Image (FA));
      A.Assert (Is_File (FA), Msg => "check that utf-8 file is a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that utf-8 file is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that utf-8 file is not a symbolic link");

      IO.Put_Line ("test file with invalid utf-8 name");
      FA := Stat (Character'Val (16#FE#) & "");
      IO.Put_Line (Image (FA));
      A.Assert (not Exists (FA),
                Msg => "check that non_existing does not exist");
      A.Assert (not Is_File (FA),
                Msg => "check that non_existing is not a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that non_existing is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that non_existing is not a symbolic link");

      FA := Stat ("non_existing");
      A.Assert (not Exists (FA),
                Msg => "check that non_existing does not exist");
      A.Assert (not Is_File (FA),
                Msg => "check that non_existing is not a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that non_existing is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that non_existing is not a symbolic link");

      FA := Stat ("directory/../regular_file");
      A.Assert (Is_File (FA), Msg => "check that regular_file is a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that regular_file is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that regular_file is not a symbolic link");

      FA := Stat ("directory/../non_existing");
      A.Assert (not Exists (FA),
                Msg => "check that non_existing does not exist");
      A.Assert (not Is_File (FA),
                Msg => "check that non_existing is not a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that non_existing is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that non_existing is not a symbolic link");

      FA := Stat ("directory_non_existent/../regular_file");
      A.Assert (Is_File (FA), Msg => "check that regular_file is a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that regular_file is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that regular_file is not a symbolic link");

      FA := Stat ("directory_non_existent/../non_existing");
      A.Assert (not Exists (FA),
                Msg => "check that non_existing does not exist");
      A.Assert (not Is_File (FA),
                Msg => "check that non_existing is not a file");
      A.Assert (not Is_Directory (FA),
                Msg => "check that non_existing is not a dir");
      A.Assert (not Is_Symbolic_Link (FA),
                Msg => "check that non_existing is not a symbolic link");

      if OS /= GNATCOLL.OS.Windows then
         FA := Stat ("sym_link");
         A.Assert (Is_File (FA),
                   Msg => "check that sym_link target is a file");
         A.Assert (not Is_Directory (FA),
                   Msg => "check that sym_link target is not a dir");
         A.Assert (not Is_Symbolic_Link (FA),
                   Msg => "check that sym_link target is not a symbolic link");
         FA := Stat ("sym_link", Follow_Symlinks => False);
         A.Assert (not Is_File (FA),
                   Msg => "check that sym_link is not a file");
         A.Assert (not Is_Directory (FA),
                   Msg => "check that sym_link is not a dir");
         A.Assert (Is_Symbolic_Link (FA),
                   Msg => "check that sym_link is a symbolic link");
      end if;

   end;

   return A.Report;
end Test;
