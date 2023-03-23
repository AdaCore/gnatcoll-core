with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.FS;     use GNATCOLL.OS.FS;
with GNATCOLL.OS.Stat;   use GNATCOLL.OS.Stat;
with Ada.Calendar;       use Ada.Calendar;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

begin
   IO.Put_Line ("GNATCOLL.OS.FSUtil test");

   declare
      SHA1_Str   : constant String := String (SHA1 ("./data.txt"));
      SHA256_Str : constant String := String (SHA256 ("./data.txt"));
   begin
      A.Assert
        (SHA1_Str, "0a963bb418c97dff49ec8d166834ee23a912a0e9", "Check sha1");
      A.Assert
        (SHA256_Str,
         "93827371a7c9502512672999a530fb55999b054d4a05af3c2c02290bdded0d4c",
         "Check sha256");
   end;

   --  Check file copying
   declare
      function Check_File_Content
        (File_Name : String; Expected_Content : String) return Boolean;

      function Check_File_Content
        (File_Name : String; Expected_Content : String) return Boolean
      is
         FD : constant File_Descriptor := Open (File_Name, Mode => Read_Mode);
      begin
         declare
            Content : constant String := Read (FD);
         begin
            Close (FD);
            return Content = Expected_Content;
         end;
      end Check_File_Content;

      From_Name      : constant String := "from_file";
      Creation_Delay : constant        := 1.000_01;
      --  Delay between timestamp test files, to ensure that timestamps
      --  are not initially the same. Shall be at least 1 second, so we
      --  do not have the same timestamp if nano second to second conversion
      --  truncates.

      FD : File_Descriptor;

   begin
      --  Create input file, and adds some content to it

      FD := Open (From_Name, Mode => Write_Mode);
      Write (FD, "Input content");
      Close (FD);

      A.Assert (Check_File_Content (From_Name, "Input content"));

      declare
         To_Name           : constant String := "to_file";
         Missing_File_Name : constant String := "missing_file";
      begin
         A.Assert (Copy_File (From_Name, To_Name));

         --  Check that copy was successfull

         A.Assert (Check_File_Content (To_Name, "Input content"));

         A.Assert (not Copy_File (Missing_File_Name, To_Name));
      end;

      declare
         To_Name        : constant String          := "timestamp_file";
         From_File_Attr : constant File_Attributes :=
           GNATCOLL.OS.Stat.Stat (From_Name);

      begin
         A.Assert (Exists (From_File_Attr));

         --  Add a delay, to ensure that timestamps are not initially
         --  the same.
         delay (Creation_Delay);
         A.Assert
           (Copy_File (From_Name, To_Name, Preserve_Timestamps => True));

         --  Check timestamps

         declare
            To_File_Attr : constant File_Attributes :=
              GNATCOLL.OS.Stat.Stat (To_Name);
         begin
            A.Assert
              (Modification_Time (From_File_Attr) =
               Modification_Time (To_File_Attr));
         end;

         --  Ensure that copying with timestamps preservation does not
         --  impact content copy.
         A.Assert (Check_File_Content (From_Name, "Input content"));
         A.Assert (Check_File_Content (To_Name, "Input content"));

      end;

      --  Test timestamps copy alone, as Windows CopyFile copies
      --  timestamps by default.
      declare
         To_Name        : constant String          := "another_timestamp_file";
         From_File_Attr : constant File_Attributes :=
           GNATCOLL.OS.Stat.Stat (From_Name);

      begin
         A.Assert (Exists (From_File_Attr));

         --  Add a delay, to ensure that timestamps are not initially
         --  the same
         delay (Creation_Delay);
         FD := Open (To_Name, Mode => Write_Mode);
         Close (FD);

         declare
            To_File_Attr : File_Attributes := GNATCOLL.OS.Stat.Stat (To_Name);
         begin

            --  Ensure that timestamps are initially different
            A.Assert
              (Modification_Time (From_File_Attr) /=
               Modification_Time (To_File_Attr));

            A.Assert (Copy_Timestamps (From_Name, To_Name));

            To_File_Attr := GNATCOLL.OS.Stat.Stat (To_Name);

            A.Assert
              (Modification_Time (From_File_Attr) =
               Modification_Time (To_File_Attr));
         end;

         A.Assert (not Copy_Timestamps ("missing_file", To_Name));
      end;
   end;

   --  Files content are check by a python post-test script, as it is
   --  easier to deal with huge files.

   A.Assert (Copy_File ("huge_file", "huge_file_copy"));

   --  Test file removal
   declare
      FD : File_Descriptor;
   begin
      FD := Open ("remove_file_test", Mode => Write_Mode);
      A.Assert (FD /= Invalid_FD);
      Close (FD);

      FD := Open ("remove_file_test", Mode => Read_Mode);
      A.Assert (FD /= Invalid_FD);
      Close (FD);

      A.Assert (Remove_File ("remove_file_test"));

      FD := Open ("remove_file_test", Mode => Read_Mode);
      A.Assert (FD = Invalid_FD);

      A.Assert (not Remove_File ("remove_file_test"));
   end;

   --  Create_Directory tests

   A.Assert (not Create_Directory ("already_existing_dir"));

   A.Assert (Create_Directory ("Not_already_existing_dir"));

   A.Assert (not Create_Directory ("Not_already_existing_dir"));

   A.Assert (Create_Directory ("Not_already_existing_dir/sub_dir"));

   return A.Report;
end Test;
