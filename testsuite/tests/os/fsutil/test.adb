with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.FS;     use GNATCOLL.OS.FS;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

begin
   IO.Put_Line ("GNATCOLL.OS.FSUtil test");

   declare
      SHA1_Str : constant String := String (SHA1 ("./data.txt"));
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
      Src_Name : constant String := "from_file";
      Dst_Name : constant String := "to_file";
      FD       : File_Descriptor;
   begin
      --  Create input file, and adds some content to it

      FD := Open (Src_Name, Mode => Write_Mode);
      Write (FD, "Input content");
      Close (FD);

      FD := Open (Src_Name, Mode => Read_Mode);
      declare
         File_Content : constant String := Read (FD);
      begin
         A.Assert (File_Content, "Input content");
      end;
      Close (FD);

      A.Assert (Copy_File (Src_Name, Dst_Name));

      --  Check that copy was successfull

      FD := Open (Dst_Name, Mode => Read_Mode);
      declare
         File_Content : constant String := Read (FD);
      begin
         A.Assert (File_Content, "Input content");
      end;
      Close (FD);

      A.Assert
        (not Copy_File (Src_Name, Dst_Name, Preserve_Timestamps => True));
      A.Assert
        (not Copy_File (Src_Name, Dst_Name, Preserve_Permissions => True));
      A.Assert
        (not Copy_File
           (Src_Name, Dst_Name, Preserve_Timestamps => True,
            Preserve_Permissions                    => True));

      declare
         Missing_File_Name : constant String := "missing_file";
      begin
         A.Assert (not Copy_File (Missing_File_Name, Dst_Name));
      end;
   end;

   --  Files content are check by a python post-test script, as it is
   --  easier to deal with huge files.
   A.Assert (Copy_File ("huge_file", "huge_file_copy"));

   return A.Report;
end Test;
