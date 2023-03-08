with GNATCOLL.OS.FS; use GNATCOLL.OS.FS;
with GNATCOLL.OS;    use GNATCOLL.OS;
with Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;

begin
   IO.Put_Line ("GNATCOLL.OS.FS.Open test");
   declare
      FD : FS.File_Descriptor;
   begin
      FD := Open ("./non_existing_file", Mode => Read_Mode);
      A.Assert (FD = Invalid_FD);

      declare
         Buffer : String (1 .. 4_096);
         Size   : Integer;
      begin
         Size := Read (FD, Buffer);
         A.Assert
           (False, Msg => "OS_Error should be raised: bytes read:" & Size'Img);
      exception
         when OS_Error =>
            A.Assert (True, Msg => "OS_Error raised");
         when others   =>
            A.Assert (False, Msg => "Wrong exception raised");
      end;

      FD := Open ("./new_file", Mode => Write_Mode);
      A.Assert
        (FD /= Invalid_FD,
         Msg => "in write mode unexisting file should be created");
      Write (FD, "this is the first line");
      A.Assert (not Is_Console (FD));
      Close (FD);

      FD := Open ("./new_file", Mode => Read_Mode);
      declare
         File_Content : constant String := Read (FD);
      begin
         A.Assert (File_Content, "this is the first line");
      end;
      Close (FD);

      FD := Open ("./new_file", Mode => Append_Mode);
      Write (FD, " and it continues");
      Close (FD);

      FD := Open ("./new_file", Mode => Read_Mode);
      declare
         File_Content : constant String := Read (FD);
      begin
         A.Assert (File_Content, "this is the first line and it continues");
      end;
      Close (FD);

      --  Check that opening a file in write mode truncates the file to size 0

      declare
         Name : constant String := "non_empty_file";
      begin
         --  First create a file that contains several KB of data (just in case
         --  it makes a difference).

         FD := Open (Name, Mode => Write_Mode);
         for C in Character range 'A' .. 'Z' loop
            for Dummy in 1 .. 10 loop
               Write (FD, (1 .. 80 => C));
               Write (FD, (1 => ASCII.LF));
            end loop;
         end loop;

         Close (FD);

         --  Then try to overwrite it

         FD := Open (Name, Mode => Write_Mode);
         Write (FD, "not much here");
         Close (FD);

         --  Now, check that it contains data from the last write session only

         FD := Open (Name, Mode => Read_Mode);
         declare
            File_Content : constant String := Read (FD);
         begin
            A.Assert (File_Content, "not much here");
         end;
         Close (FD);
      end;
   end;
   return A.Report;
end Test;
