with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO;
with GNATCOLL.OS.FS;
with Test_Assert;
with GNATCOLL.Buffer; use GNATCOLL.Buffer;

function Test return Integer is
   package A renames Test_Assert;
   package FS renames GNATCOLL.OS.FS;

   FD : FS.File_Descriptor;
   Start : Time;
   Test_Duration : Duration;
   Line, Column : Integer;

   procedure Put_Speed (Test_Duration : Duration; Size : Long_Float);
   procedure Put_Speed (Test_Duration : Duration; Size : Long_Float) is
   begin
      Ada.Text_IO.Put_Line
        (Integer'Image
           (Integer (Size / Long_Float (Test_Duration))) & "MB/s");
   end Put_Speed;
begin
   --  Create a 100Mo file
   FD := FS.Open (Path => "./buffer.txt", Mode => FS.Write_Mode);
   for J in 1 .. 1_000_000 loop
      FS.Write (FD, "012345678" & ASCII.LF);
   end loop;
   FS.Close (FD);

   Ada.Text_IO.Put ("Count zeros (no releases, mmap)");
   --  Read at least once the file to have it in cache
   Start := Ada.Calendar.Clock;
   declare
      R : Reader := Open ("./buffer.txt");
      C : Character;
      Number_Of_Zeros : Integer := 0;
   begin
      while R.Next (C) loop
         if C = '0' then
            Number_Of_Zeros := Number_Of_Zeros + 1;
         end if;
      end loop;
      Test_Duration := Ada.Calendar.Clock - Start;
      Put_Speed (Test_Duration, 10.0 * 1_000_000.0 / 1024.0 / 1024.0);
      A.Assert (Number_Of_Zeros, 1_000_000, "expect 1_000_000 '0'");
      Current_Text_Position (R, Line, Column);
      A.Assert (Line, 1_000_000, "expect position to be line 1_000_000");
      A.Assert (Column, 10, "expect column to be 10");
   end;

   Ada.Text_IO.Put ("Count zeros (release characters, mmap)");
   Start := Ada.Calendar.Clock;
   declare
      R : Reader := Open ("./buffer.txt");
      C : Character;
      Number_Of_Zeros : Integer := 0;
   begin
      while R.Next (C) loop
         if C = '0' then
            Number_Of_Zeros := Number_Of_Zeros + 1;
         end if;
         R.Release;
      end loop;
      Test_Duration := Ada.Calendar.Clock - Start;
      Put_Speed (Test_Duration, 10.0 * 1_000_000.0 / 1024.0 / 1024.0);
      A.Assert (Number_Of_Zeros, 1_000_000, "expect 1_000_000 '0'");
      Current_Text_Position (R, Line, Column);
      A.Assert (Line, 1_000_000, "expect position to be line 1_000_000");
      A.Assert (Column, 10, "expect column to be 10");
   end;

   Ada.Text_IO.Put ("Count zeros (local offset, no release, mmap)");
   Start := Ada.Calendar.Clock;
   declare
      R : Reader := Open ("./buffer.txt");
      C : Character;
      Number_Of_Zeros : Integer := 0;
      Offset : Integer := R.Window_Offset;
   begin
      while R.Next (C, Offset) loop
         if C = '0' then
            Number_Of_Zeros := Number_Of_Zeros + 1;
         end if;
      end loop;
      R.Set_Window_Offset (Offset);
      Test_Duration := Ada.Calendar.Clock - Start;
      Put_Speed (Test_Duration, 10.0 * 1_000_000.0 / 1024.0 / 1024.0);
      A.Assert (Number_Of_Zeros, 1_000_000, "expect 1_000_000 '0'");
      Current_Text_Position (R, Line, Column);
      A.Assert (Line, 1_000_000, "expect position to be line 1_000_000");
      A.Assert (Column, 10, "expect column to be 10");
   end;

   Ada.Text_IO.Put ("Count zeros (file descriptor, release)");
   Start := Ada.Calendar.Clock;
   declare
      FD : constant FS.File_Descriptor := FS.Open ("./buffer.txt");
      R : Reader := Open (FD);
      C : Character;
      Number_Of_Zeros : Integer := 0;
   begin
      while R.Next (C) loop
         if C = ASCII.LF then
            Number_Of_Zeros := Number_Of_Zeros + 1;
         end if;
         R.Release;
      end loop;
      Test_Duration := Ada.Calendar.Clock - Start;
      Put_Speed (Test_Duration, 10.0 * 1_000_000.0 / 1024.0 / 1024.0);
      A.Assert (Number_Of_Zeros, 1_000_000, "expect 1_000_000 '0'");
      Current_Text_Position (R, Line, Column);
      A.Assert (Line, 0, "expect position to be line 0");
      A.Assert (Column, 0, "expect column to be 0");
      FS.Close (FD);
   end;

   Ada.Text_IO.Put ("Count zeros (file descriptor, no release)");
   Start := Ada.Calendar.Clock;
   declare
      FD : constant FS.File_Descriptor := FS.Open ("./buffer.txt");
      R : Reader := Open (FD);
      C : Character;
      Number_Of_Zeros : Integer := 0;
   begin
      while R.Next (C) loop
         if C = '0' then
            Number_Of_Zeros := Number_Of_Zeros + 1;
         end if;
      end loop;
      Test_Duration := Ada.Calendar.Clock - Start;
      Put_Speed (Test_Duration, 10.0 * 1_000_000.0 / 1024.0 / 1024.0);
      A.Assert (Number_Of_Zeros, 1_000_000, "expect 1_000_000 '0'");
      Current_Text_Position (R, Line, Column);
      A.Assert (Line, 0, "expect position to be line 0");
      A.Assert (Column, 0, "expect column to be 0");
      FS.Close (FD);
   end;

   Ada.Text_IO.Put_Line ("Check index behavior (file descriptor, release)");
   declare
      FD : constant FS.File_Descriptor := FS.Open ("./buffer.txt");
      R : Reader := Open (FD);
      C : Character;
   begin
      if R.Next (C) then
         A.Assert ("" & R.Current_Char, "0", "expect character '0'");
         A.Assert (Integer (R.Current_Position), 0, "expect position 0");
         A.Assert (R.Check ("1") = True, "Buffer next char is '1'");
         A.Assert ("" & R.Current_Char, "1", "expect character '1'");
         A.Assert (Integer (R.Current_Position), 1, "expect position 1");
         A.Assert (R.Check ("1") = False, "Buffer next char is not '1'");
         A.Assert ("" & R.Current_Char, "1", "expect character '1'");
         A.Assert (Integer (R.Current_Position), 1, "expect position 1");
      end if;
      R.Release;
      FS.Close (FD);
   end;

   return A.Report;

end Test;
