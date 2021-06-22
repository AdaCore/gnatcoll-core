with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.Dir;
with GNATCOLL.OS.Stat;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package Dir renames GNATCOLL.OS.Dir;
   package Stat renames GNATCOLL.OS.Stat;

   H : Dir.Dir_Handle;
   E : Dir.Dir_Entry;
   Entry_Number : Integer := 0;
   Dir_Number   : Integer := 0;
   File_Number  : Integer := 0;
begin
   IO.Put_Line ("Test for GNATCOLL.OS.Dir");
   H := Dir.Open ("test1");
   loop
      E := Dir.Read (H);
      exit when Dir.End_Of_Iteration (E);
      Entry_Number := Entry_Number + 1;
      if Stat.Is_File (Dir.Attributes (E)) then
         File_Number := File_Number + 1;
      elsif Stat.Is_Directory (Dir.Attributes (E)) then
         Dir_Number := Dir_Number + 1;
      end if;
   end loop;
   A.Assert (Entry_Number, 2000, "expect 1000 entries in the directory");
   A.Assert (File_Number, 1000, "expect 1000 files in the directory");
   A.Assert (Dir_Number, 1000, "expect 1000 directories in the directory");
   Dir.Close (H);
   return A.Report;
end Test;
