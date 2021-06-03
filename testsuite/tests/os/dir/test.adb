with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.Dir;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package Dir renames GNATCOLL.OS.Dir;

   H : Dir.Dir_Handle;
   E : Dir.Dir_Entry;
   Entry_Number : Integer := 0;
begin
   IO.Put_Line ("Test for GNATCOLL.OS.Dir");
   H := Dir.Open ("test1");
   loop
      E := Dir.Read (H);
      exit when Dir.End_Of_Iteration (E);
      Entry_Number := Entry_Number + 1;
   end loop;
   A.Assert (Entry_Number, 1000, "expect 1000 entries in the directory");
   Dir.Close (H);
   return A.Report;
end Test;
