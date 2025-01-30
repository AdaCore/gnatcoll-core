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
   Sym_Number   : Integer := 0;
begin
   IO.Put_Line ("Test for GNATCOLL.OS.Dir");
   --  With follow symlinks we should not find any symlink
   H := Dir.Open ("test1");
   loop
      E := Dir.Read (H, Follow_Symlinks => True);
      exit when Dir.End_Of_Iteration (E);
      Entry_Number := Entry_Number + 1;
      if Stat.Is_File (Dir.Attributes (E)) then
         File_Number := File_Number + 1;
      elsif Stat.Is_Directory (Dir.Attributes (E)) then
         Dir_Number := Dir_Number + 1;
      elsif Stat.Is_Symbolic_Link (Dir.Attributes (E)) then
         Sym_Number := Sym_Number + 1;
      end if;
   end loop;
   A.Assert (Entry_Number, 2, "expect 2 entries in the directory");
   A.Assert (File_Number, 2, "expect 2 files in the directory");
   A.Assert (Dir_Number, 0, "expect 0 directories in the directory");
   A.Assert (Sym_Number, 0, "expect 0 symlinks in the directory");
   Dir.Close (H);

   Entry_Number := 0;
   Dir_Number   := 0;
   File_Number  := 0;
   Sym_Number   := 0;

   --  With follow symlinks disable we should find a link
   H := Dir.Open ("test1");
   loop
      E := Dir.Read (H, Follow_Symlinks => False);
      exit when Dir.End_Of_Iteration (E);
      Entry_Number := Entry_Number + 1;
      if Stat.Is_File (Dir.Attributes (E)) then
         File_Number := File_Number + 1;
      elsif Stat.Is_Directory (Dir.Attributes (E)) then
         Dir_Number := Dir_Number + 1;
      elsif Stat.Is_Symbolic_Link (Dir.Attributes (E)) then
         Sym_Number := Sym_Number + 1;
      end if;
   end loop;
   A.Assert (Entry_Number, 2, "expect 2 entries in the directory");
   A.Assert (File_Number, 1, "expect 2 files in the directory");
   A.Assert (Dir_Number, 0, "expect 0 directories in the directory");
   A.Assert (Sym_Number, 1, "expect 0 symlinks in the directory");
   Dir.Close (H);

   return A.Report;
end Test;
