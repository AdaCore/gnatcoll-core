with Test_Assert;
with GNAT.IO;
with GNATCOLL.OS.Dir;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package Dir renames GNATCOLL.OS.Dir;

   Entry_Number : Integer := 0;
   Dir_Number   : Integer := 0;
   File_Number  : Integer := 0;

   procedure Process_File (D : Dir.Dir_Handle; E : Dir.Dir_Entry);

   function Process_Dir (D : Dir.Dir_Handle; E : Dir.Dir_Entry) return Boolean;
   function Process_No_Enter
      (D : Dir.Dir_Handle; E : Dir.Dir_Entry) return Boolean;

   procedure Process_File (D : Dir.Dir_Handle; E : Dir.Dir_Entry) is
      pragma Unreferenced (D);
      pragma Unreferenced (E);
   begin
      Entry_Number := Entry_Number + 1;
      File_Number := File_Number + 1;
   end Process_File;

   function Process_Dir (D : Dir.Dir_Handle; E : Dir.Dir_Entry) return Boolean
   is
      pragma Unreferenced (D);
      pragma Unreferenced (E);
   begin
      Entry_Number := Entry_Number + 1;
      Dir_Number := Dir_Number + 1;
      return True;
   end Process_Dir;

   function Process_No_Enter
      (D : Dir.Dir_Handle; E : Dir.Dir_Entry)
      return Boolean
   is
      pragma Unreferenced (D);
      pragma Unreferenced (E);
   begin
      Entry_Number := Entry_Number + 1;
      Dir_Number := Dir_Number + 1;
      return False;
   end Process_No_Enter;

begin
   IO.Put_Line ("Test for GNATCOLL.OS.Dir");
   Dir.Walk
      ("test1",
       File_Handler => Process_File'Unrestricted_Access,
       Dir_Handler  => Process_Dir'Unrestricted_Access);
   A.Assert (Entry_Number, 102000, "expect 102000 entries in the directory");
   A.Assert (File_Number, 1000, "expect 1000 files in the directory");
   A.Assert (Dir_Number, 101000, "expect 101000 directories in the directory");

   Entry_Number := 0;
   Dir_Number := 0;
   File_Number := 0;

   Dir.Walk
      ("test1",
       File_Handler => Process_File'Unrestricted_Access,
       Dir_Handler  => null);
   A.Assert (Entry_Number, 1000, "expect 1000 entries in the directory");
   A.Assert (File_Number, 1000, "expect 1000 files in the directory");
   A.Assert (Dir_Number, 0, "expect 0 directories in the directory");

   Entry_Number := 0;
   Dir_Number := 0;
   File_Number := 0;

   Dir.Walk
      ("test1",
       File_Handler => Process_File'Unrestricted_Access,
       Dir_Handler  => Process_Dir'Unrestricted_Access,
       Max_Depth    => 1);
   A.Assert (Entry_Number, 2000, "expect 2000 entries in the directory");
   A.Assert (File_Number, 1000, "expect 1000 files in the directory");
   A.Assert (Dir_Number, 1000, "expect 1000 directories in the directory");

   Entry_Number := 0;
   Dir_Number := 0;
   File_Number := 0;

   Dir.Walk
      ("test1",
       File_Handler => Process_File'Unrestricted_Access,
       Dir_Handler  => Process_No_Enter'Unrestricted_Access);
   A.Assert (Entry_Number, 2000, "expect 2000 entries in the directory");
   A.Assert (File_Number, 1000, "expect 1000 files in the directory");
   A.Assert (Dir_Number, 1000, "expect 1000 directories in the directory");

   return A.Report;
end Test;
