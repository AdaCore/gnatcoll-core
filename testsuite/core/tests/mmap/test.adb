with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNATCOLL.Mmap;
with System;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package Mmap renames GNATCOLL.Mmap;

   use type Mmap.Mapped_File;
   use type Mmap.Mapped_Region;

   function Create_File
      (Filename : String;
       Size     : Long_Long_Integer)
       return Boolean;
   --  Create a file of size Size filled with zeros excepts the last three
   --  characters that should contain the string "xyz". Return False if the
   --  file cannot be created on disk. Method is efficient even for large
   --  files as it create a sparse file.

   procedure Test_For_Size (Name : String; Size : Long_Long_Integer);
   --  Create a file of size Size called Name and fill it with 0 except last 3
   --  characters that should contain "xyz". Ensure that we can read the file
   --  correctly using GNATcoll.Mmap facilities.

   -----------------
   -- Create_File --
   -----------------

   function Create_File
      (Filename : String;
       Size     : Long_Long_Integer)
       return Boolean
   is
      function C_Create_File
         (CFile : System.Address;
          Size  : Long_Long_Integer)
         return Integer;
      --  internal C function. Return 0 in case of failure.
      pragma Import (C, C_Create_File, "c_create_file");

      F : aliased String := Filename & ASCII.NUL;
      Status : Integer;
   begin
      Status := C_Create_File (F'Address, Size);
      if Status = 0 then
         return False;
      else
         return True;
      end if;
   end Create_File;

   -------------------
   -- Test_For_Size --
   -------------------

   procedure Test_For_Size (Name : String; Size : Long_Long_Integer) is
      Fd              : Mmap.Mapped_File;
      Mr              : Mmap.Mapped_Region;
      Creation_Status : Boolean;
      Delete_Status   : Boolean;
      Trace_Buffer    : Mmap.Str_Access;
   begin
      Creation_Status := Create_File (Name, Size);
      A.Assert (Creation_Status, "Create test file of size" & Size'Img);

      Fd := Mmap.Open_Read (Name);
      A.Assert (Fd /= Mmap.Invalid_Mapped_File, "Open file");

      GNATCOLL.Mmap.Read
           (File    => Fd,
            Region  => Mr,
            Offset  => Mmap.File_Size (Size - 3),
            Length  => 3,
            Mutable => False);
      A.Assert (Mr /= Mmap.Invalid_Mapped_Region, "valid region?");
      A.Assert (Mmap.Is_Mmapped (Fd), "is mapped?");

      Trace_Buffer := Mmap.Data (Mr);
      A.Assert (Trace_Buffer (1) = 'x', "expect character x");
      A.Assert (Trace_Buffer (2) = 'y', "expect character y");
      A.Assert (Trace_Buffer (3) = 'z', "expect character z");
      A.Assert (Mmap.Last (Mr) = 3, "expect size 3, got" & Mmap.Last (Mr)'Img);

      --  Free resources
      GNATCOLL.Mmap.Free (Mr);
      GNATCOLL.Mmap.Close (Fd);
      GNAT.OS_Lib.Delete_File (Name, Delete_Status);
      A.Assert (Delete_Status);
      A.Assert (True, "end of test for size" & Size'Img);
   end Test_For_Size;

begin
   --  close tests
   declare
      FD : Mmap.Mapped_File := Mmap.Invalid_Mapped_File;
   begin
      Mmap.Close (FD);
      A.Assert (True, "call to close on invalid mapped file should work");
   exception
      when others =>
         A.Assert (False,
                   "unexpected exception on closing invalid mapped file");
   end;

   --  tests on various file size
   Test_For_Size ("test1.txt", 8);
   Test_For_Size ("test2.txt", 1000);
   if GNAT.Directory_Operations.Dir_Separator = '\' or else
      Mmap.File_Size'Size > 32
   then
      --  On windows 32bits and 64bits unixes test file length close to 4Go
      Test_For_Size ("test3.txt", 16#0_ffff_0000#);
      Test_For_Size ("test4.txt", 16#0_ffff_fffe#);
      Test_For_Size ("test5.txt", 16#0_ffff_ffff#);
   else
      --  On 32bits unixes the maximum offset is 2**31 (as lseek takes a
      --  signed long)
      Test_For_Size ("test3.txt", 16#0_0fff_0000#);
      Test_For_Size ("test4.txt", 16#0_0fff_fffe#);
      Test_For_Size ("test5.txt", 16#0_0fff_ffff#);
   end if;

   if Mmap.File_Size'Size > 32 then
      --  System supports large file > 4GB. Ensure that GNATcoll.Mmap does too
      Test_For_Size ("test6.txt", 16#1_0000_1000#);
   end if;
   return A.Report;
end Test;
