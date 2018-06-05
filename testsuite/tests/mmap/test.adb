with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;
with GNATcoll.Mmap;
with Interfaces;
with System;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package Mmap renames GNATcoll.Mmap;

   use type Mmap.Mapped_File;
   use type Mmap.Mapped_Region;

   type Buffer is array (Interfaces.Unsigned_64) of Character;
   type Buffer_Access is access Buffer;

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

   function To_Buffer is new Ada.Unchecked_Conversion
      (GNATCOLL.Mmap.Str_Access, Buffer_Access);

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
      pragma Import(C, C_Create_File, "c_create_file");

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
      Trace_Buffer    : Buffer_Access;
   begin
      Creation_Status := Create_File (Name, Size);
      A.Assert (Creation_Status, "Create test file of size" & Size'Img);

      Fd := GNATCOLL.Mmap.Open_Read (Name);
      A.Assert (Fd /= Mmap.Invalid_Mapped_File, "Open file");

      GNATCOLL.Mmap.Read
           (File    => Fd,
            Region  => Mr,
            Offset  => GNATCOLL.Mmap.File_Size (Size - 3),
            Length  => 3,
            Mutable => False);
      A.Assert (Mr /= Mmap.Invalid_Mapped_Region, "valid region?");
      A.Assert (Mmap.Is_Mmapped (Fd), "is mapped?");

      Trace_Buffer := To_Buffer (GNATCOLL.Mmap.Data (Mr));
      A.Assert (Trace_Buffer (0) = 'x', "expect character x");
      A.Assert (Trace_Buffer (1) = 'y', "expect character y");
      A.Assert (Trace_Buffer (2) = 'z', "expect character z");

      --  Free resources
      GNATCOLL.Mmap.Free (mr);
      GNATCOLL.Mmap.Close (Fd);
      GNAT.OS_Lib.Delete_File (Name, Delete_Status);
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
   Test_For_Size ("test3.txt", 16#0_ffff_0000#);
   Test_For_Size ("test4.txt", 16#0_ffff_fffe#);
   Test_For_Size ("test5.txt", 16#0_ffff_ffff#);
   if Mmap.File_Size'Size > 32 then
      --  System supports large file > 4GB. Ensure that GNATcoll.Mmap does too
      Test_For_Size ("test6.txt", 16#1_0000_1000#);
   end if;
   return A.Report;
end Test;
