with Ada.Calendar; use Ada.Calendar;
with GNATCOLL.OS.FSUtil; use GNATCOLL.OS.FSUtil;
with Ada.Text_IO;
with GNATCOLL.Hash.xxHash;
with GNATCOLL.Hash.Blake3;
with GNATCOLL.OS.FS;
pragma Warnings (Off, ".*is an internal GNAT unit");
with System.String_Hash;
pragma Warnings (On, ".*is an internal GNAT unit");
with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   package FS renames GNATCOLL.OS.FS;

   Big_File_Size : constant Long_Float := 100_000_000.0 / 1024.0 / 1024.0;
   Start : Time := Ada.Calendar.Clock;
   Blake3_Duration, SHA256_Duration, XXH3_Duration : Duration;
   Blake3_Result : GNATCOLL.Hash.Blake3.Blake3_Digest;
   XXH3_Result : GNATCOLL.Hash.xxHash.XXH3_Digest;
   Short_XXH3_Duration, Short_GNAT_Duration, Short_Blake3_Duration : Duration;

   Digest : GNATCOLL.Hash.Blake3.Blake3_Digest;

   type Uint_32 is mod 2 ** 32;
   R32, R : Uint_32;
   function H is new System.String_Hash.Hash (Character, String, Uint_32);
   FD : FS.File_Descriptor;

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
   for J in 1 .. 10_000_000 loop
      FS.Write (FD, "0123456789");
   end loop;
   FS.Close (FD);

   Ada.Text_IO.Put_Line ("File hashes");
   --  Read at least once the file to have it in cache

   Digest := GNATCOLL.Hash.Blake3.Blake3_File_Hash ("buffer.txt");
   Ada.Text_IO.Put_Line (Digest);

   --  Blake3
   Start := Ada.Calendar.Clock;
   Blake3_Result := GNATCOLL.Hash.Blake3.Blake3_File_Hash ("buffer.txt");
   Blake3_Duration := Ada.Calendar.Clock - Start;
   A.Assert (
      Blake3_Result,
      "2b36a5c910c118025c67e8661b9826367be8294172c17d8d4a9405955f62f94f",
      "Check blake3 result");
   Ada.Text_IO.Put ("blake3: " & Blake3_Result & " ");
   Put_Speed (Blake3_Duration, Big_File_Size);

   Start := Ada.Calendar.Clock;
   Ada.Text_IO.Put
      ("sha256: " & SHA256 ("buffer.txt") & " ");
   SHA256_Duration := Ada.Calendar.Clock - Start;
   Put_Speed (SHA256_Duration, Big_File_Size);

   Start := Ada.Calendar.Clock;
   XXH3_Result := GNATCOLL.Hash.xxHash.XXH3_File_Hash ("buffer.txt");
   XXH3_Duration := Ada.Calendar.Clock - Start;
   Ada.Text_IO.Put ("xxh3: " & XXH3_Result & " ");
   A.Assert (XXH3_Result, "036b78ecf2539b1b", "Check XXH3 result");
   Put_Speed (XXH3_Duration, Big_File_Size);

   Ada.Text_IO.Put_Line ("Short in memory string hash");

   --  Test default gnat hash function
   Start := Ada.Calendar.Clock;
   for J in 1 .. 100_000_000 loop
      R32 := H ("01234567890123456789");
   end loop;
   Short_GNAT_Duration := Ada.Calendar.Clock - Start;
   Ada.Text_IO.Put ("ada string hash (100_000_000 calls of 20bytes string), ");
   Put_Speed (Short_GNAT_Duration, 20.0 * 100_000_000.0 / 1024.0 / 1024.0);
   Ada.Text_IO.Put_Line (R32'Img);

   --  Test xxh3
   Start := Ada.Calendar.Clock;
   for J in 1 .. 100_000_000 loop
      R := Uint_32
         (GNATCOLL.Hash.xxHash.XXH3 ("01234567890123456789"));
   end loop;
   Short_XXH3_Duration := Ada.Calendar.Clock - Start;
   Ada.Text_IO.Put ("xxh3 hash (100_000_000 calls of 20bytes string), ");
   Put_Speed (Short_XXH3_Duration, 20.0 * 100_000_000.0 / 1024.0 / 1024.0);
   Ada.Text_IO.Put_Line (R'Img);

   --  Test blake3
   Start := Ada.Calendar.Clock;
   for J in 1 .. 100_000_000 loop
      R := Uint_32
         (GNATCOLL.Hash.Blake3.Blake3 ("01234567890123456789"));
   end loop;
   Short_Blake3_Duration := Ada.Calendar.Clock - Start;
   Ada.Text_IO.Put ("xxh3 hash (100_000_000 calls of 20bytes string), ");
   Put_Speed (Short_Blake3_Duration, 20.0 * 100_000_000.0 / 1024.0 / 1024.0);
   Ada.Text_IO.Put_Line (R'Img);
   return A.Report;

end Test;
