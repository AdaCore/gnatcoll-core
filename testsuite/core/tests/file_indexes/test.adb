with GNATCOLL.OS.FSUtil;    use GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.FS;        use GNATCOLL.OS.FS;
with GNATCOLL.File_Indexes; use GNATCOLL.File_Indexes;
with GNATCOLL.OS.Stat;
with Test_Assert; use Test_Assert;
with Ada.Text_IO;

function Test return Integer is
   package IO renames Ada.Text_IO;
   package St renames GNATCOLL.OS.Stat;

   FD : File_Descriptor;
   File_Index : GNATCOLL.File_Indexes.File_Index;
   State : Entry_State;
   Digest : File_Index_Digest;
   Total_Size : Integer := 0;
   File_1_Length : Integer;
   D1, D2, D3 : File_Index_Digest;
   Length     : Integer;

begin
   IO.Put_Line ("GNATCOLL.OS.File_Indexes test");

   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   --  File 1

   FD := Open ("file_1", Mode => Write_Mode);
   Write (FD, "Initial content file 1");
   Close (FD);

   --  Add a delay to consider file_1's hash as trusted
   delay 1.5;

   Hash (File_Index, "file_1", State, Digest);
   Assert
      (Digest,
       "ae85d337fcdd96a5e59c2729862d9698c014d60a71f9d970094c30526e779270");
   Assert (Entry_State'Pos (State), Entry_State'Pos (NEW_FILE));
   Assert
     (Hash (File_Index, "file_1"),
      "ae85d337fcdd96a5e59c2729862d9698c014d60a71f9d970094c30526e779270");

   File_1_Length := Integer (St.Length (St.Stat ("file_1")));
   Total_Size := File_1_Length;
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   --  File 2

   FD := Open ("file_2", Mode => Write_Mode);
   Write (FD, "Initial content file 2");
   Close (FD);

   Hash (File_Index, "file_2", State, Digest);
   Assert
      (Digest,
       "9702b5fec0d266e8ae1d22303572c2bb078fcfc727dcd70831eb81c863704e73");
   Assert (Entry_State'Pos (State), Entry_State'Pos (NEW_FILE));

   Total_Size := Total_Size + Integer (St.Length (St.Stat ("file_2")));
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   Assert (Remove_File ("file_2"));

   --  File 1

   Hash (File_Index, "file_1", State, Digest);
   Assert
      (Digest,
       "ae85d337fcdd96a5e59c2729862d9698c014d60a71f9d970094c30526e779270");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNCHANGED_FILE));

   FD := Open ("file_1", Mode => Append_Mode);
   Write (FD, " and new content");
   Close (FD);

   Hash (File_Index, "file_1", State, Digest, True);
   Assert
      (Digest,
       "7a5bc31122404d1c8bb158ac7d14927423752f5a0eefd4093097c13bb222aa07");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UPDATED_FILE));

   --  Save the index and reload it. Ensure that the hash and status stays the
   --  same.
   Save_Index (File_Index, "db.json");
   File_Index := Load_Index ("db.json");

   Hash (File_Index, "file_1", State, Digest);
   Assert
      (Digest,
       "7a5bc31122404d1c8bb158ac7d14927423752f5a0eefd4093097c13bb222aa07");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNCHANGED_FILE));

   Total_Size := Total_Size - File_1_Length;
   File_1_Length := Integer (St.Length (St.Stat ("file_1")));
   Total_Size := Total_Size + File_1_Length;
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   Hash (File_Index, "file_1", State, Digest);
   Assert
      (Digest,
       "7a5bc31122404d1c8bb158ac7d14927423752f5a0eefd4093097c13bb222aa07");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNCHANGED_FILE));

   Assert (Remove_File ("file_1"));

   Hash (File_Index, "file_1", State, Digest);
   Assert (Entry_State'Pos (State), Entry_State'Pos (REMOVED_FILE));
   Total_Size := Total_Size - File_1_Length;
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   Hash (File_Index, "file_1", State, Digest);
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNHASHABLE_FILE));

   --  File 3

   Hash (File_Index, "file_3", State, Digest);
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNHASHABLE_FILE));

   Clear_Cache (File_Index);
   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   Assert (Create_Directory ("dir_1"));
   Hash (File_Index, "dir_1", State, Digest);
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNHASHABLE_FILE));

   Assert (Remove_Directory ("dir_1"));

   --  Try to load invalid indexes
   --  First a non existing file
   File_Index := Load_Index ("non-existing-db.json");
   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   --  Then a non valid JSON file
   FD := Open ("invalid-db.json", Mode => Write_Mode);
   Write (FD, "{{");
   Close (FD);
   File_Index := Load_Index ("invalid-db.json");
   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   --  A JSON file that does not have the expected mimetype header
   FD := Open ("invalid-db.json", Mode => Write_Mode);
   Write (FD, "{}");
   Close (FD);
   File_Index := Load_Index ("invalid-db.json");
   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   --  A JSON file with the wrong mimetype
   FD := Open ("invalid-db.json", Mode => Write_Mode);
   Write
      (FD,
       "{""mimetype"": ""test/json""," &
       """last_update_time"": 1, ""total_size"": 1}");
   Close (FD);
   File_Index := Load_Index ("invalid-db.json");
   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   --  Generations

   FD := Open ("gen_1", Mode => Write_Mode);
   Write (FD, "one");
   Close (FD);

   --  A digest computed within a second of the file's modification time is
   --  not trusted, and only a trusted one is kept across a generation

   delay 1.5;

   D1 := Hash (File_Index, "gen_1");

   FD := Open ("gen_1", Mode => Append_Mode);
   Write (FD, " two");
   Close (FD);

   D2 := Hash (File_Index, "gen_1");
   Assert (D2 /= D1, "a change is seen when no generation is open");

   --  Force_Cache trusts the digest it computes, so the entry is trusted
   --  again without waiting out the modification time

   D2 := Hash (File_Index, "gen_1", Force_Cache => True);

   Start_Generation (File_Index);
   Assert (Hash (File_Index, "gen_1"), D2, "unchanged file in a generation");

   FD := Open ("gen_1", Mode => Append_Mode);
   Write (FD, " three");
   Close (FD);

   Assert
     (Hash (File_Index, "gen_1"), D2,
      "a change during a generation is not seen without Force_Cache");

   D3 := Hash (File_Index, "gen_1", Force_Cache => True);
   Assert (D3 /= D2, "Force_Cache sees a change made during a generation");

   FD := Open ("gen_1", Mode => Append_Mode);
   Write (FD, " four");
   Close (FD);

   Assert
     (Hash (File_Index, "gen_1"), D3, "still cached in the same generation");

   Start_Generation (File_Index);
   Assert
     (Hash (File_Index, "gen_1") /= D3, "a new generation sees the change");

   --  Path_Is_Normalized

   Clear_Cache (File_Index);

   FD := Open ("norm_1", Mode => Write_Mode);
   Write (FD, "normalized");
   Close (FD);

   D1 := Hash (File_Index, "norm_1");
   Length := Integer (St.Length (St.Stat ("norm_1")));
   Assert (Integer (Indexed_Content_Size (File_Index)), Length);

   --  Left unset, the path is normalized first, so this is the same entry

   Assert (Hash (File_Index, "./norm_1"), D1);
   Assert
     (Integer (Indexed_Content_Size (File_Index)), Length,
      "a path normalizing to a known one reuses its entry");

   --  Set, the path is taken as it comes: the same file, indexed twice

   Assert (Hash (File_Index, "./norm_1", Path_Is_Normalized => True), D1);
   Assert
     (Integer (Indexed_Content_Size (File_Index)), 2 * Length,
      "an unnormalized path taken as-is gets its own entry");

   return Report;
end Test;
