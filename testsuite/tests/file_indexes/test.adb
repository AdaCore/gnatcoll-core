with GNATCOLL.OS.FSUtil;    use GNATCOLL.OS.FSUtil;
with GNATCOLL.Hash.Blake3;  use GNATCOLL.Hash.Blake3;
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
   Digest : Blake3_Digest;
   Total_Size : Integer := 0;
   File_1_Length : Integer;

begin
   IO.Put_Line ("GNATCOLL.OS.File_Indexes test");

   Assert (Integer (Indexed_Content_Size (File_Index)), 0);

   --  File 1

   FD := Open ("file_1", Mode => Write_Mode);
   Write (FD, "Initial content file 1");
   Close (FD);

   Hash (File_Index, "file_1", State, Digest);
   Assert
      (Digest,
       "ae85d337fcdd96a5e59c2729862d9698c014d60a71f9d970094c30526e779270");
   Assert
     (Hash (File_Index, "file_1"),
      "ae85d337fcdd96a5e59c2729862d9698c014d60a71f9d970094c30526e779270");
   Assert (Entry_State'Pos (State), Entry_State'Pos (NEW_FILE));

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

   Hash (File_Index, "file_1", State, Digest);
   Assert
      (Digest,
       "7a5bc31122404d1c8bb158ac7d14927423752f5a0eefd4093097c13bb222aa07");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UPDATED_FILE));

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

   return Report;
end Test;
