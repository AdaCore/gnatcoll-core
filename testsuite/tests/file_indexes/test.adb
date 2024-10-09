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
   Digest : SHA1_Digest;
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
   Assert (Digest, "d50b4407ded5f724d783dae8df84b78f7aa2b058");
   Assert
     (Hash (File_Index, "file_1"), "d50b4407ded5f724d783dae8df84b78f7aa2b058");
   Assert (Entry_State'Pos (State), Entry_State'Pos (NEW_FILE));

   File_1_Length := Integer (St.Length (St.Stat ("file_1")));
   Total_Size := File_1_Length;
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   --  File 2

   FD := Open ("file_2", Mode => Write_Mode);
   Write (FD, "Initial content file 2");
   Close (FD);

   Hash (File_Index, "file_2", State, Digest);
   Assert (Digest, "456e1639ca781486249d61e64bae7a852a3ef4ed");
   Assert (Entry_State'Pos (State), Entry_State'Pos (NEW_FILE));

   Total_Size := Total_Size + Integer (St.Length (St.Stat ("file_2")));
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   Assert (Remove_File ("file_2"));

   --  File 1

   Hash (File_Index, "file_1", State, Digest);
   Assert (Digest, "d50b4407ded5f724d783dae8df84b78f7aa2b058");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UNCHANGED_FILE));

   FD := Open ("file_1", Mode => Append_Mode);
   Write (FD, " and new content");
   Close (FD);

   Hash (File_Index, "file_1", State, Digest);
   Assert (Digest, "50989c91ed821899ed777887daa0d3512fa18e3c");
   Assert (Entry_State'Pos (State), Entry_State'Pos (UPDATED_FILE));

   Total_Size := Total_Size - File_1_Length;
   File_1_Length := Integer (St.Length (St.Stat ("file_1")));
   Total_Size := Total_Size + File_1_Length;
   Assert (Integer (Indexed_Content_Size (File_Index)), Total_Size);

   Hash (File_Index, "file_1", State, Digest);
   Assert (Digest, "50989c91ed821899ed777887daa0d3512fa18e3c");
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
