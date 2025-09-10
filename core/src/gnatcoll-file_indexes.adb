------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                        Copyright (C) 2023-2024, AdaCore                  --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the impied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;
with GNATCOLL.JSON;
with GNATCOLL.OS.FS;
with Interfaces.C;
with Ada.Calendar.Conversions;
with Ada.Strings.Unbounded;

package body GNATCOLL.File_Indexes is

   package JSON renames GNATCOLL.JSON;
   package FS renames GNATCOLL.OS.FS;

   JSON_INDEX_MIMETYPE : constant String := "text/json+file-index-1.0";

   procedure Internal_Hash
      (Self            : in out File_Index;
       Normalized_Path : UTF8.UTF_8_String;
       Attrs           : Stat.File_Attributes;
       State           : out Entry_State;
       Digest          : out File_Index_Digest);

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Self : in out File_Index) is
   begin
      Self.DB.Clear;
      Self.Total_Size := 0;
   end Clear_Cache;

   ----------
   -- Hash --
   ----------

   function Hash
      (Self  : in out File_Index;
       Path  : UTF8.UTF_8_String)
      return File_Index_Digest
   is
      State : Entry_State;
      Digest : File_Index_Digest;
   begin
      Hash (Self => Self, Path => Path, State => State, Digest => Digest);
      return Digest;
   end Hash;

   procedure Hash
      (Self   : in out File_Index;
       Path   : UTF8.UTF_8_String;
       State  : out Entry_State;
       Digest : out File_Index_Digest)
   is
      Normalized_Path : constant String := GNAT.OS_Lib.Normalize_Pathname
         (Path, Resolve_Links => False);
   begin
      Internal_Hash
         (Self, Normalized_Path, Stat.Stat (Normalized_Path), State, Digest);
   end Hash;

   procedure Hash
      (Self       : in out File_Index;
       Path       : UTF8.UTF_8_String;
       Attrs      : Stat.File_Attributes;
       State      : out Entry_State;
       Digest     : out File_Index_Digest)
   is
   begin
      Internal_Hash
         (Self,
          GNAT.OS_Lib.Normalize_Pathname (Path, Resolve_Links => False),
          Attrs,
          State,
          Digest);
   end Hash;

   -------------------
   -- Internal_Hash --
   -------------------

   procedure Internal_Hash
      (Self            : in out File_Index;
       Normalized_Path : UTF8.UTF_8_String;
       Attrs           : Stat.File_Attributes;
       State           : out Entry_State;
       Digest          : out File_Index_Digest)
   is
      use File_Maps;
      use type Stat.File_Attributes;
      use type Ada.Calendar.Time;

      No_Digest      : constant File_Index_Digest := (others => ' ');
      Prev_Cursor    : Cursor := Find (Self.DB, Normalized_Path);
      Prev_Hash      : File_Index_Digest := No_Digest;
      New_Hash       : File_Index_Digest := No_Digest;
      Trust_New_Hash : Boolean := True;
   begin

      if Prev_Cursor /= No_Element then
         declare
            Prev : constant Index_Element := Element (Prev_Cursor);
         begin
            if Prev.Trust_Hash and then Attrs = Prev.Attrs then
               State := UNCHANGED_FILE;
               Digest := Prev.Hash_Digest;
               return;
            end if;

            --  Two possibilities:
            --  - Hash is going to be recomputed
            --  - File does not exist anymore
            --  In both cases, previous file length must be removed
            --  from the total length.

            Self.Total_Size := Self.Total_Size - Stat.Length (Prev.Attrs);

            if not Stat.Exists (Attrs) then
               Delete (Self.DB, Prev_Cursor);
               State  := REMOVED_FILE;
               Digest := No_Digest;
               return;
            end if;

            --  default state is now UPDATED_FILE
            State := UPDATED_FILE;

            --  Keep track of prev hash
            Prev_Hash := Prev.Hash_Digest;
         end;
      else
         --  This is a new file
         State := NEW_FILE;
      end if;

      begin
         --  Compute the new hash
         New_Hash := File_Index_Digest
            (Blake3.Blake3_File_Hash (Path => Normalized_Path));
      exception
         when others =>
            State  := UNHASHABLE_FILE;
            Digest := No_Digest;
            return;
      end;

      --  Some file system do not have a better resolution than 1s for
      --  modification time. If at the time of the query the file has been
      --  modified less than 1s ago, there is a possible race condition in
      --  which the file is modified again in the same second after we updated
      --  the File_Index DB. In those cases don't trust the hash (i.e: always
      --  recompute it in the next query).
      Trust_New_Hash :=
         (Ada.Calendar.Clock - Stat.Modification_Time (Attrs)) > 1.0;

      --  Compute Hash
      Include
         (Self.DB,
          Normalized_Path,
          (Attrs        => Attrs,
           Hash_Digest  => New_Hash,
           Trust_Hash   => Trust_New_Hash,
           Save_On_Disk => True));
      Self.Total_Size := Self.Total_Size + Stat.Length (Attrs);

      --  If the hash has not changed set State to UNCHANGED_FILE

      if State = UPDATED_FILE and then New_Hash = Prev_Hash then
         State := UNCHANGED_FILE;
      end if;

      Digest := New_Hash;
   end Internal_Hash;

   --------------------------
   -- Indexed_Content_Size --
   --------------------------

   function Indexed_Content_Size (Self : File_Index) return Long_Long_Integer
   is
   begin
      return Self.Total_Size;
   end Indexed_Content_Size;

   ----------------
   -- Save_Index --
   ----------------

   procedure Save_Index (Self : File_Index; Filename : UTF8.UTF_8_String)
   is
      use File_Maps;
      Result     : constant JSON.JSON_Value := JSON.Create_Object;
      JSON_DB    : constant JSON.JSON_Value := JSON.Create_Object;
      Result_Str : Ada.Strings.Unbounded.Unbounded_String;
      FD         : FS.File_Descriptor;

      function Create (T : Ada.Calendar.Time) return JSON.JSON_Value;
      --  Serialize a time T into an integer (UNIX time epoch)

      function Create (T : Ada.Calendar.Time) return JSON.JSON_Value is
      begin
         return JSON.Create
            (Long_Long_Integer
               (Ada.Calendar.Conversions.To_Unix_Time (T)));
      end Create;

   begin
      --  The mime field is only used by the Load_Index function and ensure
      --  we don't try to load a file a a distinct format
      Result.Set_Field ("mimetype", JSON_INDEX_MIMETYPE);

      --  Dump global data
      Result.Set_Field ("total_size", JSON.Create (Self.Total_Size));

      --  Iterate over the database
      declare
         C : Cursor := First (Self.DB);
      begin
         while C /= No_Element loop
            --  For each element the following data structure is created:
            --
            --  {
            --    "trust": bool,
            --    "hash": str,
            --    "stat": [...]  # stat information
            --  }
            declare
               El        : constant Index_Element := Element (C);
               JSON_El   : constant JSON.JSON_Value := JSON.Create_Object;
               Stat_Data : constant JSON.JSON_Value := JSON.Create
                  (JSON.Empty_Array);
            begin
               JSON_El.Set_Field ("trust", El.Trust_Hash);
               JSON_El.Set_Field ("hash", El.Hash_Digest);

               Stat_Data.Append (JSON.Create (Stat.Exists (El.Attrs)));
               Stat_Data.Append (JSON.Create (Stat.Is_Writable (El.Attrs)));
               Stat_Data.Append (JSON.Create (Stat.Is_Readable (El.Attrs)));
               Stat_Data.Append (JSON.Create (Stat.Is_Executable (El.Attrs)));
               Stat_Data.Append
                  (JSON.Create (Stat.Is_Symbolic_Link (El.Attrs)));
               Stat_Data.Append (JSON.Create (Stat.Is_File (El.Attrs)));
               Stat_Data.Append (JSON.Create (Stat.Is_Directory (El.Attrs)));
               Stat_Data.Append (Create (Stat.Modification_Time (El.Attrs)));
               Stat_Data.Append (JSON.Create (Stat.Length (El.Attrs)));
               JSON_El.Set_Field ("stat", Stat_Data);

               JSON_DB.Set_Field (Key (C), JSON_El);
            end;
            C := Next (C);
         end loop;
      end;
      Result.Set_Field ("db", JSON_DB);

      --  Write the final JSON
      Result_Str := JSON.Write (Result, Compact => False);
      FD := FS.Open (Filename, Mode => FS.Write_Mode);
      FS.Write_Unbounded (FD, Result_Str);
      FS.Close (FD);
   end Save_Index;

   ----------------
   -- Load_Index --
   ----------------

   function Load_Index (Filename : UTF8.UTF_8_String) return File_Index
   is
      JSON_Result : JSON.Read_Result;
      JSON_Data   : JSON.JSON_Value;
      Result      : File_Index;

      function Get (V : JSON.JSON_Value) return Ada.Calendar.Time;
      --  Transform an integer back to an Ada Time.

      procedure Process_Entry
         (Name : JSON.UTF8_String; Value : JSON.JSON_Value);
      --  Function called on each file entry

      function Get (V : JSON.JSON_Value) return Ada.Calendar.Time
      is
         I : constant Long_Integer := JSON.Get (V);
      begin
         return Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (I));
      end Get;

      procedure Process_Entry
         (Name : JSON.UTF8_String; Value : JSON.JSON_Value)
      is
         V : Index_Element;
         JSON_Stat : JSON.JSON_Array := JSON.Get (Value, "stat");
      begin
         V.Trust_Hash := JSON.Get (Value, "trust");
         V.Hash_Digest := JSON.Get (Value, "hash");
         V.Attrs := Stat.New_File_Attributes
            (Exists        => JSON.Get (JSON.Get (JSON_Stat, 1)),
             Writable      => JSON.Get (JSON.Get (JSON_Stat, 2)),
             Readable      => JSON.Get (JSON.Get (JSON_Stat, 3)),
             Executable    => JSON.Get (JSON.Get (JSON_Stat, 4)),
             Symbolic_Link => JSON.Get (JSON.Get (JSON_Stat, 5)),
             Regular       => JSON.Get (JSON.Get (JSON_Stat, 6)),
             Directory     => JSON.Get (JSON.Get (JSON_Stat, 7)),
             Stamp         => Get (JSON.Get (JSON_Stat, 8)),
             Length        => JSON.Get (JSON.Get (JSON_Stat, 9)));
         Result.DB.Include (Name, V);
      end Process_Entry;

   begin
      JSON_Result := JSON.Read_File (Filename);

      if not JSON_Result.Success then
         --  Silently ignore index errors
         return Result;
      end if;

      JSON_Data := JSON_Result.Value;

      if not JSON.Has_Field (JSON_Data, "mimetype") or else
         not JSON.Has_Field (JSON_Data, "total_size")
      then
         return Result;
      end if;

      declare
         Mimetype : constant String := JSON.Get (JSON_Data, "mimetype");
      begin
         if Mimetype /= JSON_INDEX_MIMETYPE then
            return Result;
         end if;

         Result.Total_Size  :=
            JSON.Get (JSON.Get (JSON_Data, "total_size"));

         JSON.Map_JSON_Object
            (JSON.Get (JSON_Data, "db"), Process_Entry'Unrestricted_Access);
      end;
      return Result;
   end Load_Index;

end GNATCOLL.File_Indexes;
