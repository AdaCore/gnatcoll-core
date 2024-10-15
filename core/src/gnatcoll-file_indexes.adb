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

package body GNATCOLL.File_Indexes is

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

      Prev_Cursor    : Cursor := Find (Self.DB, Normalized_Path);
      Prev_Hash      : File_Index_Digest;
      New_Hash       : File_Index_Digest;
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
               State := REMOVED_FILE;
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
            State := UNHASHABLE_FILE;
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

end GNATCOLL.File_Indexes;
