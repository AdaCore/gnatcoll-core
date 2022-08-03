------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

--  Provide an implementation for an efficient cache of file SHA1 checksums.

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Calendar;
with Ada.Strings.Hash;
with Ada.Strings.UTF_Encoding;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Stat;

package GNATCOLL.File_Indexes is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package Stat renames GNATCOLL.OS.Stat;
   package FSUtil renames GNATCOLL.OS.FSUtil;

   type File_Index is private;
   --  A database tracking SHA1 of a set of files. The caching mechanism
   --  ensures that SHA1 for a given file is recomputed only when the file
   --  changes

   type Entry_State is (UNCHANGED_FILE, UPDATED_FILE, NEW_FILE, REMOVED_FILE);
   --  Indication returned when doing a SHA1 query in File_Index:
   --
   --  UNCHANGED_FILE: returned when SHA1 did not change since last query
   --  UPDATED_FILE: returned when file was in the File_Index but SHA1 has been
   --    recomputed as a file change has been detected
   --  NEW_FILE: returned when SHA1 is computed because file was not present in
   --    the index
   --  REMOVED_FILE: returned whenever a file does not exist anymore.
   --  UNHASHABLE_FILE: returned when a file content hash cannot be computed
   --    (file does not exist, is not a file or content cannot be accessed).

   function Indexed_Content_Size (Self : File_Index) return Long_Long_Integer;
   --  Return the total number of bytes that have been indexed

   procedure Hash
      (Self   : in out File_Index;
       Path   : UTF8.UTF_8_String;
       Attrs  : Stat.File_Attributes;
       State  : out Entry_State;
       Digest : out FSUtil.SHA1_Digest);
   --  Get the hash digest for the file located at Path and with file
   --  attributes Attrs (obtained with a call to GNATCOLL.OS.Stat). See
   --  Entry_State documentation for the meaning of State.
   --  Note that this version of the Hash function is interesting when
   --  iterating on a directory using GNATCOLL.OS.Dir. Indeed the Dir_Entry
   --  already contains the stat information for the given and thus this avoid
   --  calling stat a second time (specially efficient on Windows platform).

   procedure Hash
      (Self   : in out File_Index;
       Path   : UTF8.UTF_8_String;
       State  : out Entry_State;
       Digest : out FSUtil.SHA1_Digest);
   --  Same as previous function except that a call to Stat is done
   --  automatically to get file attributes.

   function Hash (Self : in out File_Index; Path : UTF8.UTF_8_String)
      return FSUtil.SHA1_Digest;
   --  Same as previous function without State as output.

   --  procedure Save_Index (Self : File_Index; Filename : UTF8.UTF_8_String);
   --  Dump a File_Index on disk

   --  function Load_Index (Filename : UTF8.UTF_8_String) return File_Index;
   --  Load a File_Index from disk

   procedure Clear_Cache (Self : in out File_Index);
   --  Clear the index content.

private

   type Index_Element is record
      Attrs        : Stat.File_Attributes;
      Hash_Digest  : FSUtil.SHA1_Digest;
      Trust_Hash   : Boolean;
      Save_On_Disk : Boolean;
   end record;

   package File_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => UTF8.UTF_8_String,
       Element_Type    => Index_Element,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   type File_Index is record
      Last_Update_Time : Ada.Calendar.Time;
      Total_Size       : Long_Long_Integer := 0;
      DB               : File_Maps.Map;
   end record;

end GNATCOLL.File_Indexes;
