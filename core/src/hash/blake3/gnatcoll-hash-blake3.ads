------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
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

--  This is a binding to the BLAKE3 hash algorithm.

with Ada.Strings.UTF_Encoding;
with Ada.Containers;
with GNATCOLL.OS.FS;

package GNATCOLL.Hash.Blake3 is

   package FS renames GNATCOLL.OS.FS;
   package UTF8 renames Ada.Strings.UTF_Encoding;

   type Blake3_Context is tagged limited private;
   --  Object that hold a blake3 context.

   subtype Blake3_Digest is String (1 .. 64);
   --  Digest message.

   procedure Init_Hash_Context (Self : in out Blake3_Context'Class);
   --  Initialize a Blake3 state.

   procedure Update_Hash_Context
      (Self : in out Blake3_Context'Class; Buffer : String);
   --  Add data to the context.

   function Hash_Digest
      (Self : in out Blake3_Context'Class)
      return Blake3_Digest;
   --  Return final digest as a string

   function Blake3 (Str : String) return Ada.Containers.Hash_Type;
   --  Suitable hash function for Ada.Containers.

   function Blake3_File_Hash
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return Blake3_Digest;
   --  Compute the Blake3 of the content of the file located at Path. When
   --  reading the file the function used a finite size Buffer. In most cases
   --  the default buffer size is the one that provide the most performant
   --  way to scan the file.
   --  Blake3_Digest is a fixed sized String. It can be used with both String
   --  and UTF_8_String.

private

   type Blake3_Context is tagged limited record
      Opaque : String (1 .. 2048);
   end record;

end GNATCOLL.Hash.Blake3;
