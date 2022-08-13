------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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

package body GNATCOLL.OS.Stat is

   function Stat
      (Path            : UTF8.UTF_8_String;
       Follow_Symlinks : Boolean := True)
      return File_Attributes is separate;

   ------------
   -- Exists --
   ------------

   function Exists (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Exists;
   end Exists;

   -----------
   -- Image --
   -----------

   function Image (Self : File_Attributes) return String
   is
   begin
      return "exists: " & Self.Exists'Img &
        ", writable: " & Self.Writable'Img &
        ", readable: " & Self.Readable'Img &
        ", executable: " & Self.Executable'Img &
        ", regular: " & Self.Regular'Img &
        ", directory: " & Self.Directory'Img &
        ", link: " & Self.Symbolic_Link'Img;
   end Image;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Directory;
   end Is_Directory;

   -------------------
   -- Is_Executable --
   -------------------

   function Is_Executable (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Executable;
   end Is_Executable;

   ------------------------
   -- Is_Executable_File --
   ------------------------

   function Is_Executable_File (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Regular and then Self.Executable;
   end Is_Executable_File;

   -------------
   -- Is_File --
   -------------

   function Is_File (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Regular;
   end Is_File;

   -----------------
   -- Is_Readable --
   -----------------

   function Is_Readable (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Readable;
   end Is_Readable;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Symbolic_Link;
   end Is_Symbolic_Link;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (Self : File_Attributes) return Boolean
   is
   begin
      return Self.Writable;
   end Is_Writable;

   ------------
   -- Length --
   ------------

   function Length (Self : File_Attributes) return Long_Long_Integer
   is
   begin
      return Self.Length;
   end Length;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time (Self : File_Attributes) return Time
   is
   begin
      return Self.Stamp;
   end Modification_Time;

   -------------------------
   -- New_File_Attributes --
   -------------------------

   function New_File_Attributes
      (Exists        : Boolean;
       Writable      : Boolean;
       Readable      : Boolean;
       Executable    : Boolean;
       Symbolic_Link : Boolean;
       Regular       : Boolean;
       Directory     : Boolean;
       Stamp         : Time;
       Length        : Long_Long_Integer)
      return File_Attributes
   is
   begin
      return (Exists        => Exists,
              Writable      => Writable,
              Readable      => Readable,
              Executable    => Executable,
              Symbolic_Link => Symbolic_Link,
              Regular       => Regular,
              Directory     => Directory,
              Stamp         => Stamp,
              Length        => Length);
   end New_File_Attributes;

end GNATCOLL.OS.Stat;
