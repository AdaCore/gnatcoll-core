------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

--  Get information on files

with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.UTF_Encoding;

package GNATCOLL.OS.Stat is

   package UTF8 renames Ada.Strings.UTF_Encoding;

   type File_Attributes is private;
   --  Record containing information about a file or directory on the
   --  filesystem

   function Stat
      (Path         : UTF8.UTF_8_String;
       Follow_Symlinks : Boolean := True)
      return File_Attributes;
   --  Retrieve file information about a file located at Path. If
   --  Follow_Symlinks is True then in case the file is a symlink the
   --  returned information is regarding the file which is the target of
   --  the symlink. If False, return the information about the symlink itself.
   --
   --  When passing a relative path, stat does not check that intermediate
   --  directories do exist. For example if file file.txt exists then stat
   --  returns success when calling Stat("non_existing_dir/../file.txt").
   --
   --  In case Path is not a valid UTF-8 string the function behaves as if the
   --  file does not exist.

   function Exists (Self : File_Attributes) return Boolean;
   --  Return True if the file exist on the filesystem

   function Is_File (Self : File_Attributes) return Boolean;
   --  Return True if the file is a regular file

   function Is_Directory (Self : File_Attributes) return Boolean;
   --  Return True if the file is a directory

   function Is_Symbolic_Link (Self : File_Attributes) return Boolean;
   --  Return True if the file is a symbolic link

   function Modification_Time (Self : File_Attributes) return Time;
   --  Return file modification time

   function Image (Self : File_Attributes) return String;
   --  String image of a File_Attributes structure

private

   type File_Attributes is record
      Exists            : Boolean := False;
      Writable          : Boolean := False;
      Readable          : Boolean := False;
      Executable        : Boolean := False;
      Symbolic_Link     : Boolean := False;
      Regular           : Boolean := False;
      Directory         : Boolean := False;
      Stamp             : Time;
      Length            : Long_Long_Integer := 0;
   end record;

   pragma Inline (Exists);
   pragma Inline (Is_File);
   pragma Inline (Is_Directory);
   pragma Inline (Is_Symbolic_Link);
   pragma Inline (Modification_Time);

end GNATCOLL.OS.Stat;
