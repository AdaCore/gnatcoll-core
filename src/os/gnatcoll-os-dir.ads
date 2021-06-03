------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  Provides a low-level portable API to iterate on directories content. The
--  main advantage of this implementation over the default one in the GNAT
--  runtime is that it returns both the filename and the "stat" information
--  for each entry in a given directory. On systems such as Windows this
--  reduce significantly the cost of file system operations as the Win32 API
--  can achieve this with one system call (the equivalent to readdir).

with Ada.Strings.UTF_Encoding;
with GNATCOLL.OS.Stat;
with GNATCOLL.OS.Dir_Types;

package GNATCOLL.OS.Dir is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package Stat renames GNATCOLL.OS.Stat;

   type Dir_Handle is private;
   --  Handle on a directory

   type Dir_Entry is private;
   --  Directory entry

   function Open (Path : UTF8.UTF_8_String) return Dir_Handle;
   --  Open a directory at path Path. In case of error OS_Error exception is
   --  raised.

   procedure Close (Handle : Dir_Handle);
   --  Close a directory. In case of error OS_Error is raised.

   function Read (Handle : Dir_Handle) return Dir_Entry;
   --  Read next entry in directory Handle. In case of error OS_Error is
   --  raised. When reaching the end of the entry list a special Dir_Entry E
   --  is returned for which End_Of_Iteration (E) is True.

   function End_Of_Iteration (Self : Dir_Entry) return Boolean;
   --  Return True if the entry represent the end of the iteration. Note that
   --  the entry for which En_Of_Iteration returns True is not a valid entry.
   --  (no name and no attributes are set).

   function Name (Self : Dir_Entry) return UTF8.UTF_8_String;
   --  Return the filename for Self

   function Attributes (Self : Dir_Entry) return Stat.File_Attributes;
   --  Return the file attributes for Self

private

   PATH_MAX : constant Integer := 4096;
   NAME_MAX : constant Integer := 1024;

   type Dir_Handle is record
      Handle    : GNATCOLL.OS.Dir_Types.OS_Dir_Handle;
      Path_Last : Integer;
      Path      : UTF8.UTF_8_String (1 .. PATH_MAX);
   end record;

   type Dir_Entry is record
      Info        : Stat.File_Attributes;
      Name_Last   : Integer;
      Name_Buffer : UTF8.UTF_8_String (1 .. NAME_MAX);
   end record;

end GNATCOLL.OS.Dir;
