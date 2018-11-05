------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with System;

with GNAT.OS_Lib;

--  OS peculiarities abstraction package for Unix systems.

package GNATCOLL.Mmap.System is

   type System_File is record
      Fd     : GNAT.OS_Lib.File_Descriptor;

      Mapped : Boolean;
      --  Whether mapping is requested by the user and available on the system

      Write  : Boolean;
      --  Whether this file can be written to

      Length : File_Size;
      --  Length of the file. Used to know what can be mapped in the file
   end record;

   type System_Mapping is record
      Address : Standard.System.Address;
      Length  : File_Size;
   end record;

   Invalid_System_File    : constant System_File :=
     (GNAT.OS_Lib.Invalid_FD, False, False, 0);
   Invalid_System_Mapping : constant System_Mapping :=
     (Standard.System.Null_Address, 0);

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File;
   --  Open a file for reading and return the corresponding System_File. Raise
   --  a Ada.IO_Exceptions.Name_Error if unsuccessful.

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File;
   --  Likewise for writing to a file

   procedure Close (File : in out System_File);
   --  Close a system file

   function Read_From_Disk
     (File           : System_File;
      Offset, Length : File_Size) return GNAT.Strings.String_Access;
   --  Read a fragment of a file. It is up to the caller to free the result
   --  when done with it.
   --  Doesn't use mmap.

   procedure Write_To_Disk
     (File           : System_File;
      Offset, Length : File_Size;
      Buffer         : GNAT.Strings.String_Access);
   --  Write some content to a fragment of a file

   procedure Create_Mapping
     (File           : System_File;
      Offset, Length : in out File_Size;
      Mutable        : Boolean;
      Mapping        : out System_Mapping;
      Advice         : Use_Advice := Use_Normal);
   --  Create a memory mapping for the given File, for the area starting at
   --  Offset and containing Length bytes. Store it to Mapping.
   --  Note that Offset and Length may be modified according to the system
   --  needs (for boundaries, for instance). The caller must cope with actually
   --  wider mapped areas.

   procedure Dispose_Mapping
     (Mapping : in out System_Mapping);
   --  Unmap a previously-created mapping

   function Get_Page_Size return File_Size;
   --  Return the number of bytes in a system page.

end GNATCOLL.Mmap.System;
