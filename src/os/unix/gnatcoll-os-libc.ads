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
with Interfaces.C; use Interfaces.C;
--  with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package GNATCOLL.OS.Libc is

   subtype Uint_64 is unsigned_long_long;
   subtype Uint_32 is unsigned;
   subtype Sint_64 is long_long;
   subtype Sint_32 is int;

   type Uint is mod 2 ** Standard'Address_Size;

   --  Status returned by most libc functions.
   subtype Libc_Status is Sint_32 range -1 .. 0;
   Success : constant Libc_Status := 0;
   Error : constant Libc_Status := -1;

   --  File modes
   type File_Mode is new Uint_32;

   S_IRWXU  : constant File_Mode := 8#0700#;
   S_IRUSR  : constant File_Mode := 8#0400#;
   S_IWUSR  : constant File_Mode := 8#0200#;
   S_IXUSR  : constant File_Mode := 8#0100#;

   S_IRWXG  : constant File_Mode := 8#0070#;
   S_IRGRP  : constant File_Mode := 8#0040#;
   S_IWGRP  : constant File_Mode := 8#0020#;
   S_IXGRP  : constant File_Mode := 8#0010#;

   S_IRWXO  : constant File_Mode := 8#0007#;
   S_IROTH  : constant File_Mode := 8#0004#;
   S_IWOTH  : constant File_Mode := 8#0002#;
   S_IXOTH  : constant File_Mode := 8#0001#;

   S_IFDIR  : constant File_Mode := 8#040000#;
   S_IFCHR  : constant File_Mode := 8#020000#;
   S_IFBLK  : constant File_Mode := 8#060000#;
   S_IFREG  : constant File_Mode := 8#100000#;
   S_IFIFO  : constant File_Mode := 8#010000#;
   S_IFLNK  : constant File_Mode := 8#120000#;
   S_IFSOCK : constant File_Mode := 8#140000#;
   S_IFMT   : constant File_Mode := 8#170000#;

end GNATCOLL.OS.Libc;
