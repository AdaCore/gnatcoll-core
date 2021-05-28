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

--  Binding to POSIX directory interface (usually implemented in dirent.h)
with System;

package GNATCOLL.OS.Libc.Dirent is

   type Dir_Handle is new System.Address;

   Invalid_Handle : constant Dir_Handle := Dir_Handle (System.Null_Address);

   type Dir_Entry is record
      Inode     : Uint_64;
      Offset    : Uint_64;
      Reclen    : Uint_32;
      File_Type : Uint_8;
      Name      : Interfaces.C.char_array (0 .. 1023);
   end record;

   function Opendir (Path : C_String) return Dir_Handle
   with Import        => True,
        Convention    => C,
        External_Name => "opendir";

   function Closedir (Dir : Dir_Handle) return Libc_Status
   with Import        => True,
        Convention    => C,
        External_Name => "closedir";

   procedure Readdir (Dir : Dir_Handle; Element : in out Dir_Entry)
   with Import        => True,
        Convention    => C,
        External_Name => "__gnatcoll_readdir";

end GNATCOLL.OS.Libc.Dirent;
