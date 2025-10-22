------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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
with GNAT.OS_Lib;
with GNATCOLL.String_Builders;
with GNATCOLL.OS.Libc; use GNATCOLL.OS.Libc;
with GNATCOLL.OS.Libc.Stat; use GNATCOLL.OS.Libc.Stat;

separate (GNATCOLL.OS.Stat)
function Fstat (FD : FS.File_Descriptor) return File_Attributes
is

   package Libc renames GNATCOLL.OS.Libc;

   Stat_Result : Stat_Info;
   Result      : File_Attributes;
   Status      : Libc_Status;
begin

   Status := Libc.Stat.Fstat (FD, Stat_Result);

   Result.Exists := Status = Success;

   if Status = Success then
      Result.Symbolic_Link := (Stat_Result.Mode and S_IFMT) = S_IFLNK;
      Result.Regular       := (Stat_Result.Mode and S_IFMT) = S_IFREG;
      Result.Directory     := (Stat_Result.Mode and S_IFMT) = S_IFDIR;
      Result.Stamp         := Stat_Result.Mtime;
      Result.Length        := Long_Long_Integer (Stat_Result.Size);
   end if;

   return Result;
end Fstat;
