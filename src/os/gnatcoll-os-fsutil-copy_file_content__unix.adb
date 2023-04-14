------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with GNATCOLL.OS.Libc; use GNATCOLL.OS.Libc;
with GNATCOLL.OS.FS;   use GNATCOLL.OS.FS;

separate (GNATCOLL.OS.FSUtil)
function Copy_File_Content
  (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean
is

   Src_File_Attr : File_Attributes;
   Src_FD        : File_Descriptor;
   Dst_FD        : File_Descriptor;

begin
   Src_File_Attr := GNATCOLL.OS.Stat.Stat (Src);

   Src_FD := Open (Src, Read_Mode);
   if Src_FD = Invalid_FD then
      return False;
   end if;

   Dst_FD := Open (Dst, Write_Mode);
   if Dst_FD = Invalid_FD then
      Close (Src_FD);
      return False;
   end if;

   declare
      type Ref_UInt_64 is access Uint_64;
      Count            : constant Uint_64 := Uint_64 (Length (Src_File_Attr));
      Nb_Bytes_Written : Sint_64;
      Offset           : constant Ref_UInt_64 := new Uint_64'(0);
   begin
      Nb_Bytes_Written := Send_File (Dst_FD, Src_FD, Offset, Count);

      Close (Src_FD);
      Close (Dst_FD);

      --  This cast may raise a constraint error is count is superior to
      --  Sint_64 maximum value. Nevertheless, we do not handle it, as this
      --  would be too costly compared to the number of occurences.
      if Nb_Bytes_Written /= Sint_64 (Count) then
         return False;
      end if;
   end;

   return True;
end Copy_File_Content;
