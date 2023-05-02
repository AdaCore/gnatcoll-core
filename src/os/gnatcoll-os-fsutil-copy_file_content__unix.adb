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
with GNATCOLL.Memory;  use GNATCOLL.Memory;

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
      Count            : Long_Long_Integer := Length (Src_File_Attr);
      Nb_Bytes_Written : GNATCOLL.Memory.ssize_t;
      Err_Code         : aliased Integer;
      Count_S          : Send_File_Count;
      Ret              : Boolean           := True;
   begin

      while Count > 0 loop
         --  Count is 64 bits long, even for 32 bits systems. Need to split
         --  the count in several sub count for 32 bits systems.
         Count_S :=
           Send_File_Count
             (Long_Long_Integer'Min
                (Long_Long_Integer (Send_File_Count'Last), Count));

         Nb_Bytes_Written :=
           Send_File (Dst_FD, Src_FD, Count_S, Err_Code'Access);
         if Nb_Bytes_Written = -1 then

            if Err_Code = EINVAL or Err_Code = ENOSYS then
               --  Fallback to Read/write copy.
               Nb_Bytes_Written :=
                 Read_Write_Copy
                   (Dst_FD, Src_FD,
                    ssize_t
                      (Long_Long_Integer'Min
                         (Long_Long_Integer (ssize_t'Last), Count)),
                    Err_Code'Access);
               if Nb_Bytes_Written = -1 then
                  Ret := False;
               end if;
            else
               Ret := False;
            end if;
         end if;

         exit when Ret = False;
         Count := Count - Long_Long_Integer (Nb_Bytes_Written);
      end loop;

      Close (Src_FD);
      Close (Dst_FD);
      return Ret;

   end;
end Copy_File_Content;
