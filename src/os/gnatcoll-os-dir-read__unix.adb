------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021-2022, AdaCore                   --
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

with GNATCOLL.OS.Libc.Dirent;
with GNATCOLL.OS.Libc;
with Interfaces.C;

separate (GNATCOLL.OS.Dir)
function Read
   (Handle : Dir_Handle;
    Follow_Symlinks : Boolean := True)
   return Dir_Entry
is
   package Libc renames GNATCOLL.OS.Libc;
   package Dirent renames GNATCOLL.OS.Libc.Dirent;

   use all type Libc.Uint_64;

   C_Result     : Dirent.Dir_Entry;
   Result       : Dir_Entry;
   Ignore_Entry : Boolean := False;

begin
   loop
      --  Note that set_errno is thread safe
      Libc.Set_Errno (0);

      Dirent.Readdir
         (Dir => Dirent.Dir_Handle (Handle.Handle), Element => C_Result);

      if Libc.Errno = 0 then

         if C_Result.Inode = 0 then
            --  There is no inode associated. This means that the complete
            --  list of directory entries has already been read.
            Result.Name_Last := 0;
            Ignore_Entry := False;
         else
            declare
               Name : constant String := Interfaces.C.To_Ada (C_Result.Name);
            begin
               --  Ignore . and .. entries
               Ignore_Entry := (Name = "." or else Name = "..");

               if not Ignore_Entry then
                  --  Save name and stat info for the file
                  Result.Name_Last := Name'Length;
                  Result.Name_Buffer
                     (Result.Name_Buffer'First ..
                      Result.Name_Buffer'First + Name'Length - 1) := Name;
                  Result.Info := Stat.Stat
                     (Handle.Path (1 .. Handle.Path_Last) &
                      "/" &
                      Result.Name_Buffer
                         (Result.Name_Buffer'First ..
                          Result.Name_Buffer'First + Result.Name_Last - 1),
                      Follow_Symlinks => Follow_Symlinks);
               end if;
            end;
         end if;

      else
         raise OS_Error
         with "error while reading directory (error:" & Libc.Errno'Img & ")";
      end if;

      exit when not Ignore_Entry;

   end loop;

   return Result;
end Read;
