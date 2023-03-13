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

with GNATCOLL.OS.Stat;
with GNATCOLL.OS.Win32;         use GNATCOLL.OS.Win32;
with GNATCOLL.OS.Win32.Files;   use GNATCOLL.OS.Win32.Files;
with GNATCOLL.OS.Win32.Process; use GNATCOLL.OS.Win32.Process;
with GNATCOLL.WString_Builders;

separate (GNATCOLL.OS.FSUtil)
function Copy_Timestamps
  (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean
is
   package SB renames GNATCOLL.WString_Builders;
   package Win32 renames GNATCOLL.OS.Win32;

   use all type SB.Static_WString_Builder;

   Src_C_Path : SB.Static_WString_Builder (Src'Length + 1);
   Dst_C_Path : SB.Static_WString_Builder (Dst'Length + 1);

   File_Handle    : HANDLE;
   Last_Access_FT : FILETIME;
   Last_Write_FT  : FILETIME;

   --  Ensure that Creation_FT fields are set to 0, as we do not
   --  want to modify the creation timestamp
   Creation_FT : FILETIME := (DwLowDateTime => 0, DwHighDateTime => 0);
begin

   Append (Src_C_Path, Src);
   Append (Dst_C_Path, Dst);

   File_Handle :=
     CreateFile
       (Filename => As_C_WString (Src_C_Path), DesiredAccess => GENERIC_READ,
        ShareMode           => CF_FILE_SHARE_NONE,
        CreationDisposition => CF_OPEN_EXISTING,
        FlagsAndAttributes  =>
          CF_FILE_ATTRIBUTE_NORMAL or CF_FILE_FLAG_BACKUP_SEMANTICS);

   if File_Handle = INVALID_HANDLE_VALUE then
      return False;
   end if;

   declare
      --  Obtain source file timestamps

      Success : constant Win32.BOOL :=
        GetFileTime
          (File_Handle, Creation_Time => Creation_FT,
           Last_Access_Time           => Last_Access_FT,
           Last_Write_Time            => Last_Write_FT);
   begin
      if CloseHandle (File_Handle) = BOOL_FALSE then
         return False;
      end if;

      if Success = BOOL_FALSE then
         return False;
      end if;
   end;

   --  Copy timestamps to destination file

   File_Handle :=
     CreateFile
       (Filename => As_C_WString (Dst_C_Path), DesiredAccess => GENERIC_WRITE,
        ShareMode           => CF_FILE_SHARE_NONE,
        CreationDisposition => CF_OPEN_EXISTING,
        FlagsAndAttributes  =>
          CF_FILE_ATTRIBUTE_NORMAL or CF_FILE_FLAG_BACKUP_SEMANTICS);

   Creation_FT := (DwLowDateTime => 0, DwHighDateTime => 0);

   declare
      Success : constant Win32.BOOL :=
        SetFileTime
          (File_Handle, Creation_Time => Creation_FT,
           Last_Access_Time           => Last_Access_FT,
           Last_Write_Time            => Last_Write_FT);
   begin
      if CloseHandle (File_Handle) = BOOL_FALSE then
         return False;
      end if;

      if Success = BOOL_FALSE then
         return False;
      end if;
   end;

   return True;
end Copy_Timestamps;
