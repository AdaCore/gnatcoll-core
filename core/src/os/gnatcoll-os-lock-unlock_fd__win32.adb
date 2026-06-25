--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with GNATCOLL.OS.Win32.Files;

separate (GNATCOLL.OS.Lock)
procedure Unlock_FD (FD : FS.File_Descriptor) is
   package Win32 renames GNATCOLL.OS.Win32;
   Result_Pos    : Win32.LONG;
   Result_Status : Integer;
   pragma Unreferenced (Result_Pos, Result_Status);
begin
   --  Seek to the start so the unlocked byte is the same byte that was locked
   Result_Pos := Win32.Files.Lseek (FD, 0, Win32.Files.SEEK_SET);
   Result_Status :=
     Integer
       (Win32.Files.Locking
          (FD => FD, Mode => Win32.Files.LK_UNLCK, Length => 1));
end Unlock_FD;
