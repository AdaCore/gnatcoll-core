--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with GNATCOLL.OS.Win32.Files;

separate (GNATCOLL.OS.Lock)
function Lock_FD
  (FD : FS.File_Descriptor; Blocking : Boolean) return Lock_Result
is
   package Win32 renames GNATCOLL.OS.Win32;
   Ignore : Win32.LONG;
   pragma Unreferenced (Blocking, Ignore);
begin
   --  _locking takes the byte range starting at the descriptor's current file
   --  position, so seek to the start first. This makes every contender lock
   --  the same byte (and lets Unlock release exactly that byte) regardless of
   --  where the descriptor happens to be positioned. It's possible to lock
   --  bytes past end of file, so a single byte at offset 0 works even when the
   --  lock file is empty.
   Ignore := Win32.Files.Lseek (FD, 0, Win32.Files.SEEK_SET);
   if Integer
        (Win32.Files.Locking
           (FD => FD, Mode => Win32.Files.LK_NBLCK, Length => 1))
     = 0
   then
      return Acquired;
   elsif Integer (Win32.Files.Errno) = Win32.Files.EACCES then
      return Contended;
   else
      return Failed;
   end if;
end Lock_FD;
