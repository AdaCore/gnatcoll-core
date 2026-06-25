--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with GNATCOLL.OS.Libc;

separate (GNATCOLL.OS.Lock)
procedure Unlock_FD (FD : FS.File_Descriptor) is
   Result : Libc.Libc_Status;
   pragma Unreferenced (Result);
begin
   Result := Libc.Flock (FD, GNATCOLL.OS.Libc.LOCK_UN);
end Unlock_FD;
