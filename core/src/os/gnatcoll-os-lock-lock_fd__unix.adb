--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

with GNATCOLL.OS.Libc;

separate (GNATCOLL.OS.Lock)
function Lock_FD
  (FD : FS.File_Descriptor; Blocking : Boolean) return Lock_Result
is
   package Libc renames GNATCOLL.OS.Libc;
   use type Libc.Flock_Operation;
   Operation : Libc.Flock_Operation := Libc.LOCK_EX;
   Errno     : Integer;
begin
   if not Blocking then
      Operation := Operation or Libc.LOCK_NB;
   end if;

   --  If a blocking flock is interrupted by a signal (EINTR); loop and retry
   --  until it either takes the lock or fails for a real reason.
   loop
      if Libc.Flock (FD, Operation) = Libc.Success then
         return Acquired;
      end if;
      Errno := Libc.Errno;
      if Errno = Libc.EWOULDBLOCK
        or else (Errno = Libc.EINTR and then not Blocking)
      then
         return Contended;
      elsif Errno /= Libc.EINTR then
         return Failed;
      end if;
   end loop;
end Lock_FD;
