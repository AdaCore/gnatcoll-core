--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--
with GNATCOLL.OS.FS;
with GNAT.Task_Lock;

package body GNATCOLL.OS.Random is

   package FS renames GNATCOLL.OS.FS;

   use type FS.File_Descriptor;
   use type Interfaces.C.size_t;

   Urandom_FD : FS.File_Descriptor := FS.Invalid_FD;
   --  A file descriptor on "/dev/urandom". The file descriptor is shared by
   --  all Ada tasks.

   ------------------
   -- Random_Bytes --
   ------------------

   procedure Random_Bytes
      (Buffer : System.Address;
       Size   : Interfaces.C.size_t)
   is
      Read_Bytes : Integer;
   begin
      --  Safely open one non-inherited handle to /dev/urandom.
      if Urandom_FD = FS.Invalid_FD then
         GNAT.Task_Lock.Lock;
         if Urandom_FD = FS.Invalid_FD then
            --  FS.Open does not raise exception in case of error so no need
            --  to catch exceptions and call unlock.
            Urandom_FD := FS.Open ("/dev/urandom");
         end if;
         GNAT.Task_Lock.Unlock;
      end if;

      Read_Bytes := FS.Unsafe_Read (Urandom_FD, Buffer, Size);
      if Read_Bytes /= Integer (Size) then
         raise OS_Error with "error while reading data from /dev/urandom";
      end if;
   end Random_Bytes;

end GNATCOLL.OS.Random;
