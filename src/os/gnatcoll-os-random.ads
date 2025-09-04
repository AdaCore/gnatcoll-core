--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--
--  The unit provices a low-level but portable access to an OS
--  Cryptographically-Secure Pseudo-Random Number Generator (CSPRNG)
--
--  Note that for the general case, it's preferable to use GNATCOLL.Random
--  which provides higher level interface and rely on that unit
with Interfaces.C;

package GNATCOLL.OS.Random is

   procedure Random_Bytes
      (Buffer : System.Address;
       Size   : Interfaces.C.size_t);
   --  Low level interface to the CSPRNG that fill a buffer of size Size with
   --  random data.

end GNATCOLL.OS.Random;
