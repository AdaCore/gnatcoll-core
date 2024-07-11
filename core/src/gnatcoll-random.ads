--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--
--  The unit provides functions to generate random data using the OS CSPRNG
--  This means that this functions are suitable for cryptographic contexts
--  The downside is that that they around one order of magnitud slower than
--  implementation provided in the default Ada runtime.
with Interfaces;

package GNATCOLL.Random is

   generic
      type Data is private;
   function Random_Value return Data;
   --  Fill a value of type Data with random data
   --
   --  Note that memory associated with the result will be filled with random
   --  data. As a consequence the generated value might not be valid if there
   --  are any constraints on the data layout.

   generic
      type Data is (<>);
      type Data_Array is array (Positive range <>) of Data;
   procedure Random_Array (Buffer : out Data_Array);
   --  Fill an array of Data with random Data
   --
   --  Note that memory associated with the result will be filled with random
   --  data. As a consequence the generated value might not be valid if there
   --  are any constraints on the data layout.

   generic
      type Data is (<>);
   function Random_Range
      (First : Data := Data'First; Last : Data := Data'Last)
      return Data;
   --  Generic function for discrete type that return a value in a subrange

   --  Declaration of functions for commonly used types

   function Random_Unsigned_32 return Interfaces.Unsigned_32
   with Inline => True;
   --  Return a random unsigned 32bits integer

   function Random_Unsigned_64 return Interfaces.Unsigned_64
   with Inline => True;
   --  Return a random unsigned 64bits integer

   function Random_Unsigned_128 return Interfaces.Unsigned_128
   with Inline => True;
   --  Return a random unsigned 128bits integer

   function Random_Integer return Integer
   with Inline => True;
   --  Return a random integer

   function Random_Integer_Range
      (First : Integer; Last : Integer) return Integer
   with Inline => True;

   function Random_Alphanumerical (Case_Sensitive : Boolean := True)
      return Character
   with Inline => True;

   procedure Random_Alphanumerical_String
      (Buffer : out String; Case_Sensitive : Boolean := True);
   --  Return a random alpha-numerical string

end GNATCOLL.Random;
