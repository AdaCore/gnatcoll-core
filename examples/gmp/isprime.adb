-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                      Copyright (C) 2009, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This program allows a user to determine whether a given (big) integer is a
--  prime number. It is based directly on the example provided by the
--  underlying C implementation of the GMP. Note that the input can be
--  expressed in bases other than base 10, if desired, by using C prefixes such
--  as "0x" and so forth.
--
--  This program also illustrates use of an underlying C library routine that
--  does not have a corresponding Ada binding already defined elsewhere.

with Ada.Text_IO;               use Ada.Text_IO;
with GNATCOLL.GMP.Integers;     use GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Integers.IO;  use GNATCOLL.GMP.Integers.IO;
with Interfaces.C;              use Interfaces.C;
with GNATCOLL.GMP.Lib;          use GNATCOLL.GMP.Lib;

procedure IsPrime is

   N : Big_Integer;

   Result : int;

   function mpz_probab_prime_p (this : access constant mpz_t; x : int)
      return int;
   pragma Import (C, mpz_probab_prime_p, "__gmpz_probab_prime_p");

begin
   loop
      Put ("Enter candidate number (return to quit): ");
      declare
         Input : constant String := Get_Line;
      begin
         if Input'Length = 0 then
            return;
         else
            Set (N, Input, 0);  -- base 0 allows the input to define the base
         end if;
      end;

      Result := mpz_probab_prime_p (As_mpz_t (N), 5);

      Put (N);

      case Result is
         when 0 =>
            Put_Line (" is not a prime");
         when 1 =>
            Put_Line (" is a probable prime");
         when 2 =>
            Put_Line (" is a prime");
         when others =>
            Put_Line ("Unexpected result from mpz_probab_prime_p:" &
                      Result'Img);
            return;
      end case;
   end loop;
end IsPrime;
