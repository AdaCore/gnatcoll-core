------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

--  This package defines an arbitrary precision integer type based on the
--  underlying GNU Multiple Precision library. The name of the type is
--  "Big_Integer."
--
--  Most Ada operations and operators are supported for type Big_Integer,
--  except that the type is limited private in order to enforce the underlying
--  semantics required by the library implementation. Assignment is performed
--  via the Set routines, and the equality operator is explicitly defined.
--
--  Most of the Ada routines are direct calls to the underlying C routines,
--  except where the Ada semantics differ. The routines all have pragma Inline
--  applied so that no additional overhead is incurred over that of a direct
--  call to the C routine (when the body consists of only such a call).
--
--  Note that using the operators returning Big_Integer objects will result in
--  temporaries, except when used as the initial value in object declarations.
--  These temporaries are automatically initialized when created but will thus
--  incur some overhead. Procedural versions of the arithmetic operators are
--  therefore included, matching those of the underlying GMP library in C
--  (except in name), which avoid temporaries and operate directly on the
--  operands.

with GNATCOLL.GMP.Lib;
with Ada.Finalization;

package GNATCOLL.GMP.Integers is

   pragma Preelaborate;

   type Big_Integer is tagged limited private;
   --  The type is limited because clients should not use predefined
   --  assignment; nor should they use predefined equality. This matches the
   --  semantics of the underlying GMP library in C. For assignment, use the
   --  Set routines. The equality operator is explicitly redefined.

   --  The underlying C version of the GMP requires the user to manually
   --  initialize the arbitrary precision integer objects (i.e., those of type
   --  mpz_t). Likewise, users are expected to clear these objects to reclaim
   --  the memory allocated. Initialization and clearing are performed
   --  automatically in this Ada version.

   Failure : exception;

   --  Assignment

   procedure Set
     (This : out Big_Integer;
      To   : String;
      Base : Int := 10);
   --  Set the value of This from To, a string containing a number expressed in
   --  base Base. White space is allowed in the string and is simply ignored.
   --
   --  The value of Base may vary from 2 to 62. For bases up to 36, case is
   --  ignored; upper-case and lower-case letters have the same value. For
   --  bases 37 to 62, upper-case letter represent the usual 10..35 while
   --  lower-case letter represent 36..61.
   --
   --  Raises Failure if the entire string To is not a valid number in base
   --  Base.

   function Make (This : String;  Base : Int := 10) return Big_Integer;
   --  Constructs a Big_Integer from This, using the same rules as procedure
   --  Set above.

   procedure Set (This : out Big_Integer;  To : Big_Integer);
   --  Set the value of This from To.

   procedure Set (This : out Big_Integer;  To : Long);
   --  Set the value of This from To.

   procedure Set_UL (This : out Big_Integer;  To : Unsigned_Long);
   --  Set the value of This from To.

   pragma Inline (Set);
   pragma Inline (Set_UL);

   --  Relationals

   function "=" (Left : Big_Integer;  Right : Big_Integer) return Boolean;
   function "=" (Left : Big_Integer;  Right : Long)        return Boolean;
   function "=" (Left : Long;         Right : Big_Integer) return Boolean;

   function ">" (Left : Big_Integer;  Right : Big_Integer) return Boolean;
   function ">" (Left : Big_Integer;  Right : Long)        return Boolean;
   function ">" (Left : Long;         Right : Big_Integer) return Boolean;

   function "<" (Left : Big_Integer;  Right : Big_Integer) return Boolean;
   function "<" (Left : Big_Integer;  Right : Long)        return Boolean;
   function "<" (Left : Long;         Right : Big_Integer) return Boolean;

   function ">=" (Left : Big_Integer;  Right : Big_Integer) return Boolean;
   function ">=" (Left : Big_Integer;  Right : Long)        return Boolean;
   function ">=" (Left : Long;         Right : Big_Integer) return Boolean;

   function "<=" (Left : Big_Integer;  Right : Big_Integer) return Boolean;
   function "<=" (Left : Big_Integer;  Right : Long)        return Boolean;
   function "<=" (Left : Long;         Right : Big_Integer) return Boolean;

   pragma Inline ("=");
   pragma Inline (">");
   pragma Inline ("<");
   pragma Inline (">=");
   pragma Inline ("<=");

   --  Addition

   procedure Add (To : in out Big_Integer;  This : Unsigned_Long);
   procedure Add (To : in out Big_Integer;  This : Big_Integer);

   procedure Add (Result : out Big_Integer;  Op1, Op2 : Big_Integer);
   --  Result := Op1 + Op2;
   --  No temporaries required.

   function "+" (Left, Right : Big_Integer)
      return Big_Integer;

   function "+" (Left : Big_Integer; Right : Unsigned_Long)
      return Big_Integer;

   function "+" (Left : Unsigned_Long; Right : Big_Integer)
      return Big_Integer;

   pragma Inline (Add);
   pragma Inline ("+");

   --  Subtraction

   procedure Subtract (From : in out Big_Integer;  This : Unsigned_Long);
   procedure Subtract (From : in out Big_Integer;  This : Big_Integer);

   procedure Subtract (Result : out Big_Integer;  Op1, Op2 : Big_Integer);
   --  Result := Op1 - Op2;
   --  No temporaries required.

   function "-" (Left, Right : Big_Integer)
      return Big_Integer;

   function "-" (Left : Big_Integer;  Right : Unsigned_Long)
      return Big_Integer;

   function "-" (Left : Unsigned_Long; Right : Big_Integer)
      return Big_Integer;

   pragma Inline (Subtract);
   pragma Inline ("-");

   --  Unary

   function "-" (Left : Big_Integer) return Big_Integer;

   procedure Negate (This : in out Big_Integer);

   pragma Inline ("-");
   pragma Inline (Negate);

   --  Multiplication

   procedure Multiply (This : in out Big_Integer;  By : Long);
   procedure Multiply (This : in out Big_Integer;  By : Big_Integer);

   procedure Multiply (Result : out Big_Integer;  Op1, Op2 : Big_Integer);
   --  Result := Op1 * Op2;
   --  No temporaries required.

   function "*" (Left : Big_Integer;  Right : Big_Integer)
      return Big_Integer;

   function "*" (Left : Long;  Right : Big_Integer)
      return Big_Integer;

   function "*" (Left : Big_Integer;  Right : Long)
      return Big_Integer;

   pragma Inline (Multiply);
   pragma Inline ("*");

   --  Division

   procedure Divide (Q : in out Big_Integer;
                     N : Big_Integer;
                     D : Unsigned_Long);

   procedure Divide (Q : in out Big_Integer;
                     N : Big_Integer;
                     D : Big_Integer);

   function "/" (Left, Right : Big_Integer) return Big_Integer;

   function "/" (Left : Big_Integer;  Right : Unsigned_Long)
      return Big_Integer;

   pragma Inline (Divide);
   pragma Inline ("/");

   function "mod" (Left : Big_Integer; Right : Big_Integer)
      return Big_Integer;

   function "mod" (Left : Big_Integer; Right : Long)
      return Big_Integer;

   procedure Get_Mod (Result : out Big_Integer;  N, D : Big_Integer);

   pragma Inline ("mod");
   pragma Inline (Get_Mod);

   function "rem" (Left : Big_Integer; Right : Big_Integer)
      return Big_Integer;

   function "rem" (Left : Big_Integer; Right : Unsigned_Long)
      return Big_Integer;

   procedure Get_Rem (Result : out Big_Integer;  N, D : Big_Integer);

   pragma Inline ("rem");
   pragma Inline (Get_Rem);

   --  Highest Precedence Operators

   function "**"(Left : Big_Integer; Right : Unsigned_Long)
      return Big_Integer;

   procedure Raise_To_N (This : in out Big_Integer; N : Unsigned_Long);

   function "abs" (Left : Big_Integer) return Big_Integer;

   procedure Get_Abs (Result : out Big_Integer;  From : Big_Integer);

   pragma Inline ("**");
   pragma Inline (Raise_To_N);
   pragma Inline ("abs");
   pragma Inline (Get_Abs);

   --  Miscellaneous functionality

   function Image (This : Big_Integer;  Base : Positive := 10) return String;
   --  Returns This as a string of digits in base Base.  The base argument
   --  may vary from 2 to 62 or from -2 to -36.
   --
   --  Does not include a leading blank if This is >= 0.
   --
   --  For Base in the range 2..36, digits and lower-case letters are
   --  used; for -2..-36, digits and upper-case letters are used; for
   --  37..62, digits, upper-case letters, and lower-case letters (in
   --  that significance order) are used.

   function As_mpz_t (This : Big_Integer)
      return access constant GNATCOLL.GMP.Lib.mpz_t;
   --  This function is useful for passing Big_Integer values to routines from
   --  gmplib that do not have an Ada binding defined by this package. In that
   --  case the user will define the binding but will not be able to pass
   --  Big_Integer objects as parameters to their routine. This function
   --  provides the required visibility to the internal mpz_t component of a
   --  Big_Integer object. For example, the user might do the following:
   --
   --    function mpz_probab_prime_p (this : access constant mpz_t; x : Int)
   --       return Int;
   --    pragma Import (C, mpz_probab_prime_p, "__gmpz_probab_prime_p");
   --
   --    N : Big_Integer;
   --    Result : Int;
   --    ...
   --    Result := mpz_probab_prime_p (As_mpz_t (N), 5);

   pragma Inline (As_mpz_t);

   function Sign (This : Big_Integer) return Integer;
   --  Returns +1 if This > 0, 0 if This = 0, and -1 if This < 0.

   pragma Inline (Sign);

private

   type Big_Integer is new Ada.Finalization.Limited_Controlled with
      record
         Value : aliased GNATCOLL.GMP.Lib.mpz_t;
      end record;

   procedure Initialize (This : in out Big_Integer);
   procedure Finalize   (This : in out Big_Integer);

end GNATCOLL.GMP.Integers;
