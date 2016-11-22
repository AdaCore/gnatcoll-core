------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
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

with Interfaces.C.Strings;

package body GNATCOLL.GMP.Integers is

   use GNATCOLL.GMP.Lib;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Big_Integer) is
   begin
      mpz_init (This.Value'Access);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Big_Integer) is
   begin
      mpz_clear (This.Value'Access);
   end Finalize;

   ---------
   -- Set --
   ---------

   procedure Set
     (This : out Big_Integer;
      To   : String;
      Base : Int := 10)
   is
      use Interfaces.C.Strings;

      Result : Int;
      Input  : chars_ptr := New_String (To);
   begin
      Result := mpz_set_str (This.Value'Access, Input, Base);
      Free (Input);
      if Result /= 0 then
         raise Failure;
      end if;
   end Set;

   ----------
   -- Make --
   ----------

   function Make (This : String;  Base : Int := 10) return Big_Integer is
   begin
      return Result : Big_Integer do
         Set (Result, This, Base);
      end return;
   end Make;

   ---------
   -- Set --
   ---------

   procedure Set (This : out Big_Integer;  To : Big_Integer) is
   begin
      mpz_set (This.Value'Access, To.Value'Access);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This : out Big_Integer;  To : Long) is
   begin
      mpz_set_si (This.Value'Access, To);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set_UL (This : out Big_Integer;  To : Unsigned_Long) is
   begin
      mpz_set_ui (This.Value'Access, To);
   end Set_UL;

   ---------
   -- "=" --
   ---------

   function "=" (Left : Big_Integer;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp (Left.Value'Access, Right.Value'Access) = 0;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Big_Integer;  Right : Long)
      return Boolean
   is
   begin
      return mpz_cmp_si (Left.Value'Access, Right) = 0;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Long;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp_si (Right.Value'Access, Left) = 0;
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left : Big_Integer;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp (Left.Value'Access, Right.Value'Access) > 0;
   end ">";

   ---------
   -- ">" --
   ---------

   function ">" (Left : Big_Integer;  Right : Long)
      return Boolean
   is
   begin
      return mpz_cmp_si (Left.Value'Access, Right) > 0;
   end ">";

   ---------
   -- ">" --
   ---------

   function ">" (Left : Long;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp_si (Right.Value'Access, Left) <= 0;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left : Big_Integer;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp (Left.Value'Access, Right.Value'Access) >= 0;
   end ">=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left : Big_Integer;  Right : Long)
      return Boolean
   is
   begin
      return mpz_cmp_si (Left.Value'Access, Right) >= 0;
   end ">=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left : Long; Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp_si (Right.Value'Access, Left) < 0;
   end ">=";

   ---------
   -- "<" --
   ---------

   function "<" (Left : Big_Integer;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp (Left.Value'Access, Right.Value'Access) < 0;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<" (Left : Big_Integer;  Right : Long)
      return Boolean
   is
   begin
      return mpz_cmp_si (Left.Value'Access, Right) < 0;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<" (Left : Long;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp_si (Right.Value'Access, Left) >= 0;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Big_Integer;  Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp (Left.Value'Access, Right.Value'Access) <= 0;
   end "<=";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Big_Integer;  Right : Long)
      return Boolean
   is
   begin
      return mpz_cmp_si (Left.Value'Access, Right) <= 0;
   end "<=";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Long; Right : Big_Integer)
      return Boolean
   is
   begin
      return mpz_cmp_si (Right.Value'Access, Left) > 0;
   end "<=";

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Big_Integer;  This : Unsigned_Long) is
   begin
      mpz_add_ui (To.Value'Access, To.Value'Access, This);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Big_Integer;  This : Big_Integer) is
   begin
      mpz_add (To.Value'Access, To.Value'Access, This.Value'Access);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Result : out Big_Integer;  Op1, Op2 : Big_Integer) is
   begin
      mpz_add (Result.Value'Access, Op1.Value'Access, Op2.Value'Access);
   end Add;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_add (Result.Value'Access, Left.Value'Access, Right.Value'Access);
      end return;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Big_Integer; Right : Unsigned_Long)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_add_ui (Result.Value'Access,
                     Left.Value'Access,
                     Right);
      end return;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Unsigned_Long; Right : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_add_ui (Result.Value'Access,
                     Right.Value'Access,
                     Left);
      end return;
   end "+";

   --------------
   -- Subtract --
   --------------

   procedure Subtract (From : in out Big_Integer;  This : Unsigned_Long) is
   begin
      mpz_sub_ui (From.Value'Access, From.Value'Access, This);
   end Subtract;

   --------------
   -- Subtract --
   --------------

   procedure Subtract (From : in out Big_Integer;  This : Big_Integer) is
   begin
      mpz_sub (From.Value'Access, From.Value'Access, This.Value'Access);
   end Subtract;

   --------------
   -- Subtract --
   --------------

   procedure Subtract (Result : out Big_Integer; Op1, Op2 : Big_Integer) is
   begin
      mpz_sub (Result.Value'Access, Op1.Value'Access, Op2.Value'Access);
   end Subtract;

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_sub (Result.Value'Access, Left.Value'Access, Right.Value'Access);
      end return;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Big_Integer; Right : Unsigned_Long)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_add_ui (Result.Value'Access,
                     Left.Value'Access,
                     Right);
      end return;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Unsigned_Long; Right : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_add_ui (Result.Value'Access,
                     Right.Value'Access,
                     Left);
      end return;
   end "-";

   --------------
   -- Multiply --
   --------------

   procedure Multiply (This : in out Big_Integer;  By : Long) is
   begin
      mpz_mul_si (This.Value'Access, This.Value'Access, By);
   end Multiply;

   --------------
   -- Multiply --
   --------------

   procedure Multiply (This : in out Big_Integer;  By : Big_Integer) is
   begin
      mpz_mul (This.Value'Access, This.Value'Access, By.Value'Access);
   end Multiply;

   --------------
   -- Multiply --
   --------------

   procedure Multiply (Result : out Big_Integer;  Op1, Op2 : Big_Integer) is
   begin
      mpz_mul (Result.Value'Access, Op1.Value'Access, Op2.Value'Access);
   end Multiply;

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_mul (Result.Value'Access, Left.Value'Access, Right.Value'Access);
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Long; Right : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_mul_si (Result.Value'Access, Right.Value'Access, Left);
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Big_Integer;  Right : Long)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_mul_si (Result.Value'Access, Left.Value'Access, Right);
      end return;
   end "*";

   ------------
   -- Divide --
   ------------

   procedure Divide (Q : in out Big_Integer;
                     N : Big_Integer;
                     D : Unsigned_Long)
   is
      Dummy : Long;
      pragma Unreferenced (Dummy);
   begin
      if D = 0 then
         raise Constraint_Error;
      end if;
      Dummy := mpz_tdiv_q_ui (Q.Value'Access, N.Value'Access, D);
   end Divide;

   ------------
   -- Divide --
   ------------

   procedure Divide (Q : in out Big_Integer;
                     N : Big_Integer;
                     D : Big_Integer)
   is
   begin
      if mpz_cmp_ui (D.Value'Access, 0) = 0 then
         raise Constraint_Error;
      end if;
      mpz_tdiv_q (Q.Value'Access, N.Value'Access, D.Value'Access);
   end Divide;

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Big_Integer)
      return Big_Integer
   is
   begin
      if mpz_cmp_ui (Right.Value'Access, 0) = 0 then
         raise Constraint_Error;
      end if;
      return Result : Big_Integer do
         mpz_tdiv_q (Q => Result.Value'Access,
                     N => Left.Value'Access,
                     D => Right.Value'Access);
      end return;
   end "/";

   ---------
   -- "/" --
   ---------

   function "/" (Left : Big_Integer;  Right : Unsigned_Long)
      return Big_Integer
   is
      Dummy : Long;
      pragma Unreferenced (Dummy);
   begin
      if Right = 0 then
         raise Constraint_Error;
      end if;
      return Result : Big_Integer do
         Dummy := mpz_tdiv_q_ui (Q => Result.Value'Access,
                                 N => Left.Value'Access,
                                 D => Right);
      end return;
   end "/";

   -----------
   -- "rem" --
   -----------

   function "rem" (Left : Big_Integer;  Right : Big_Integer)
      return Big_Integer
   is
   begin
      if mpz_cmp_ui (Right.Value'Access, 0) = 0 then
         raise Constraint_Error;
      end if;
      return Result : Big_Integer do
         mpz_tdiv_r (R => Result.Value'Access,
                     N => Left.Value'Access,
                     D => Right.Value'Access);
         --  the result takes the sign of N, as required by the RM
      end return;
   end "rem";

   -----------
   -- "rem" --
   -----------

   function "rem" (Left : Big_Integer;  Right : Unsigned_Long)
      return Big_Integer
   is
      Dummy : Long;
      pragma Unreferenced (Dummy);
   begin
      if Right = 0 then
         raise Constraint_Error;
      end if;
      return Result : Big_Integer do
         Dummy := mpz_tdiv_r_ui (R => Result.Value'Access,
                                 N => Left.Value'Access,
                                 D => Right);
         --  the result is always non-negative so we have to set the sign to
         --  that of Left
         if Sign (Left) /= Sign (Result) then
            Negate (Result);
         end if;
      end return;
   end "rem";

   -------------
   -- Get_Rem --
   -------------

   procedure Get_Rem (Result : out Big_Integer;  N, D : Big_Integer) is
   begin
      if mpz_cmp_ui (D.Value'Access, 0) = 0 then
         raise Constraint_Error;
      end if;
      mpz_tdiv_r (Result.Value'Access, N.Value'Access, D.Value'Access);
      --  the result takes the sign of N, as required by the RM
   end Get_Rem;

   ---------
   -- "-" --
   ---------

   function "-" (Left : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_neg (Result.Value'Access, Left.Value'Access);
      end return;
   end "-";

   ------------
   -- Negate --
   ------------

   procedure Negate (This : in out Big_Integer) is
   begin
      mpz_neg (This.Value'Access, This.Value'Access);
   end Negate;

   ----------
   -- "**" --
   ----------

   function "**"(Left : Big_Integer; Right : Unsigned_Long)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_pow_ui (Result.Value'Access, Left.Value'Access, Right);
      end return;
   end "**";

   ----------------
   -- Raise_To_N --
   ----------------

   procedure Raise_To_N (This : in out Big_Integer; N : Unsigned_Long) is
   begin
      mpz_pow_ui (This.Value'Access, This.Value'Access, N);
   end Raise_To_N;

   -----------
   -- "abs" --
   -----------

   function "abs" (Left : Big_Integer)
      return Big_Integer
   is
   begin
      return Result : Big_Integer do
         mpz_abs (Result.Value'Access, Left.Value'Access);
      end return;
   end "abs";

   -------------
   -- Get_Abs --
   -------------

   procedure Get_Abs (Result : out Big_Integer;  From : Big_Integer) is
   begin
      mpz_abs (Result.Value'Access, From.Value'Access);
   end Get_Abs;

   -----------
   -- "mod" --
   -----------

   function "mod" (Left : Big_Integer;  Right : Big_Integer)
     return Big_Integer
   is
   begin
      if mpz_cmp_ui (Right.Value'Access, 0) = 0 then
         raise Constraint_Error;
      end if;
      return Result : Big_Integer do
         Get_Mod (Result, Left, Right);
      end return;
   end "mod";

   -----------
   -- "mod" --
   -----------

   function "mod" (Left : Big_Integer;  Right : Long)
      return Big_Integer
   is
   begin
      if Right = 0 then
         raise Constraint_Error;
      end if;
      return Result : Big_Integer do
         declare
            Temp_Right : Big_Integer;
         begin
            Set (Temp_Right, To => Right);
            Get_Mod (Result, Left, Temp_Right);
         end;
      end return;
   end "mod";

   -------------
   -- Get_Mod --
   -------------

   procedure Get_Mod (Result : out Big_Integer;  N, D : Big_Integer) is
   begin
      if mpz_cmp_ui (D.Value'Access, 0) = 0 then
         raise Constraint_Error;
      end if;
      if Sign (N) /= -1 and Sign (D) /= -1 then  -- neither is negative
         mpz_mod (Result.Value'Access, N.Value'Access, D.Value'Access);
      else
         --  The GMP library provides operators defined by C semantics, but the
         --  semantics of Ada's mod operator are not the same as C's when
         --  negative values are involved. We do the following to implement the
         --  required Ada semantics.
         declare
            Temp_Left   : Big_Integer;
            Temp_Right  : Big_Integer;
            Temp_Result : Big_Integer;
         begin
            Set (Temp_Left,  To => N);
            Set (Temp_Right, To => D);

            if Sign (N) = -1 then -- N is negative
               Negate (Temp_Left);
            end if;
            if Sign (D) = -1 then  -- D is negative
               Negate (Temp_Right);
            end if;
            --  now both Temp_Left and Temp_Right are nonnegative

            mpz_mod (Temp_Result.Value'Access,
                     Temp_Left.Value'Access,
                     Temp_Right.Value'Access);

            if mpz_cmp_ui (Temp_Result.Value'Access, 0) = 0 then
               --  if Temp_Result is zero we are done
               Set (Result, To => Temp_Result);
            else
               if Sign (N) = -1 then -- N is negative
                  if Sign (D) = -1 then -- D is negative too
                     Set (Result, To => Temp_Result);
                     Negate (Result);
                  else -- N is negative but D is not
                     Set (Result, Temp_Right - Temp_Result);
                  end if;
               else  -- N is not negative
                  if Sign (D) = -1 then  -- D is negative
                     --  Set (Result, Temp_Result - Temp_Right);
                     mpz_sub (Result.Value'Access,
                              Temp_Result.Value'Access,
                              Temp_Right.Value'Access);
                  else -- neither is negative
                     Set (Result, To => Temp_Result);
                  end if;
               end if;
            end if;
         end;
      end if;
   end Get_Mod;

   -----------
   -- Image --
   -----------

   function Image (This : Big_Integer; Base : Integer := 10) return String is
      use Interfaces.C, Interfaces.C.Strings;

      Number_Digits : constant size_t := mpz_sizeinbase
         (This.Value'Access, Int (abs Base));

      Buffer : String (1 .. Integer (Number_Digits) + 2);
      --  The correct number to allocate is 2 more than Number_Digits in order
      --  to handle a possible minus sign and the null-terminator.

      Result : chars_ptr;
   begin
      Result := mpz_get_str (Buffer'Address, Int (Base), This.Value'Access);
      return Value (Result);
   end Image;

   --------------
   -- As_mpz_t --
   --------------

   function As_mpz_t (This : Big_Integer)
      return access constant GNATCOLL.GMP.Lib.mpz_t
   is
   begin
      return This.Value'Unchecked_Access;
   end As_mpz_t;

   ----------
   -- Sign --
   ----------

   function Sign (This : Big_Integer)
      return Integer
   is
   begin
      return Integer (mpz_sgn (This.Value'Access));
   end Sign;

end GNATCOLL.GMP.Integers;
