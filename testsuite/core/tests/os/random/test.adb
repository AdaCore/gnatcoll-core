with GNATCOLL.Random;
with GNAT.IO;
with Test_Assert;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Ordered_Sets;
with Interfaces;

function Test return Integer
is
   package IO renames GNAT.IO;
   package Rand renames GNATCOLL.Random;
   package A renames Test_Assert;
   Start_Time, End_Time : Time;

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);

begin
   IO.Put_Line ("Test random integer generation");
   declare
      RI : Integer;
   begin
      Start_Time := Clock;
      for J in 1 .. 1_000_000 loop
         RI := Rand.Random_Integer;
      end loop;
      End_Time := Clock;
      IO.Put_Line ("Random integer sample:" & RI'Img);
      IO.Put ("Time to generate 1_000_000 integers:");
      IO.Put_Line (Duration'Image (End_Time - Start_Time));
   end;

   IO.Put_Line ("Test random integer range generation");
   declare
      RI : Integer;
      Frequence : array (1 .. 62) of Integer := (others => 0);
      Error : Long_Float;
   begin
      Start_Time := Clock;
      for J in 1 .. 6_200_000 loop
         RI := Rand.Random_Integer_Range (First => 1, Last => 62);
         Frequence (RI) := Frequence (RI) + 1;
      end loop;
      End_Time := Clock;
      IO.Put_Line ("Random integer sample:" & RI'Img);
      IO.Put ("Time to generate 6_200_000 integers in range 1 .. 62:");
      IO.Put_Line (Duration'Image (End_Time - Start_Time));

      --  Ideally each number should appears 100_000 times. This is not a real
      --  test for the RNG quality. Rather a sanity check that there is no
      --  obvious bias introduced by a mistake on the algorithm used on top of
      --  the CSRNG.
      for Idx in Frequence'Range loop
         Error := Long_Float (Frequence (Idx) - 100_000) / 100_000.0;
         A.Assert
            (Error < 0.013,
             Idx'Img & " appears " & Frequence (Idx)'Img &
             " (error:" & Long_Float'Image (Error * 100.0) & "%)");
      end loop;
   end;

   IO.Put_Line ("Test bad range exception");
   declare
      I : Integer;
   begin
      I := Rand.Random_Integer_Range (First => 43, Last => 42);
      A.Assert (False, "exception not raised. got integer" & I'Img);
   exception
      when Constraint_Error =>
         A.Assert (True, "right exception raised on bad range");
      when others =>
         A.Assert (False, "wrong exception raised on bad range");
   end;

   IO.Put_Line ("Test random random alphanumerical");
   declare
      S1 : String (1 .. 100_000);
   begin
      Rand.Random_Alphanumerical_String (S1);
      for Idx in S1'Range loop
         if S1 (Idx) not in '0' .. '9' and then
            S1 (Idx) not in 'a' .. 'z' and then
            S1 (Idx) not in 'A' .. 'Z'
         then
            A.Assert
               (False,
                "invalid character returned by " &
                "Random_Alphanumerical_String");
         end if;
      end loop;

      Rand.Random_Alphanumerical_String (S1, Case_Sensitive => False);
      for Idx in S1'Range loop
         if S1 (Idx) not in '0' .. '9' and then
            S1 (Idx) not in 'a' .. 'z'
         then
            A.Assert
               (False,
                "invalid character returned by " &
                "Random_Alphanumerical_String");
         end if;
      end loop;

   end;

   IO.Put_Line ("Unsigned generation");
   declare
      use Interfaces;
      use Rand;
      U32  : constant Unsigned_32 := Random_Unsigned_32;
      U64  : constant Unsigned_64 := Random_Unsigned_64;
      U128 : constant Unsigned_128 := Random_Unsigned_128;
   begin
      A.Assert (U32 /= Random_Unsigned_32);
      A.Assert (U64 /= Random_Unsigned_64);
      A.Assert (U128 /= Random_Unsigned_128);
   end;

   IO.Put_Line ("128 bits ranges");
   declare
      use Interfaces;
      use Rand;
      function U128_Range is new Random_Range (Unsigned_128);
      Result : Unsigned_128;
   begin
      Result := U128_Range;
      Result := U128_Range (First => 42, Last => 42);
      A.Assert (Result = 42);
      for Idx in 1 .. 100_000 loop
         Result := U128_Range (First => 0, Last => 10);
         if Result > 10 then
            A.Assert
               (False, "128 bits number in the wrong range" & Result'Img);
         end if;
      end loop;

      --  Test that that tries to check the case in which some random values
      --  are discarded (see implementation of Random_Range)
      for Idx in 1 .. 100_000 loop
         Result := U128_Range
            (First => Unsigned_128'First, Last => Unsigned_128'Last / 2 + 1);
      end loop;
      A.Assert (Result <= Unsigned_128'Last / 2 + 1);

   end;

   IO.Put_Line ("64 bits ranges");
   declare
      use Interfaces;
      use Rand;
      function U64_Range is new Random_Range (Unsigned_64);
      Result : Unsigned_64;
   begin
      Result := U64_Range;
      Result := U64_Range (First => 42, Last => 42);
      A.Assert (Result = 42);
      for Idx in 1 .. 100_000 loop
         Result := U64_Range (First => 0, Last => 10);
         if Result > 10 then
            A.Assert
               (False, "64 bits number in the wrong range" & Result'Img);
         end if;
      end loop;

      --  Test that that tries to check the case in which some random values
      --  are discarded (see implementation of Random_Range)
      for Idx in 1 .. 100_000 loop
         Result := U64_Range
            (First => Unsigned_64'First, Last => Unsigned_64'Last / 2 + 1);
      end loop;
      A.Assert (Result <= Unsigned_64'Last / 2 + 1);
   end;

   IO.Put_Line ("32 bits ranges");
   declare
      use Interfaces;
      use Rand;
      function U32_Range is new Random_Range (Unsigned_32);
      Result : Unsigned_32;
   begin
      Result := U32_Range;
      Result := U32_Range (First => 42, Last => 42);
      A.Assert (Result = 42);
      for Idx in 1 .. 100_000 loop
         Result := U32_Range (First => 0, Last => 10);
         if Result > 10 then
            A.Assert
               (False, "64 bits number in the wrong range" & Result'Img);
         end if;
      end loop;

      --  Test that that tries to check the case in which some random values
      --  are discarded (see implementation of Random_Range)
      for Idx in 1 .. 100_000 loop
         Result := U32_Range
            (First => Unsigned_32'First, Last => Unsigned_32'Last / 2 + 1);
      end loop;
      A.Assert (Result <= Unsigned_32'Last / 2 + 1);
   end;

   IO.Put_Line ("We should not get duplicates on short set of integers");
   --  The test is just to check obvious errors in the RNG
   declare
      Result : Integer;
      Counts : Int_Sets.Set;
   begin
      for Idx in 1 .. 1_000 loop
         Result := Rand.Random_Integer_Range
            (First => Integer'First, Last => Integer'Last / 2 + 1);
         if Counts.Contains (Result) then
            A.Assert (False, "repeating value not expected: " & Result'Img);
         else
            Counts.Insert (Result);
         end if;
      end loop;
      A.Assert (Result <= Integer'Last / 2 + 1);
   end;

   declare
      Result : Integer;
      Counts : Int_Sets.Set;
   begin
      for Idx in 1 .. 1_000 loop
         Result := Rand.Random_Integer;
         if Counts.Contains (Result) then
            A.Assert
               (False, "repeating integer value not expected: " & Result'Img);
         else
            Counts.Insert (Result);
         end if;
      end loop;
   end;

   IO.Put_Line ("Test for random_array");
   declare
      type Integer_Array is array (Positive range <>) of Integer;
      procedure RA is new Rand.Random_Array (Integer, Integer_Array);

      A : Integer_Array (1 .. 128);
      B : Integer_Array (1 .. 0);
   begin
      RA (A);
      RA (B);
   end;

   return A.Report;
end Test;
