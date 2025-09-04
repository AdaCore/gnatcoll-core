with GNATCOLL.OS.Random;
with Interfaces.C;
with Ada.Unchecked_Conversion;

package body GNATCOLL.Random is

   use type Interfaces.C.size_t;

   Alphanumerical_Mapping : constant array (1 .. 62) of Character :=
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
       'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
       'u', 'v', 'w', 'x', 'y', 'z',
       'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
       'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
       'U', 'V', 'W', 'X', 'Y', 'Z');

   ---------------------------
   -- Random_Alphanumerical --
   ---------------------------

   function Random_Alphanumerical
      (Case_Sensitive : Boolean := True) return Character
   is
   begin
      if Case_Sensitive then
         return Alphanumerical_Mapping
            (Random_Integer_Range (First => 1, Last => 62));
      else
         return Alphanumerical_Mapping
            (Random_Integer_Range (First => 1, Last => 36));
      end if;
   end Random_Alphanumerical;

   ----------------------------------
   -- Random_Alphanumerical_String --
   ----------------------------------

   procedure Random_Alphanumerical_String
      (Buffer : out String; Case_Sensitive : Boolean := True)
   is
   begin
      for Idx in Buffer'Range loop
         Buffer (Idx) :=
            Random_Alphanumerical (Case_Sensitive => Case_Sensitive);
      end loop;
   end Random_Alphanumerical_String;

   ------------------
   -- Random_Array --
   ------------------

   procedure Random_Array (Buffer : out Data_Array) is
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      GNATCOLL.OS.Random.Random_Bytes
         (Buffer (Buffer'First)'Address, Buffer'Size / 8);
   end Random_Array;

   --------------------
   -- Random_Integer --
   --------------------

   function Random_Integer return Integer is
      function Internal is new Random_Value (Integer);
   begin
      return Internal;
   end Random_Integer;

   --------------------------
   -- Random_Integer_Range --
   --------------------------

   function Random_Integer_Range
      (First : Integer; Last : Integer) return Integer
   is
      function Internal is new Random_Range (Integer);
   begin
      return Internal (First => First, Last => Last);
   end Random_Integer_Range;

   ------------------
   -- Random_Range --
   ------------------

   function Random_Range
      (First : Data := Data'First; Last : Data := Data'Last) return Data
   is
      use Interfaces;
   begin
      if Last = First then
         --  No need for randomness
         return Last;

      elsif Last < First then
         --  Invalid range
         raise Constraint_Error;

      --  For a valid range the following approch is taken:
      --  1- Find an unsigned integer type that matches the size of Data'Base
      --     (as only the right branch is taken during execution the warning
      --      that may occurs regarding unequal size for Unchecked_Conversion
      --      declarations can be ignored -- pragma Warnings Z and z calls).
      --  2- Generate a random number for that unsigned type and map it to a
      --     given number in the range. Note that in order to avoid having a
      --     bias in the generator, all values in the range are mapped to the
      --     exact same number of values in the unsigned type range. This means
      --     that, depending on the chosen range, some random unsigned values
      --     might be discarded. In theory this might introduce an infinite
      --     loop (in the worst case almost 50% of the values might be
      --     discarded).

      elsif Data'Base'Size > 64 then
         declare
            pragma Warnings ("Z");
            function To_U128 is new Ada.Unchecked_Conversion
               (Data'Base, Unsigned_128);
            function To_Data is new Ada.Unchecked_Conversion
               (Unsigned_128, Data'Base);
            pragma Warnings ("z");

            N : constant Unsigned_128 := To_U128 (Last) - To_U128 (First) + 1;
            --  The length of the requested interval. Since we are working
            --  with unsigned type, and since Last < First, N = 0 can only
            --  occurs if the full 128 bits range is covered.

         begin
            if N = 0 then
               --  Full 128bits range is covered so just returned a converted
               --  random 128bits unsigned.
               return To_Data (To_U128 (First) + Random_Unsigned_128);

            else
               declare
                  X : Unsigned_128 := Random_Unsigned_128;
                  --  Hold generated random 128 bits unsigned integers until
                  --  we find one that maps into the desired range.

                  Last_Valid : constant Unsigned_128 :=
                     Unsigned_128'Last - (Unsigned_128'Last rem N + 1);
                  --  Last valid value for X. if X > Last_Value we discard the
                  --  value in order to not introduce bias.

               begin
                  --  Though potentially there is here a potential infinite
                  --  loop, in practice this does not occur.
                  while X > Last_Valid loop
                     X := Random_Unsigned_128;
                  end loop;

                  return To_Data (To_U128 (First) + X rem N);
               end;
            end if;
         end;

      elsif Data'Base'Size > 32 then
         declare
            pragma Warnings ("Z");
            function To_U64 is
               new Ada.Unchecked_Conversion (Data'Base, Unsigned_64);
            function To_Data is
               new Ada.Unchecked_Conversion (Unsigned_64, Data'Base);
            pragma Warnings ("z");

            N : constant Unsigned_64 := To_U64 (Last) - To_U64 (First) + 1;
            --  The length of the requested interval. Since we are working
            --  with unsigned type, and since Last < First, N = 0 can only
            --  occurs if the full 64  bits range is covered.

         begin
            if N = 0 then
               --  Full 64 bits range is covered so just returned a converted
               --  random 64 bits unsigned.
               return To_Data (To_U64 (First) + Random_Unsigned_64);

            else
               declare
                  X : Unsigned_64 := Random_Unsigned_64;
                  --  Hold generated random 64 bits unsigned intergers

                  Last_Valid : constant Unsigned_64 :=
                     Unsigned_64'Last - (Unsigned_64'Last rem N + 1);
                  --  Last valid value for X. if X > Last_Value we discard the
                  --  value in order to not introduce bias.

               begin
                  --  Though potentially there is here a potential infinite
                  --  loop, in practice this does not occur.
                  while X > Last_Valid loop
                     X := Random_Unsigned_64;
                  end loop;

                  return To_Data (To_U64 (First) + X rem N);
               end;
            end if;
         end;

      --  In the 32-bit case, we need to handle both integer and enumeration
      --  types and, therefore, rely on 'Pos and 'Val in the computation.
      --  In the 32-bit case we can use the Universal Integer type rather
      --  than an intermediate unsigned integer.

      elsif Data'Pos (Last) - Data'Pos (First) = 2 ** 32 - 1 then
         return Data'Val (Data'Pos (First) +
            Unsigned_32'Pos (Random_Unsigned_32));
      else
         declare
            N : constant Unsigned_32 :=
               Unsigned_32 (Data'Pos (Last) - Data'Pos (First) + 1);
            Last_Valid : constant Unsigned_32 :=
               Unsigned_32'Last - (Unsigned_32'Last rem N + 1);
            X : Unsigned_32 := Random_Unsigned_32;

         begin
            --  Though potentially there is here a potential infinite
            --  loop, in practice this does not occur.
            while X > Last_Valid loop
               X := Random_Unsigned_32;
            end loop;

            return Data'Val (Data'Pos (First) + Unsigned_32'Pos (X rem N));
         end;
      end if;
   end Random_Range;

   ------------------------
   -- Random_Unsigned_32 --
   ------------------------

   function Random_Unsigned_32 return Interfaces.Unsigned_32 is
      function Internal is new Random_Value (Interfaces.Unsigned_32);
   begin
      return Internal;
   end Random_Unsigned_32;

   ------------------------
   -- Random_Unsigned_64 --
   ------------------------

   function Random_Unsigned_64 return Interfaces.Unsigned_64 is
      function Internal is new Random_Value (Interfaces.Unsigned_64);
   begin
      return Internal;
   end Random_Unsigned_64;

   -------------------------
   -- Random_Unsigned_128 --
   -------------------------

   function Random_Unsigned_128 return Interfaces.Unsigned_128 is
      function Internal is new Random_Value (Interfaces.Unsigned_128);
   begin
      return Internal;
   end Random_Unsigned_128;

   ------------------
   -- Random_Value --
   ------------------

   function Random_Value return Data is
   begin
      return Result : Data do
         GNATCOLL.OS.Random.Random_Bytes (Result'Address, Result'Size / 8);
      end return;
   end Random_Value;

end GNATCOLL.Random;
