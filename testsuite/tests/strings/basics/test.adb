with Ada.Command_Line;          use Ada.Command_Line;
with GNATCOLL.Asserts;
with GNATCOLL.Strings;
with GNATCOLL.Strings_Impl;
with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Text_IO;                  use Ada.Text_IO;
with GNAT.Source_Info;
with GNAT.Strings;              use GNAT.Strings;
with Memory;

pragma Warnings (Off);
with System.Memory;
pragma Warnings (On);

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   generic
      with package Strings is new GNATCOLL.Strings_Impl.Strings (<>);
      with function Image (S : Strings.Char_String) return String is <>;
      First_Displayable : Strings.Char_Type :=
         Strings.Char_Type'Val (Character'Pos ('A'));
   procedure Do_Test (Title : String);
   --  Tests for various instances of xstring

   type On_Error is new GNATCOLL.Asserts.Error_Reporter with null record;
   overriding procedure On_Assertion_Failed
      (Self     : On_Error;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String);

   Report : On_Error;

   package Asserts is new GNATCOLL.Asserts.Asserts (Report);

   package Equals_Boolean is new Asserts.Equals (Boolean, Boolean'Image);
   package Equals_Integer is new Asserts.Equals (Integer, Integer'Image);
   use Equals_Boolean, Equals_Integer;

   function Image (S : String) return String is ("--" & S & "--");
   function Image (S : Wide_String) return String;
   --  For convenience of instantiation of Asserts, below.

   procedure Reset_Mem;
   --  Reset memory allocation counters

   ---------------
   -- Reset_Mem --
   ---------------

   procedure Reset_Mem is
   begin
      Memory.Allocs := 0;
      Memory.Reallocs := 0;
   end Reset_Mem;

   -----------
   -- Image --
   -----------

   function Image (S : Wide_String) return String is
      Result : Unbounded_String;
   begin
      Append (Result, "--");
      for C of S loop
         if Wide_Character'Pos (C) <= 127 then
            Append (Result, Character'Val (Wide_Character'Pos (C)));
         else
            Append (Result, '['
               & Integer'Image (Wide_Character'Pos (C))
               & ']');
         end if;
      end loop;
      Append (Result, "--");
      return To_String (Result);
   end Image;

   -------------------------
   -- On_Assertion_Failed --
   -------------------------

   overriding procedure On_Assertion_Failed
      (Self     : On_Error;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line
         ((if Msg = "" then "" else Msg & " ")
          & "(at " & Location & ", in " & Entity & ")"
          & ASCII.LF & "   " & Details);
   end On_Assertion_Failed;

   -------------
   -- Do_Test --
   -------------

   procedure Do_Test (Title : String) is
      use Strings;

      function Image (S : XString) return String is
         (Image (S.To_String));

      package Equals1 is new Asserts.Equals
         (T      => Strings.Char_Type,
          Image  => Strings.Char_Type'Image);
      package Equals2 is new Asserts.Equals
         (T      => Strings.Char_String,
          Image  => Image);
      use Equals1, Equals2;

      generic
          type Left_Type (<>) is private;
          type Right_Type (<>) is private;
          Op : String;
          with function Compare
             (Left : Left_Type; Right : Right_Type) return Boolean;
          with function Image (L : Left_Type) return String is <>;
          with function Image (L : Right_Type) return String is <>;
      procedure Assert_Generic
         (Left : Left_Type; Right : Right_Type;
          Location : String := GNAT.Source_Info.Source_Location;
          Entity   : String := GNAT.Source_Info.Enclosing_Entity);

      procedure Assert_Generic
         (Left : Left_Type; Right : Right_Type;
          Location : String := GNAT.Source_Info.Source_Location;
          Entity   : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if not Compare (Left, Right) then
            Asserts.Assert_Failed
               (Image (Left) & ' ' & Op & ' ' & Image (Right),
                Location => Location, Entity => Entity);
         end if;
      end Assert_Generic;

      procedure Assert is new Assert_Generic (XString, Char_String, "=", "=");
      procedure Assert is
         new Assert_Generic (Char_String, XString, "=", "=");
      procedure Assert is new Assert_Generic (XString, XString, "=", "=");

      procedure Assert_Less is
         new Assert_Generic (XString, Char_String, "<", "<");
      procedure Assert_Less is
         new Assert_Generic (Char_String, XString, "<", "<");
      procedure Assert_Less is
         new Assert_Generic (XString, XString, "<", "<");

      procedure Assert_Less_Equal is
         new Assert_Generic (XString, Char_String, "<=", "<=");
      procedure Assert_Less_Equal is
         new Assert_Generic (Char_String, XString, "<=", "<=");
      procedure Assert_Less_Equal is
         new Assert_Generic (XString, XString, "<=", "<=");

      procedure Assert_Greater is
         new Assert_Generic (XString, Char_String, ">", ">");
      procedure Assert_Greater is
         new Assert_Generic (Char_String, XString, ">", ">");
      procedure Assert_Greater is
         new Assert_Generic (XString, XString, ">", ">");

      procedure Assert_Greater_Equal is
         new Assert_Generic (XString, Char_String, ">=", ">=");
      procedure Assert_Greater_Equal is
         new Assert_Generic (Char_String, XString, ">=", ">=");
      procedure Assert_Greater_Equal is
         new Assert_Generic (XString, XString, ">=", ">=");

      Space   : constant Char_Type := Char_Type'Val (Character'Pos (' '));
      Spaces  : constant Char_String := Space & Space;
      Newline : constant Char_Type := Char_Type'Val (Character'Pos (ASCII.LF));
      Equal   : constant Char_Type := Char_Type'Val (Character'Pos ('='));
      Empty : Char_String (1 .. 0);
      Short : Char_String (3 .. 7);
      Long  : Char_String (3 .. Integer (Strings.SSize'Last) + 10);
      Null_String : Char_String (1 .. 0);

      procedure Test_Append;
      procedure Test_Compare;
      procedure Test_Indexing (Base : Char_String);
      procedure Test_Trim;
      procedure Test_Substrings;
      procedure Test_Starts;
      procedure Test_Head_Tail;
      procedure Test_Iterate;
      procedure Test_Modify (Base : Char_String);
      procedure Test_Shrink;
      procedure Test_Swap;
      procedure Test_Justify (Base : Char_String);
      procedure Test_Split (Base : Char_String);
      procedure Test_Search (Base : Char_String);
      procedure Test_Casing;
      procedure Test_Access;

      -----------------
      -- Test_Append --
      -----------------

      procedure Test_Append is
         S, S2 : XString;
      begin
         S.Set (Short);
         A.Assert (S = Short, Title);

         S.Set (Long);
         A.Assert (S = Long, Title);

         S2 := S;
         Append (S, Short);
         A.Assert (S2 = Long, Title);
         A.Assert (S = Long & Short, Title);

         S.Set (Short);
         A.Assert (S = Short, Title);
         A.Assert (S2 = Long, Title);

         --  Appending to a slice
         S.Set (Long);
         S2 := S;
         S.Slice (2, 3);
         S.Append (Short);
         A.Assert (S2 = Long, Title);
         A.Assert
            (S =
             Long (Long'First + 1 .. Long'First + 2)
             & Short,
             Title);
      end Test_Append;

      ------------------
      -- Test_Compare --
      ------------------

      procedure Test_Compare is
         S, S2 : XString;
      begin
         --  Test equality between null strings

         S := Null_XString;
         if S /= Null_XString then
            raise Program_Error;
         end if;

         A.Assert (S.Is_Empty = True, Title);

         --  Test equality for short string and null string

         S := Null_XString;
         S.Set (Empty);
         A.Assert (S = Empty, Title);
         if S /= Null_XString then
            raise Program_Error;
         end if;

         Assert (Compare (S, Empty), 0);

         --  Test equality for big string and null string

         S := Null_XString;
         S.Set (Long);   --  force malloc
         S.Set (Empty);     --  reset to empty string
         A.Assert (S = Empty, Title);
         if S /= Null_XString then
            raise Program_Error;
         end if;

         A.Assert (S.Is_Empty = True, Title);
         A.Assert (Compare (S, Empty) = 0, Title);

         --  Test equality for short strings

         S := Null_XString;
         S2 := Null_XString;
         S.Set (Short);
         S2.Set (Short);
         if S /= S2 then
            raise Program_Error;
         end if;

         --  Test equality for long strings

         S := Null_XString;
         S2 := Null_XString;
         S.Set (Long);
         S2.Set (Long);
         if S /= S2 then
            raise Program_Error;
         end if;

         --  Test equality for short and long strings

         S := Null_XString;
         S2 := Null_XString;
         S.Set (Long);   --  force malloc
         S.Set (Short);
         S2.Set (Short);
         if S /= S2 then
            raise Program_Error;
         end if;

         -- Test equality with Char_String
         declare
            XS : XString;
            S : Char_String := Short;
         begin
            XS.Set (Short);
            A.Assert (To_String(XS) = S, Title);
            A.Assert (To_XString(S) = XS, Title);
            A.Assert (Compare (XS, S) = 0, Title);
         end;

         --  Test equality with substrings

         declare
            S, S2, S3 : XString;
         begin
            S.Set (Short);
            S2.Set (Long);
            A.Assert (S = Short, Title);
            A.Assert (Short = S, Title);
            A.Assert (S2 = Long, Title);
            A.Assert (Long = S2, Title);
            A.Assert (S.Slice (2, 3) = S.Slice (2, 3), Title);
            A.Assert (Compare (S.Slice (2, 3), S.Slice (2, 3)) = 0, Title);
            A.Assert (S.Slice (2, 3) = S2.Slice (2, 3), Title);
            A.Assert (Compare (S.Slice (2, 3), S2.Slice (2, 3)) = 0, Title);
            A.Assert (S2.Slice (2, 3) = S.Slice (2, 3), Title);
            A.Assert (Compare (S2.Slice (2, 3), S.Slice (2, 3)) = 0, Title);
            A.Assert (S2.Slice (2, 3) = S2.Slice (2, 3), Title);
            A.Assert (Compare (S2.Slice (2, 3), S2.Slice (2, 3)) = 0, Title);
            -- Test inequality with substring
            A.Assert (Compare(S2,S) = 1, Title);
            A.Assert (Compare(S,S2) = -1, Title);

            --  Check that we have the same behavior as with standard strings
            A.Assert (S.Slice (3, 2) = Null_XString, Title);
            A.Assert (S.Slice (Natural'Last, 1) = Null_XString, Title);
            A.Assert (S.Slice (1, 0) = Null_XString, Title);

            S3 := S2;
            S3.Slice (3, 2);
            A.Assert (S3 = Null_XString, Title);

            S3 := S2;
            S3.Slice (Natural'Last, 1);
            A.Assert (S3 = Null_XString, Title);

            S3 := S2;
            S3.Slice (3, 0);
            A.Assert (S3 = Null_XString, Title);
         end;

         declare
            S, S2, S3 : XString;
         begin
            S.Set (Short);
            S2.Set (Long);

            Assert_Less (S, S2);
            Assert_Greater (S2, S);

            Assert_Less          (S, Short (Short'First + 1 .. Short'Last));
            Assert_Less_Equal    (S, Short (Short'First + 1 .. Short'Last));
            Assert_Greater_Equal (Short (Short'First + 1 .. Short'Last), S);
            Assert_Greater       (Short (Short'First + 1 .. Short'Last), S);

            Assert_Less          (Short, S.Slice (2, 3));
            Assert_Less_Equal    (Short, S.Slice (2, 3));
            Assert_Greater_Equal (S.Slice (2, 3), Short);
            Assert_Greater       (S.Slice (2, 3), Short);

            A.Assert (Compare (S, Short (Short'First + 1 .. Short'Last)) = -1,
                      Title);
            A.Assert (Compare (Short (Short'First + 1 .. Short'Last), S) = 1,
                      Title);

            Assert_Less (S2, Long (Long'First + 1 .. Long'Last));
            Assert_Less (Long, S2.Slice (2, 3));
            A.Assert (Compare (S2, Long (Long'First + 1 .. Long'Last)) = -1,
                      Title);
            A.Assert (Compare (Long (Long'First + 1 .. Long'Last), S2) = 1,
                      Title);

            A.Assert (Compare (S, S) = 0, Title);
            A.Assert (Compare (S2, S2) = 0, Title);

            S3.Set(Short(Short'First) &
                   Char_Type'Val(Char_Type'Pos(Short(Short'First + 1)) + 1) &
                   Short(Short'First + 2..Short'Last));
            A.Assert (Compare (S3, S) = 1, Title);

         end;

      end Test_Compare;

      -------------------
      -- Test_Indexing --
      -------------------

      procedure Test_Indexing (Base : Char_String) is
         Is_Long : constant Boolean :=
            Base'Length > Natural (Strings.SSize'Last);
         S, S2 : XString;
         Idx : Positive;
      begin
         S.Set (Base);

         Reset_Mem;

         --  Test the constant indexing aspect
         --  We check memory allocations to make sure we are using
         --  a constant indexing aspect

         if S (1) /= First_Displayable then
            raise Program_Error;
         end if;
         if S (2) /= Char_Type'Succ (First_Displayable) then
            raise Program_Error;
         end if;

         A.Assert (Memory.Allocs = 0, Title);
         A.Assert (Memory.Reallocs = 0, Title);

         begin
            if S (10000) /= First_Displayable then
               null;
            end if;
            if S (Base'Length + 1) /= First_Displayable then
               null;
            end if;
            raise Program_Error;  --  should have receive exeption
         exception
            when Ada.Strings.Index_Error =>
               null;
         end;

         --  Testing loops
         --  We check memory allocations to make sure we are using
         --  a constant indexing aspect

         Reset_Mem;
         Idx := Base'First;
         for C of S loop
            Assert (C, Base (Idx));
            Idx := Idx + 1;
         end loop;
         Assert (Idx, Base'Last + 1);
         S2 := S;    --  Test that the string is still shareable
         A.Assert (S = Base, Title);
         A.Assert (S2 = Base, Title);
         A.Assert (Memory.Allocs = (if not Strings.Copy_On_Write and then
                   Is_Long then 1 else 0), Title);
         A.Assert (Memory.Reallocs = 0, Title);

         --  Same test for loops on indexes

         Reset_Mem;
         Idx := Base'First;
         for Idx2 in S.Iterate loop
            A.Assert (Idx2 = Idx - Base'First + 1, Title);
            A.Assert (S (Idx2) = Base (Idx), Title);
            Idx := Idx + 1;
         end loop;
         Assert (Idx, Base'Last + 1);
         S2 := S;
         A.Assert (S = Base, Title);
         A.Assert (S2 = Base, Title);
         A.Assert (Memory.Allocs = (if not Strings.Copy_On_Write and then
                   Is_Long then 1 else 0),
                   Title);
         A.Assert (Memory.Reallocs = 0, Title);

         --  Test the Variable indexing aspect

         S.Set (Base);
         S (2) := First_Displayable;
         A.Assert (S = Base (Base'First) & First_Displayable
            & Base (Base'First + 2 .. Base'Last), Title);

         Reset_Mem;
         S.Set (Base);
         S2 := S;
         S (2) := First_Displayable;   --  makes a copy
         A.Assert (S2 = Base, Title);
         A.Assert (S = Base (Base'First) & First_Displayable
            & Base (Base'First + 2 .. Base'Last), Title);

         A.Assert (Memory.Allocs = (if Is_Long then 1 else 0), Title);
         A.Assert (Memory.Reallocs = 0, Title);

         Reset_Mem;
         S.Set (Base);
         declare
            R : Character_Reference := S.Reference (2);
         begin
            S2 := S;
            R := First_Displayable;
            A.Assert (S2 = Base, Title);
            A.Assert (S = Base (Base'First) & First_Displayable
                      & Base (Base'First + 2 .. Base'Last),
                      Title);
         end;
         A.Assert (Memory.Allocs = (if Is_Long then 1 else 0), Title);
         A.Assert (Memory.Reallocs = 0, Title);

      end Test_Indexing;

      ---------------
      -- Test_Trim --
      ---------------

      procedure Test_Trim is
         pragma Compile_Time_Error
            (Short'Length + 2 * Spaces'Length > Integer (SSize'Last),
             "Cannot test short string with spaces");
         --  So that we can test Trim with small strings.

         S, S2 : XString;
      begin
         --  Short strings

         S.Set (Spaces & Short & Spaces);

         A.Assert (S.Trim (Chars => Space) = Short, Title);
         A.Assert (S.Trim (Ada.Strings.Right) = Spaces & Short, Title);
         A.Assert (S.Trim (Ada.Strings.Left) = Short & Spaces, Title);

         S.Trim (Chars => Space);
         A.Assert (S = Short, Title);

         S.Set (Spaces & Short & Spaces);
         S.Trim (Side => Ada.Strings.Right, Chars => Space);
         A.Assert (S = Spaces & Short, Title);

         S.Set (Spaces & Short & Spaces);
         S.Trim (Side => Ada.Strings.Left, Chars => Space);
         A.Assert (S = Short & Spaces, Title);

         --  Long strings

         S.Set (Spaces & Long & Spaces);

         A.Assert (S.Trim = Long, Title);
         A.Assert (S.Trim (Ada.Strings.Right) = Spaces & Long, Title);
         A.Assert (S.Trim (Ada.Strings.Left) = Long & Spaces, Title);

         S2 := S;
         S.Trim (Chars => Space);
         A.Assert (S = Long, Title);

         S := S2;
         S.Trim (Side => Ada.Strings.Right, Chars => Space);
         A.Assert (S = Spaces & Long, Title);

         S := S2;
         S.Trim (Side => Ada.Strings.Left, Chars => Space);
         A.Assert (S = Long & Spaces, Title);

         --  Long strings with First /= 1

         S.Set (Spaces & Spaces & Long & Spaces & Spaces);
         S.Slice (3, Spaces'Length * 4 + Long'Length - 2);
         A.Assert (S.Trim = Long, Title);
         A.Assert (S.Trim (Ada.Strings.Right) = Spaces & Long, Title);
         A.Assert (S.Trim (Ada.Strings.Left) = Long & Spaces, Title);

         S2 := S;
         S.Trim;
         A.Assert (S = Long, Title);

         S := S2;
         S.Trim (Ada.Strings.Right);
         A.Assert (S = Spaces & Long, Title);

         S := S2;
         S.Trim (Ada.Strings.Left);
         A.Assert (S = Long & Spaces, Title);
      end Test_Trim;

      ---------------------
      -- Test_Substrings --
      ---------------------

      procedure Test_Substrings is
         S, S2, S3 : XString;
      begin
         --  Short strings

         S.Set (Spaces & Short & Spaces);
         S2 := S;   --  shared buffer

         S.Trim;    --  get a substring
         S2.Append (Short);  --  no longer shared
         S3 := S;   --  shared buffer
         S3.Append (Short);  --  no longer shared

         A.Assert (S = Short, Title);
         A.Assert (S2 = Spaces & Short & Spaces & Short, Title);
         A.Assert (S3 = Short & Short, Title);

         --  Check that first character is still index 1 even with
         --  small strings.
         A.Assert (S (1) = First_Displayable, Title);
      end Test_Substrings;

      -----------------
      -- Test_Starts --
      -----------------

      procedure Test_Starts is
         S : XString;
      begin
         S.Set (Short);

         A.Assert (S.Starts_With (Short) = True, Title);
         A.Assert (S.Starts_With (Short (3 .. 4)) = True, Title);
         A.Assert (S.Starts_With (Null_XString) = True, Title);
         A.Assert (S.Starts_With (Null_String) = True, Title);
         A.Assert (S.Starts_With (Short & First_Displayable) = False, Title);

         A.Assert (S.Ends_With (Short) = True, Title);
         A.Assert (S.Ends_With (Short (Short'Last - 1 .. Short'Last)) = True,
                   Title);
         A.Assert (S.Ends_With (Null_XString) = True, Title);
         A.Assert (S.Ends_With (Null_String) = True, Title);
         A.Assert (S.Ends_With (Short & First_Displayable) = False, Title);

         S.Set (Long);

         A.Assert (S.Starts_With (Long) = True, Title);
         A.Assert (S.Starts_With (Long (3 .. 4)) = True, Title);
         A.Assert (S.Starts_With (Null_XString) = True, Title);
         A.Assert (S.Starts_With (Null_String) = True, Title);
         A.Assert (S.Starts_With (Long & First_Displayable) = False, Title);

         A.Assert (S.Ends_With (Long) = True, Title);
         A.Assert (S.Ends_With (Long (Long'Last - 1 .. Long'Last)) = True,
                   Title);
         A.Assert (S.Ends_With (Null_XString) = True, Title);
         A.Assert (S.Ends_With (Null_String) = True, Title);
         A.Assert (S.Ends_With (Long & First_Displayable) = False, Title);
      end Test_Starts;

      --------------------
      -- Test_Head_Tail --
      --------------------

      procedure Test_Head_Tail is
         S, S2 : XString;
      begin
         S.Set (Short);
         A.Assert (S.Head (1) = Short (Short'First .. Short'First), Title);
         A.Assert (S.Head (2) = Short (Short'First .. Short'First + 1), Title);
         A.Assert (S.Head (1000) = Short, Title);
         A.Assert (S.Tail (1) = Short (Short'Last .. Short'Last), Title);
         A.Assert (S.Tail (2) = Short (Short'Last - 1 .. Short'Last), Title);
         A.Assert (S.Tail (1000) = Short, Title);

         S.Set (Long);
         A.Assert (S.Head (1) = Long (Long'First .. Long'First), Title);
         A.Assert (S.Head (2) = Long (Long'First .. Long'First + 1), Title);
         A.Assert (S.Head (1000) = Long, Title);
         A.Assert (S.Tail (1) = Long (Long'Last .. Long'Last), Title);
         A.Assert (S.Tail (2) = Long (Long'Last - 1 .. Long'Last), Title);
         A.Assert (S.Tail (1000) = Long, Title);

         S.Set (Long);
         S.Slice (3, 10);
         A.Assert (S.Head (1) = Long (Long'First + 2 .. Long'First + 2), Title);
         A.Assert (S.Head (2) = Long (Long'First + 2 .. Long'First + 3), Title);
         A.Assert (S.Head (1000) = Long (Long'First + 2 .. Long'First + 9),
                   Title);
         A.Assert (S.Tail (1) = Long (Long'First + 9 .. Long'First + 9), Title);
         A.Assert (S.Tail (2) = Long (Long'First + 8 .. Long'First + 9), Title);
         A.Assert (S.Tail (1000) =Long (Long'First + 2 .. Long'First + 9), Title);

         --  A slice into itself
         S.Set (Long);
         S.Slice (3, 10, Into => S);
         A.Assert (S = Long (Long'First + 2 .. Long'First + 9), Title);

         --  A slice into an already allocated string should not leak
         --  memory.

         S.Set (Long);
         S2.Set (Long);
         S.Slice (3, 10, Into => S2);
         A.Assert (S = Long, Title);
         A.Assert (S2 = Long (Long'First + 2 .. Long'First + 9), Title);

      end Test_Head_Tail;

      ------------------
      -- Test_Iterate --
      ------------------

      procedure Test_Iterate is
         S : XString;
         Expected : Natural;
         E : Char_Type;
      begin
         --  Iterate on indexes, short string

         S.Set (Short);

         Expected := 1;
         for Index of S.Iterate loop
            A.Assert (Index = Expected, Title);
            Expected := Expected + 1;
         end loop;
         A.Assert (Expected = Short'Length + 1, Title);

         Expected := 1;
         for Index of S.Slice (2, 3).Iterate loop
            A.Assert (Index = Expected, Title);
            Expected := Expected + 1;
         end loop;
         A.Assert (Expected = 3, Title);

         --  Iterate on characters, short string

         E := First_Displayable;
         for C of S loop
            A.Assert (C = E, Title);
            E := Char_Type'Succ (E);
         end loop;
         A.Assert (Char_Type'Pos (E) =
                   Char_Type'Pos (First_Displayable) + Short'Length,
                   Title);

         E := Char_Type'Succ (First_Displayable);
         for C of S.Slice (2, 3) loop
            A.Assert (C = E, Title);
            E := Char_Type'Succ (E);
         end loop;
         A.Assert (Char_Type'Pos (E) =
                   Char_Type'Pos (First_Displayable) + 3, Title);

         --  Iterate on indexes, long string

         S.Set (Long);

         Expected := 1;
         for Index of S.Iterate loop
            A.Assert (Index = Expected, Title);
            Expected := Expected + 1;
         end loop;
         A.Assert (Expected = Long'Length + 1, Title);

         Expected := 1;
         for Index of S.Slice (2, 10).Iterate loop
            A.Assert (Index = Expected, Title);
            Expected := Expected + 1;
         end loop;
         A.Assert (Expected = 10, Title);

         --  Iterate on characters, long string

         E := First_Displayable;
         for C of S loop
            A.Assert (C = E, Title);
            E := Char_Type'Succ (E);
         end loop;
         A.Assert (Char_Type'Pos (E) =
                   Char_Type'Pos (First_Displayable) + Long'Length,
                   Title);

         E := Char_Type'Succ (First_Displayable);
         for C of S.Slice (2, 10) loop
            A.Assert (C = E, Title);
            E := Char_Type'Succ (E);
         end loop;
         A.Assert (Char_Type'Pos (E) =
                   Char_Type'Pos (First_Displayable) + 10, Title);

      end Test_Iterate;

      -----------------
      -- Test_Modify --
      -----------------

      procedure Test_Modify(Base : Char_String) is
         S, S2 : XString;
      begin
         S.Set (Base);
         S2 := S;
         S.Replace (2, First_Displayable);
         A.Assert
            (S =
             Base (Base'First)
             & First_Displayable
             & Base (Base'First + 2 .. Base'Last),
             Title);
         A.Assert (S2 = Base, Title);

         --  Replace with small string
         S.Set (Base);
         S2 := S;
         S.Replace (2, 3, (3 => First_Displayable));
         A.Assert
            (S =
             Base (Base'First)
             & First_Displayable
             & Base (Base'First + 3 .. Base'Last),
             Title);
         A.Assert (S2 = Base, Title);

         --  Replace with longer string
         S.Set (Base);
         S2 := S;
         S.Replace (2, 3, (3 .. 20 => First_Displayable));
         A.Assert
            (S =
             Base (Base'First)
             & (3 .. 20 => First_Displayable)
             & Base (Base'First + 3 .. Base'Last),
             Title);
         A.Assert (S2 = Base,
                   Title);

         --  Replace an empty string (same as insert)
         S.Set (Base);
         S2 := S;
         S.Replace (2, 1, (3 .. 20 => First_Displayable));
         A.Assert
            (S =
             Base (Base'First)
             & (3 .. 20 => First_Displayable)
             & Base (Base'First + 1 .. Base'Last),
             Title);
         A.Assert (S2 = Base, Title);

         --  Replace with self
         S.Set (Base);
         S.Replace_Slice (1, 1, S);
         A.Assert
            (S =
             Base
             & Base (Base'First + 1 .. Base'Last),
             Title);

         S.Set (Base);
         S2 := S;
         S.Insert (3, First_Displayable & First_Displayable);
         A.Assert
            (S =
             Base (Base'First .. Base'First + 1)
             & First_Displayable & First_Displayable
             & Base (Base'First + 2 .. Base'Last),
             Title);
         A.Assert (S2 = Base, Title);

         --  Overwrite with shorter string
         S.Set (Base);
         S2 := S;
         S.Overwrite (3, Base (Base'First .. Base'First + 1));
         A.Assert
            (S =
             Base (Base'First .. Base'First + 1)
             & Base (Base'First .. Base'First + 1)
             & Base (Base'First + 4 .. Base'Last),
             Title);
         A.Assert (S2 = Base, Title);

         --  Overwrite with longer string
         S.Set (Base);
         S2 := S;
         S.Overwrite (3, Base);
         A.Assert
            (S =
             Base (Base'First .. Base'First + 1)
             & Base,
             Title);
         A.Assert (S2 = Base, Title);

         --  Deleting
         S.Set (Base);
         S2 := S;
         S.Delete (3, 4);
         A.Assert
            (S =
             Base (Base'First .. Base'First + 1)
             & Base (Base'First + 4 .. Base'Last),
             Title);
         A.Assert (S2 = Base, Title);

         --  Deleting past the end
         S.Set (Base);
         S2 := S;
         S.Delete (3, 10_000);
         A.Assert
            (S =
             Base (Base'First .. Base'First + 1),
             Title);
         A.Assert (S2 = Base, Title);

         --  Clearing the string
         S.Set (Base);
         S2 := S;
         S.Clear;
         A.Assert (S2 = Base, Title);
         A.Assert (S = Null_String, Title);
      end Test_Modify;

      -----------------
      -- Test_Shrink --
      -----------------

      procedure Test_Shrink is
         S : XString;
      begin
         S.Set (Short);
         S.Shrink;
         A.Assert (S = Short, Title);

         S := 50 * Long;
         S.Set (Long);

         S.Shrink;
         A.Assert (S = Long, Title);

         S.Set (Short);
         S.Shrink;
         A.Assert (S = Short, Title);
      end Test_Shrink;

      ---------------
      -- Test_Swap --
      ---------------

      procedure Test_Swap is
         S, S2 : XString;
      begin
         --  Swapping two short strings

         S.Set (Short);
         S2.Set (Spaces);

         S.Swap (S2);
         A.Assert (S = Spaces, Title);
         A.Assert (S2 = Short, Title);

         S.Swap (S2);
         A.Assert (S = Short, Title);
         A.Assert (S2 = Spaces, Title);

         --  Swapping a short and a long string

         S.Set (Short);
         S2.Set (Long);

         S.Swap (S2);
         A.Assert (S = Long, Title);
         A.Assert (S2 = Short, Title);

         S.Swap (S2);
         A.Assert (S = Short, Title);
         A.Assert (S2 = Long, Title);

         --  Swapping self

         S.Set (Short);
         S.Swap (S);
         A.Assert (S = Short, Title);

         S.Set (Long);
         S.Swap (S);
         A.Assert (S = Long, Title);
      end Test_Swap;

      ------------------
      -- Test_Justify --
      ------------------

      procedure Test_Justify (Base : Char_String) is
         S, S2 : XString;
      begin
         --  procedure Center

         S.Set (Base);
         S.Center (Width => Base'Length + 4);
         A.Assert (S = Space & Space & Base & Space & Space, Title);

         S.Clear;
         S.Set (Base);
         S.Center (Width => Base'Length + 3);
         A.Assert (S = Space & Space & Base & Space, Title);

         S.Clear;
         S.Set (Base);
         S.Center (Width => Base'Length - 1);
         A.Assert (S = Base, Title);

         S.Clear;
         S.Set (Base);
         S.Center (Base'Length + 2, First_Displayable);
         A.Assert (S = First_Displayable & Base & First_Displayable, Title);

         --  function Center

         S.Clear;
         S.Set (Base);
         A.Assert (S.Center (Base'Length + 4) =
                   Space & Space & Base & Space & Space,
                   Title);
         A.Assert (S.Center (Base'Length + 3) =
                   Space & Space & Base & Space,
                   Title);
         A.Assert (S.Center (Base'Length - 1) = Base, Title);

         --  procedure Left justify

         S.Clear;
         S.Set (Base);
         S.Left_Justify (Width => Base'Length + 2);
         A.Assert (S = Base & Space & Space, Title);

         S.Clear;
         S.Set (Base);
         S.Left_Justify (Width => Base'Length - 1);
         A.Assert (S = Base, Title);

         S.Clear;
         S.Set (Base);
         S.Left_Justify (Base'Length + 2, First_Displayable);
         A.Assert (S = Base & First_Displayable & First_Displayable, Title);

         --  function Left justify

         S.Clear;
         S.Set (Base);
         A.Assert (S.Left_Justify (Base'Length + 2) = Base & Space & Space,
                   Title);
         A.Assert (S.Left_Justify (Base'Length - 1) = Base,
                   Title);
         A.Assert (S.Left_Justify (Base'Length + 2, First_Displayable) =
                   Base & First_Displayable & First_Displayable,
                   Title);

         --  procedure Right justify

         S.Clear;
         S.Set (Base);
         S.Right_Justify (Width => Base'Length + 2);
         A.Assert (S = Space & Space & Base, Title);

         S.Clear;
         S.Set (Base);
         S.Right_Justify (Width => Base'Length - 1);
         A.Assert (S = Base, Title);

         S.Clear;
         S.Set (Base);
         S.Right_Justify (Base'Length + 2, First_Displayable);
         A.Assert (S = First_Displayable & First_Displayable & Base,
                   Title);

         --  function Right justify

         S.Set (Base);
         A.Assert (S.Right_Justify (Base'Length + 2) = Space & Space & Base,
                   Title);
         A.Assert (S.Right_Justify (Base'Length - 1) = Base,
                   Title);
         A.Assert (S.Right_Justify (Base'Length + 2, First_Displayable) =
                   First_Displayable & First_Displayable & Base,
                   Title);
      end Test_Justify;

      ----------------
      -- Test_Split --
      ----------------

      procedure Test_Split (Base : Char_String) is
         Is_Long : constant Boolean :=
            Base'Length > Natural (Strings.SSize'Last);
         S, S2 : XString;
      begin
         S.Set (Space
                & Space
                & Base & Space
                & Base & Space
                & Base & Space
                & Space);

         Reset_Mem;
         declare
            R : constant XString_Array := S.Split (Space);
         begin
            --  2 copies per non-null substring when not copy-on-write
            --     1 copy when creating the slide
            --     1 copies when returning the array

            A.Assert (Memory.Allocs =
                      (if Is_Long and not Copy_On_Write then 6 else 0),
                      Title);
            A.Assert (Memory.Reallocs = 0, Title);

            A.Assert (R'Length = 7, Title);
            A.Assert (R (R'First + 0) = Null_XString, Title);
            A.Assert (R (R'First + 1) = Null_String, Title);
            A.Assert (R (R'First + 2) = Base, Title);
            A.Assert (R (R'First + 3) = Base, Title);
            A.Assert (R (R'First + 4) = Base, Title);
            A.Assert (R (R'First + 5) = Null_String, Title);
            A.Assert (R (R'First + 6) = Null_String, Title);

            S2.Set_As_Join (Space, R);
            A.Assert (S2 = S, Title);

            A.Assert (Join (Space, R) = S, Title);
            A.Assert (Join ((1 => Space), R) = S, Title);
         end;

         Reset_Mem;
         declare
            R : XString_Array (1 .. 20);
            Last : Natural;
         begin
            S.Split (Space, Into => R, Last => Last);

            --  One copy per non-null substring when not copy-on-write
            A.Assert (Memory.Allocs =
                    (if Is_Long and not Copy_On_Write then 3 else 0),
                    Title);
            A.Assert (Memory.Reallocs = 0, Title);

            A.Assert (Last = 7, Title);
         end;

         Reset_Mem;
         declare
            R : constant XString_Array := S.Right_Split (Space);
         begin
            --  Two copies per non-null substring when not copy-on-write
            A.Assert (Memory.Allocs =
                      (if Is_Long and not Copy_On_Write then 6 else 0),
                      Title);
            A.Assert (Memory.Reallocs = 0, Title);

            A.Assert (R'Length = 7, Title);
            A.Assert (R (R'First + 6) = Null_XString, Title);
            A.Assert (R (R'First + 5) = Null_String, Title);
            A.Assert (R (R'First + 4) = Base, Title);
            A.Assert (R (R'First + 3) = Base, Title);
            A.Assert (R (R'First + 2) = Base, Title);
            A.Assert (R (R'First + 1) = Null_String, Title);
            A.Assert (R (R'First + 0) = Null_String, Title);
         end;

         Reset_Mem;
         declare
            R : XString_Array (1 .. 20);
            Last : Natural;
         begin
            S.Right_Split (Space, Into => R, Last => Last);

            A.Assert (Memory.Allocs =
                      (if Is_Long and not Copy_On_Write then 3 else 0),
                      Title);
            A.Assert (Memory.Reallocs = 0, Title);

            A.Assert (Last = 7, Title);
         end;

         declare
            R : constant XString_Array := S.Split (Space, Max_Split => 1);
            R2 : constant XString_Array :=
               S.Right_Split (Space, Max_Split => 1);
         begin
            A.Assert (R'Length = 1, Title);
            A.Assert (R (R'First + 0) = S, Title);
            A.Assert (R2'Length = 1, Title);
            A.Assert (R2 (R2'First + 0) = S, Title);

            S2.Set_As_Join (Space, R);
            A.Assert (S2 = S, Title);
            A.Assert (Join (Space, R) = S, Title);
            A.Assert (Join ((1 => Space), R) = S, Title);
         end;

         declare
            R : constant XString_Array := S.Split (Space, Max_Split => 2);
            R2 : constant XString_Array :=
               S.Right_Split (Space, Max_Split => 2);
         begin
            A.Assert (R'Length = 2, Title);
            A.Assert (R (R'First + 0) = Null_XString, Title);
            A.Assert (R (R'First + 1) =
                      Space & Base & Space & Base & Space & Base &
                      Space & Space,
                      Title);

            A.Assert (R2'Length = 2, Title);
            A.Assert (R2 (R2'First + 0) = Null_XString, Title);
            A.Assert (R2 (R2'First + 1) =
                      Space & Space & Base & Space & Base & Space &
                      Base & Space,
                      Title);

            S2.Set_As_Join (Space, R);
            A.Assert (S2 = S, Title);
            A.Assert (Join (Space, R) = S, Title);
            A.Assert (Join ((1 => Space), R) = S, Title);
         end;

         declare
            R : constant XString_Array := S.Split (Space, Omit_Empty => True);
            R2 : constant XString_Array :=
               S.Right_Split (Space, Omit_Empty => True);
         begin
            A.Assert (R'Length = 3, Title);
            A.Assert (R (R'First + 0) = Base, Title);
            A.Assert (R (R'First + 1) = Base, Title);
            A.Assert (R (R'First + 2) = Base, Title);

            A.Assert (R2'Length = 3, Title);
            A.Assert (R2 (R2'First + 2) = Base, Title);
            A.Assert (R2 (R2'First + 1) = Base, Title);
            A.Assert (R2 (R2'First + 0) = Base, Title);
         end;

         declare
            R : constant XString_Array :=
               Null_XString.Split (Space, Omit_Empty => True);
            R2 : constant XString_Array :=
               Null_XString.Right_Split (Space, Omit_Empty => True);
         begin
            A.Assert (R'Length = 0, Title);
            A.Assert (R2'Length = 0, Title);
         end;

         --  Splitting on strings

         declare
            R : constant XString_Array := S.Split (Space & Space);
         begin
            A.Assert (R'Length = 3, Title);
            A.Assert (R (R'First + 0) = Null_XString, Title);
            A.Assert (R (R'First + 1) = Base & Space & Base & Space & Base,
                      Title);
            A.Assert (R (R'First + 2) = Null_XString, Title);
         end;

         declare
            R : constant XString_Array :=
               S.Split (Space & Space, Omit_Empty => True);
         begin
            A.Assert (R'Length = 1, Title);
            A.Assert (R (R'First + 0) =
                      Base & Space & Base & Space & Base,
                      Title);
         end;

         A.Assert (Null_XString.Split (Space & Space)'Length = 0, Title);
         A.Assert (Null_XString.Split (Null_String)'Length = 0, Title);
         A.Assert (S.Split (Null_String)'Length = 0, Title);

         declare
            R : constant XString_Array := S.Right_Split (Space & Space);
         begin
            A.Assert (R'Length = 3, Title);
            A.Assert (R (R'First + 0) = Null_XString, Title);
            A.Assert (R (R'First + 1) = Base & Space & Base & Space & Base,
                      Title);
            A.Assert (R (R'First + 2) = Null_XString, Title);
         end;

         declare
            R : constant XString_Array :=
               S.Right_Split (Space & Space, Omit_Empty => True);
         begin
            A.Assert (R'Length = 1, Title);
            A.Assert (R (R'First + 0) = Base & Space & Base & Space & Base,
                      Title);
         end;

         A.Assert (Null_XString.Right_Split (Space & Space)'Length = 0, Title);
         A.Assert (Null_XString.Right_Split (Null_String)'Length = 0, Title);
         A.Assert (S.Right_Split (Null_String)'Length = 0, Title);

         --  Joining

         A.Assert (Join (Null_XString,
                         XString_Array'(1 .. 0 => Null_XString)) =
                   Null_XString,
                   Title);

         A.Assert (Join (Space,
                         XString_Array'(1 .. 0 => Null_XString)) =
                   Null_XString,
                   Title);

         A.Assert (Join (Space & Space,
                         XString_Array'(1 .. 0 => Null_XString)) =
                   Null_XString,
                   Title);

         --  Parsing a file containing "key = value" lines

         declare
            L : XString_Array (1 .. 2);
            Key, Value : XString;
            Last : Natural;
         begin
            S.Set (Spaces & Newline
                   & Base & Equal & Base & Newline
                   & Base & Equal & Base & Newline
                   & Base & Equal & Base & Newline);
            Reset_Mem;
            for Line of S.Split (Newline) loop
               Line.Split (Equal, Into => L, Last => Last);
               if Last = 2 then
                  Key := L (1);
                  Key.Trim;
                  Value := L (2);
                  Value.Trim;
               end if;
            end loop;

            A.Assert (Memory.Allocs =
                      (if Is_Long and not Copy_On_Write then 14 else 0),
                      Title);
            A.Assert (Memory.Reallocs = 0, Title);
         end;
      end Test_Split;

      -----------------
      -- Test_Search --
      -----------------

      procedure Test_Search (Base : Char_String) is
         S, S2 : XString;
      begin
         S.Set (Space
                & Space
                & Base & Space
                & Base & Space
                & Base & Space
                & Space);

         A.Assert (S.Count (Space) = 6, Title);
         A.Assert (S.Count (Char_Type'Val (0)) = 0, Title);
         A.Assert (S.Count (Space & Space) = 2, Title);
         A.Assert
            (S.Count (First_Displayable & Char_Type'Succ (First_Displayable)) =
             3,
             Title);

         A.Assert (S.Count (Null_String) = Natural'Last, Title);
         A.Assert (Null_XString.Count (Space) = 0, Title);
         A.Assert (Null_XString.Count (Space & Space) = 0, Title);
         A.Assert (Null_XString.Count (Null_String) = 0, Title);

         A.Assert (S.Find (Space) = 1, Title);
         A.Assert (S.Find (Space & Space) = 1, Title);

         A.Assert (S.Slice (2, 3).Find (Space) = 1, Title);
         A.Assert (S.Slice (3, Base'Length * 2).Find (Space) = Base'Length + 1,
                   Title);
         A.Assert (S.Find (Space, Low => 2, High => 3) = 2, Title);
         A.Assert (S.Find (Space, Low => 3) = Base'Length + 3, Title);
         A.Assert (S.Find (Base, Low => 3, High => 4) = 0, Title);

         A.Assert (S.Find (First_Displayable) = 3, Title);
         A.Assert (S.Find (Base) = 3, Title);

         A.Assert (S.Right_Find (Space) = S.Length, Title);
         A.Assert (S.Right_Find (Space & Space) = S.Length - 1, Title);
         A.Assert (S.Right_Find (Base) = S.Length - 1 - Base'Length, Title);

         A.Assert (S.Find (First_Displayable & First_Displayable) = 0, Title);
         A.Assert (S.Right_Find (First_Displayable & First_Displayable) = 0,
                   Title);
      end Test_Search;

      -----------------
      -- Test_Casing --
      -----------------

      procedure Test_Casing is
         S : XString;
         S2 : XString;
      begin
         S.Set (Short);
         A.Assert (S.Is_Upper = True, Title);
         A.Assert (S.Is_Lower = False, Title);

         S2 := To_Lower(S);
         A.Assert (S2.Is_Upper = False, Title);
         A.Assert (S2.Is_Lower = True, Title);

         S.To_Lower;
         A.Assert (S.Is_Upper = False, Title);
         A.Assert (S.Is_Lower = True, Title);

         S.Capitalize;
         A.Assert ((Slice (S, 1, 1).Is_Upper and
                    Slice (S, 2, S.Length).Is_Lower) = True,
                   Title);
         A.Assert ((Slice (S, 1, 1).Is_Lower or
                    Slice (S, 2, S.Length).Is_Upper) = False,
                   Title);

         S2 := To_Upper(S);
         A.Assert (S2.Is_Upper = True, Title);
         A.Assert (S2.Is_Lower = False, Title);

         S.To_Upper;
         A.Assert (S.Is_Upper = True, Title);
         A.Assert (S.Is_Lower = False, Title);

         S.Set (Long);
         A.Assert (S.Is_Upper = (Long'Length < 100), Title);
         A.Assert (S.Is_Lower = False, Title);

         S2 := To_Lower(S);
         A.Assert (S2.Is_Upper = False, Title);
         A.Assert (S2.Is_Lower = True, Title);

         S.To_Lower;
         A.Assert (S.Is_Upper = False, Title);
         A.Assert (S.Is_Lower = True, Title);

         S2 := To_Upper(S);
         A.Assert (S2.Is_Upper = True, Title);
         A.Assert (S2.Is_Lower = False, Title);

         S.To_Upper;
         A.Assert (S.Is_Upper = True, Title);
         A.Assert (S.Is_Lower = False, Title);

         S.Capitalize;
         A.Assert ((Slice (S, 1, 1).Is_Upper and
                    Slice (S, 2, S.Length).Is_Lower) = True,
                   Title);
         A.Assert ((Slice (S, 1, 1).Is_Lower or
                    Slice (S, 2, S.Length).Is_Upper) = False,
                   Title);

      end Test_Casing;

      -----------------
      -- Test_Access --
      -----------------

      procedure Test_Access is
         S : XString;
         Is_Long : Boolean;

         procedure Callback (Data : Char_String) is
         begin
            A.Assert (Data = (if Is_Long then Long else Short), Title);

            S.Append (Space);
            A.Assert (Data = (if Is_Long then Long else Short), Title);
            A.Assert (S = (if Is_Long then Long else Short) & Space, Title);

            S := Null_XString;
            A.Assert (Data = (if Is_Long then Long else Short), Title);
         end Callback;

      begin
         S.Set (Short);
         Is_Long := False;
         S.Access_String (Callback'Access);

         S.Set (Long);
         Is_Long := True;
         S.Access_String (Callback'Access);
      end Test_Access;

   begin
      declare
         C : Char_Type := First_Displayable;
      begin
         for S in Short'Range loop
            Short (S) := C;
            C := Char_Type'Succ (C);
         end loop;

         C := First_Displayable;
         for S in Long'Range loop
            Long (S) := C;
            C := Char_Type'Succ (C);
         end loop;
      end;

      declare
         -- coverage of function '*' (Count : Natural;
         --                           Right : Char_Type) return XString
         -- and of function '*' (Count : Natural;
         --                      Right : XString) return XString
         S1 : XString := 4 * First_Displayable;
         S2 : XString := 4 * S1;
      begin
         A.Assert (S1.Length = 4);
         A.Assert (S1 (1) = First_Displayable);
         A.Assert (S1 (S1.Length) = First_Displayable);
         A.Assert (S2.Length = 16);
         A.Assert (S2 (1) = First_Displayable);
         A.Assert (S2 (S2.Length) = First_Displayable);
      end;

      Test_Append;
      Test_Compare;
      Test_Indexing (Short);
      Test_Indexing (Long);
      Test_Trim;
      Test_Substrings;
      Test_Starts;
      Test_Head_Tail;
      Test_Iterate;
      Test_Modify (Short);
      Test_Modify (Long);
      Test_Shrink;
      Test_Swap;
      Test_Justify (Short);
      Test_Justify (Long);
      Test_Search (Short);
      Test_Search (Long);
      Test_Split (Short);
      Test_Split (Long);
      Test_Casing;
      Test_Access;
   end Do_Test;

   procedure Test_COW is new Do_Test (GNATCOLL.Strings);

   package Basic_No_COW is new GNATCOLL.Strings_Impl.Strings
      (GNATCOLL.Strings_Impl.Optimal_String_Size,
       Character, String, Copy_On_Write => False);
   procedure Test_No_COW is new Do_Test (Basic_No_COW);

   type SSize_3 is mod 128;
   package Strings3 is new GNATCOLL.Strings_Impl.Strings
      (SSize_3, Character, String);
   procedure Test3 is new Do_Test (Strings3);

   type SSize_2 is mod 10;
   package Wide is new GNATCOLL.Strings_Impl.Strings
      (SSize_2,
       Wide_Character, Wide_String, Copy_On_Write => True);
   procedure Test_Wide is new Do_Test (Wide);

begin
   Test_No_COW("basic strings, no COW");
   Test_COW("basic strings, with COW");
   Test3("strings with size" & SSize_3'Last'Img);
   Test_Wide("wide strings");

   return A.Report;
end Test;
