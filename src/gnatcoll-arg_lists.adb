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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ada.Unchecked_Deallocation;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Maps;        use Ada.Strings.Maps;

with GNATCOLL.Scripts.Utils; use GNATCOLL.Scripts.Utils;

package body GNATCOLL.Arg_Lists is

   procedure Parse_Command_Line_String
     (CL   : in out Arg_List;
      Text : String);
   --  Factor code between variants of Parse_String.
   --  This processes Text as if it were passed on a command line (for instance
   --  the bash command line) and adds the arguments to CL.

   function Escape_Backslashes (A : Unbounded_String) return Unbounded_String;
   --  Escape backslashes in A

   ------------------------
   -- Escape_Backslashes --
   ------------------------

   function Escape_Backslashes
     (A : Unbounded_String) return Unbounded_String is
      S : constant String := To_String (A);
      R : Unbounded_String;
   begin
      for J in S'Range loop
         case S (J) is
            when '\' =>
               Append (R, "\\");
            when others =>
               Append (R, S (J));
         end case;
      end loop;
      return R;
   end Escape_Backslashes;

   -------------------------------
   -- Parse_Command_Line_String --
   -------------------------------

   procedure Parse_Command_Line_String
     (CL   : in out Arg_List;
      Text : String)
   is
      function Process (A : String) return Argument_Type;
      --  Post-process on each argument returned by Argument_String_To_List

      -------------
      -- Process --
      -------------

      function Process (A : String) return Argument_Type is
      begin
         if A = "" then
            return (One_Arg, Null_Unbounded_String);
         end if;

         --  Argument_String_To_List does not remove single quotes around an
         --  argument: do this now.
         if A (A'First) = '"' and then A (A'Last) = '"' then
            return (One_Arg,
                    To_Unbounded_String (A (A'First + 1 .. A'Last - 1)));
         end if;

         return (Expandable, To_Unbounded_String (A));
      end Process;

      Local_Args : Argument_List_Access;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);
   begin

      --  If we are parsing an argument in Separate_Args mode, get rid of the
      --  leading spaces, as this would result in multiple arguments in
      --  the call to Argument_String_To_List_With_Triple_Quotes
      --  Also remove trailing spaces, since otherwise the last argument on
      --  the command line, when surrounded with quotes, will be seen by
      --  Process as ending with ASCII.LF, and therefore the quotes will not be
      --  removed.

      if CL.Mode = Separate_Args then
         Local_Args := Argument_String_To_List_With_Triple_Quotes
           (Trim
              (Text,
               Left  => To_Set (' ' & ASCII.LF & ASCII.HT),
               Right => To_Set (' ' & ASCII.LF & ASCII.HT)));
      else
         Local_Args := Argument_String_To_List_With_Triple_Quotes (Text);
      end if;

      if Local_Args = null then
         return;
      end if;

      for J in Local_Args'Range loop
         CL.V.Append (Process (Local_Args (J).all));
         Free (Local_Args (J));
      end loop;

      Unchecked_Free (Local_Args);
   end Parse_Command_Line_String;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (Text : String;
      Mode : Command_Line_Mode) return Arg_List
   is
      CL : Arg_List;

   begin
      CL.Mode := Mode;

      if Mode = Separate_Args then
         Parse_Command_Line_String (CL, Text);
      else
         CL.V.Append ((One_Arg, To_Unbounded_String (Text)));
      end if;

      return CL;
   end Parse_String;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (Command : String; Text : String) return Arg_List
   is
      CL : Arg_List := Create (Command);
   begin
      Parse_Command_Line_String (CL, Text);
      return CL;
   end Parse_String;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (C : Arg_List) return String is
   begin
      if C.V.Is_Empty then
         return "";
      else
         return To_String (C.V.Element (0).Text);
      end if;
   end Get_Command;

   ------------
   -- Create --
   ------------

   function Create (Command : String) return Arg_List is
      C : Arg_List;
   begin
      C.V.Append ((One_Arg, To_Unbounded_String (Command)));
      return C;
   end Create;

   ---------------------
   -- Append_Argument --
   ---------------------

   procedure Append_Argument
     (C        : in out Arg_List;
      Argument : String;
      Mode     : Argument_Mode) is
   begin
      C.V.Append ((Mode, To_Unbounded_String (Argument)));
   end Append_Argument;

   -------------
   -- To_List --
   -------------

   function To_List
     (C               : Arg_List;
      Include_Command : Boolean) return GNAT.OS_Lib.Argument_List
   is
      First : Natural;
   begin
      if Include_Command then
         First := 0;
      else
         First := 1;
      end if;

      declare
         L : GNAT.OS_Lib.Argument_List (1 .. Natural (C.V.Length) - First);
      begin
         for J in First .. Natural (C.V.Length) - 1 loop
            L (J + 1 - First) := new String'
              (To_String (C.V.Element (J).Text));
         end loop;
         return L;
      end;
   end To_List;

   -----------------------
   -- To_Display_String --
   -----------------------

   function To_Display_String
     (C               : Arg_List;
      Include_Command : Boolean := True;
      Max_Arg_Length  : Positive := Positive'Last) return String
   is
      Result : Unbounded_String := To_Unbounded_String ("");
      Start  : Natural := 1;
   begin
      if not Include_Command then
         Start := 2;
      end if;

      for Index in Start .. Natural (C.V.Length) loop
         declare
            Arg_Len : constant Natural :=
                        Length (C.V.Element (Index - 1).Text);
         begin
            if Arg_Len > Max_Arg_Length then
               Append
                 (Result,
                  Unbounded_Slice (C.V.Element (Index - 1).Text,
                    1, Max_Arg_Length - 1));
               Append (Result, "...");
            else
               Append (Result, C.V.Element (Index - 1).Text);
            end if;
         end;

         if Index < Natural (C.V.Length) then
            Append (Result, " ");
         end if;
      end loop;
      return To_String (Result);
   end To_Display_String;

   ---------------------
   -- To_Debug_String --
   ---------------------

   function To_Debug_String (C : Arg_List) return String is
      Result : Unbounded_String := To_Unbounded_String ("Command: ");
   begin
      Append (Result, C.V.Element (0).Text);

      for J in 1 .. Natural (C.V.Length) - 1 loop
         Append (Result, ASCII.LF & "Arg: " & C.V.Element (J).Text);
      end loop;
      return To_String (Result);
   end To_Debug_String;

   ----------------------
   -- To_Script_String --
   ----------------------

   function To_Script_String (C : Arg_List) return String is

      function Arg (A : Unbounded_String) return Unbounded_String;
      --  Auxiliary function to process one arg

      ---------
      -- Arg --
      ---------

      function Arg (A : Unbounded_String) return Unbounded_String is
         S : constant String := To_String (A);
         R : Unbounded_String;
      begin
         for J in S'Range loop
            case S (J) is
            when '\' =>
               Append (R, "\\");
            when ' ' =>
               Append (R, "\ ");
            when '"' =>
               Append (R, "\""");
            when others =>
               Append (R, S (J));
            end case;
         end loop;
         return R;
      end Arg;

      Result : Unbounded_String;
   begin
      if C = Empty_Command_Line then
         return "";
      end if;

      if C.Mode = Raw_String then
         return To_String (C.V.Element (0).Text);
      end if;

      --  Convert all arguments
      for J in 1 .. Natural (C.V.Length) loop
         Append (Result, Arg (C.V.Element (J - 1).Text));
         Append (Result, ' ');
      end loop;

      --  Return result without the trailing space
      declare
         R : constant String := To_String (Result);
      begin
         return R (R'First .. R'Last - 1);
      end;
   end To_Script_String;

   ----------------
   -- Substitute --
   ----------------

   procedure Substitute
     (CL       : in out Arg_List;
      Char     : Character;
      Callback : Substitution_Function)
   is
      New_CL  : Arg_List;

      function Expand_In_String (A : Unbounded_String) return Unbounded_String;
      --  Expand the argument in place in S and return the result

      ----------------------
      -- Expand_In_String --
      ----------------------

      function Expand_In_String
        (A : Unbounded_String) return Unbounded_String
      is
         S   : constant String := To_String (A);
         U   : Unbounded_String;
         J   : Natural;
         Beg : Natural;
         New_CL  : Arg_List;
         Skip_Ending_Bracket : Boolean := False;
      begin
         if S = "" then
            return Null_Unbounded_String;
         end if;

         J := S'First;
         while J <= S'Last loop
            if S (J) = Char then
               --  Skip to the next separator
               J := J + 1;
               Beg := J;

               if S (J) = '{' then
                  --  An '{' immediately follows the special character:
                  --  the parameter should be the whole string contained
                  --  between this and the ending '}'.
                  Skip_Ending_Bracket := True;
                  J := J + 1;
                  Beg := J;
                  while J <= S'Last
                    and then S (J) /= '}'
                  loop
                     J := J + 1;
                  end loop;
               else
                  Skip_Ending_Bracket := False;
                  while J <= S'Last
                    and then (Is_Alphanumeric (S (J))
                              or else S (J) = '*'
                              or else S (J) = '-'
                              or else S (J) = '@')

                  loop
                     J := J + 1;
                  end loop;
               end if;

               --  A doubling of the special character indicates that we
               --  are inserting it.
               if S (J - 1) = Char then
                  Append (U, Char);
                  J := J + 1;
               else
                  New_CL := Callback (S (Beg .. J - 1), Raw_String);

                  for K in 0 .. Natural (New_CL.V.Length) - 1 loop

                     if CL.Mode = Raw_String then
                        Append
                          (U,
                           Escape_Backslashes (New_CL.V.Element (K).Text));
                     else
                        Append (U, New_CL.V.Element (K).Text);
                     end if;

                     if K < Natural (New_CL.V.Length) - 1 then
                        Append (U, ' ');
                     end if;
                  end loop;
               end if;

               if Skip_Ending_Bracket then
                  J := J + 1;
               end if;
            else
               Append (U, S (J));
               J := J + 1;
            end if;
         end loop;

         return U;
      end Expand_In_String;

   begin
      New_CL.Mode := CL.Mode;

      if CL = Empty_Command_Line then
         return;
      end if;

      if Callback = null then
         return;
      end if;

      case CL.Mode is
         when Raw_String =>
            declare
               U : constant Unbounded_String := Expand_In_String
                 (CL.V.Element (0).Text);
            begin
               CL.V.Replace_Element (0, (One_Arg, U));
            end;

         when Separate_Args =>
            for J in 0 .. Natural (CL.V.Length) - 1 loop
               case CL.V.Element (J).Mode is
                  when One_Arg =>
                     declare
                        U : constant Unbounded_String := Expand_In_String
                          (CL.V.Element (J).Text);
                     begin
                        New_CL.V.Append ((One_Arg, U));
                     end;

                  when Expandable =>
                     declare
                        P : constant Unbounded_String := CL.V.Element (J).Text;
                     begin
                        if Element (P, 1) = Char then
                           New_CL.V.Append
                             (Callback
                                (Slice (P, 2, Length (P)), Separate_Args).V);
                        else
                           New_CL.V.Append
                             ((Expandable, Expand_In_String (P)));
                        end if;
                     end;
               end case;
            end loop;

            CL := New_CL;
      end case;
   end Substitute;

   -----------------
   -- Args_Length --
   -----------------

   function Args_Length (C : Arg_List) return Integer is
   begin
      return Natural (C.V.Length) - 1;
   end Args_Length;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (C : Arg_List; N : Natural) return String is
   begin
      return To_String (C.V.Element (N).Text);
   end Nth_Arg;

   function Nth_Arg (C : Arg_List; N : Natural) return Unbounded_String is
   begin
      return C.V.Element (N).Text;
   end Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg (C : in out Arg_List; N : Natural; Arg : String) is
   begin
      --  If there are not enough arguments, create them
      while N > Args_Length (C) loop
         C.V.Append ((One_Arg, Null_Unbounded_String));
      end loop;

      C.V.Replace_Element
        (N, (C.V.Element (N).Mode, To_Unbounded_String (Arg)));
   end Set_Nth_Arg;

   -----------------------------
   -- Argument_List_To_String --
   -----------------------------

   function Argument_List_To_String
     (List           : GNAT.Strings.String_List;
      Protect_Quotes : Boolean := True) return String
   is
      Length : Natural := 0;
   begin
      for L in List'Range loop
         Length := Length + List (L)'Length + 1;

         if Protect_Quotes then
            for S in List (L)'Range loop
               if List (L)(S) = '"'
                 or else List (L)(S) = ' '
                 or else List (L) (S) = '\'
                 or else List (L) (S) = '''
               then
                  Length := Length + 1;
               end if;
            end loop;
         end if;
      end loop;

      declare
         S     : String (1 .. Length);
         Index : Positive := S'First;
      begin
         for L in List'Range loop
            for J in List (L)'Range loop
               if Protect_Quotes then
                  if List (L) (J) = '"'
                    or else List (L) (J) = ' '
                    or else List (L) (J) = '\'
                    or else List (L) (J) = '''
                  then
                     S (Index) := '\';
                     Index := Index + 1;
                  end if;
               end if;
               S (Index) := List (L)(J);
               Index := Index + 1;
            end loop;
            S (Index) := ' ';
            Index := Index + 1;
         end loop;
         return S (1 .. S'Last - 1);  -- Ignore last space
      end;
   end Argument_List_To_String;

end GNATCOLL.Arg_Lists;
