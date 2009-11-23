-----------------------------------------------------------------------
--                           G N A T C O L L                         --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ada.Unchecked_Deallocation;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Maps;        use Ada.Strings.Maps;

with GNATCOLL.Scripts.Utils; use GNATCOLL.Scripts.Utils;

package body GNATCOLL.Command_Lines is

   procedure Parse_Command_Line_String
     (CL   : in out Command_Line;
      Text : String);
   --  Factor code between variants of Parse_String.
   --  This processes Text as if it were passed on a command line (for instance
   --  the bash command line) and adds the arguments to CL.

   -------------------------------
   -- Parse_Command_Line_String --
   -------------------------------

   procedure Parse_Command_Line_String
     (CL   : in out Command_Line;
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

      if CL.Mode = Separate_Args then
         Local_Args := Argument_String_To_List_With_Triple_Quotes
           (Trim
              (Text,
               Left  => To_Set (' ' & ASCII.LF & ASCII.HT),
               Right => Ada.Strings.Maps.Null_Set));
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
      Mode : Command_Line_Mode) return Command_Line
   is
      CL : Command_Line;

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
     (Command : String; Text : String) return Command_Line
   is
      CL : Command_Line := Create (Command);
   begin
      Parse_Command_Line_String (CL, Text);
      return CL;
   end Parse_String;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (C : Command_Line) return String is
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

   function Create (Command : String) return Command_Line is
      C : Command_Line;
   begin
      C.V.Append ((One_Arg, To_Unbounded_String (Command)));
      return C;
   end Create;

   ---------------------
   -- Append_Argument --
   ---------------------

   procedure Append_Argument
     (C        : in out Command_Line;
      Argument : String;
      Mode     : Argument_Mode) is
   begin
      C.V.Append ((Mode, To_Unbounded_String (Argument)));
   end Append_Argument;

   -------------
   -- To_List --
   -------------

   function To_List
     (C               : Command_Line;
      Include_Command : Boolean)
      return GNAT.OS_Lib.Argument_List
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

   function To_Display_String (C : Command_Line) return String is
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      for Index in 1 .. Natural (C.V.Length) loop
         Append (Result, C.V.Element (Index - 1).Text);
         if Index < Natural (C.V.Length) then
            Append (Result, " ");
         end if;
      end loop;
      return To_String (Result);
   end To_Display_String;

   ---------------------
   -- To_Debug_String --
   ---------------------

   function To_Debug_String (C : Command_Line) return String is
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

   function To_Script_String (C : Command_Line) return String is
      function Arg (A : Unbounded_String) return Unbounded_String;
      --  Auxiliary function to process one arg

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
     (CL       : in out Command_Line;
      Char     : Character;
      Callback : Substitution_Function)
   is
      New_CL  : Command_Line;

      function Expand_In_String (A : Unbounded_String) return Unbounded_String;
      --  Expand the argument in place in S and return the result

      function Expand_In_String
        (A : Unbounded_String) return Unbounded_String
      is
         S   : constant String := To_String (A);
         U   : Unbounded_String;
         J   : Natural;
         Beg : Natural;
         New_CL  : Command_Line;
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
               while J <= S'Last
                 and then (Is_Alphanumeric (S (J))
                           or else S (J) = '*'
                           or else S (J) = '-'
                           or else S (J) = '@')

               loop
                  J := J + 1;
               end loop;

               --  A doubling of the special character indicates that we
               --  are inserting it.
               if S (J - 1) = Char then
                  Append (U, Char);
                  J := J + 1;
               else
                  New_CL := Callback (S (Beg .. J - 1), CL.Mode);
                  for K in 0 .. Natural (New_CL.V.Length) - 1 loop
                     Append (U, New_CL.V.Element (K).Text);
                     if K < Natural (New_CL.V.Length) - 1 then
                        Append (U, ' ');
                     end if;
                  end loop;
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

   function Args_Length (C : Command_Line) return Integer is
   begin
      return Natural (C.V.Length) - 1;
   end Args_Length;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (C : Command_Line; N : Natural) return String is
   begin
      return To_String (C.V.Element (N).Text);
   end Nth_Arg;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg (C : in out Command_Line; N : Positive; Arg : String)
   is
   begin
      --  If there are not enough arguments, create them
      while N > Args_Length (C) loop
         C.V.Append ((One_Arg, Null_Unbounded_String));
      end loop;

      C.V.Replace_Element
        (N, (C.V.Element (N).Mode, To_Unbounded_String (Arg)));
   end Set_Nth_Arg;

end GNATCOLL.Command_Lines;
