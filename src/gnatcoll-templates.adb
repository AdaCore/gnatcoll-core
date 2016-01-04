------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.Strings;            use GNAT.Strings;

package body GNATCOLL.Templates is

   procedure Find_Identifier
     (Str         : String;
      Delimiter   : Character;
      First       : in out Integer;
      Last        : out Integer;
      First_After : out Integer);
   --  Set Last to the last character of the identifier name.
   --  First should point to the first candidate character, but could be
   --  moved forward if it points to a curly brace.

   ----------
   -- Free --
   ----------

   procedure Free (Substrings : in out Substitution_Array) is
   begin
      for S in Substrings'Range loop
         Free (Substrings (S).Name);
         Free (Substrings (S).Value);
      end loop;
   end Free;

   ---------------------
   -- Find_Identifier --
   ---------------------

   procedure Find_Identifier
     (Str         : String;
      Delimiter   : Character;
      First       : in out Integer;
      Last        : out Integer;
      First_After : out Integer)
   is
   begin
      if Str (First) = Delimiter then
         --  We are escaping the delimiter by doubling it

         Last := First;
         First_After := First + 1;

      elsif Str (First) = '{' then
         First := First + 1;
         Last := First;
         while Last <= Str'Last and then Str (Last) /= '}' loop
            Last := Last + 1;
         end loop;
         First_After := Last + 1;
         Last        := Last - 1;

      elsif Str (First) = '(' then
         First := First + 1;
         Last := First;
         while Last <= Str'Last and then Str (Last) /= ')' loop
            Last := Last + 1;
         end loop;
         First_After := Last + 1;
         Last        := Last - 1;

      elsif Is_Digit (Str (First)) then
         Last := First + 1;
         while Last <= Str'Last
           and then Is_Digit (Str (Last))
         loop
            Last := Last + 1;
         end loop;

         if Last <= Str'Last
           and then Str (Last) = '-'
         then
            Last := Last + 1;
         end if;

         First_After := Last;
         Last        := Last - 1;

      elsif Is_Alphanumeric (Str (First)) then
         Last := First + 1;
         while Last <= Str'Last
           and then (Is_Alphanumeric (Str (Last)) or else Str (Last) = '_')
         loop
            Last := Last + 1;
         end loop;

         First_After := Last;
         Last        := Last - 1;

      else
         Last := First;
         First_After := Last + 1;
      end if;
   end Find_Identifier;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Str        : String;
      Substrings : Substitution_Array := No_Substitution;
      Callback   : Substitute_Callback := null;
      Delimiter  : Character := Default_Delimiter;
      Recursive  : Boolean := False;
      Errors     : Error_Handling := Keep_As_Is) return String
   is
      Result      : Unbounded_String;
      First, Last : Natural := Str'First;
      Found       : Boolean;
      Identifier_First, Identifier_Last, First_After : Natural;
      Quoted      : Boolean := False;

   begin
      while First <= Str'Last loop
         Last := First;

         --  Skip constant substrings

         while Last <= Str'Last and then Str (Last) /= Delimiter loop
            if Str (Last) = '"' then
               Quoted := not Quoted;
            end if;

            Last := Last + 1;
         end loop;

         if Last = Str'Last then
            Last := Last + 1;
         end if;

         Append (Result, Str (First .. Last - 1));

         exit when Last > Str'Last;

         --  Find name of identifier

         First            := Last + 1;
         Identifier_First := First;
         Find_Identifier (Str, Delimiter, Identifier_First, Last, First_After);

         --  Does the identifier contain a default value?

         Identifier_Last := Last;

         for D in Identifier_First .. Identifier_Last loop
            if Str (D) = ':' and then Str (D + 1) = '-' then
               Identifier_Last := D - 1;
               exit;
            end if;
         end loop;

         Found := False;

         for S in Substrings'Range loop
            if Substrings (S).Name.all =
              Str (Identifier_First .. Identifier_Last)
            then
               if Recursive then
                  Append
                    (Result, Substitute
                       (Str        => Substrings (S).Value.all,
                        Substrings => Substrings,
                        Callback   => Callback,
                        Delimiter  => Delimiter,
                        Recursive  => Recursive));
               else
                  Append (Result, Substrings (S).Value.all);
               end if;

               Found := True;
               exit;
            end if;
         end loop;

         --  When doubled, the delimiter is always replaced with itself by
         --  default.

         if not Found
           and then Identifier_Last = Identifier_First
           and then Str (Identifier_First) = Delimiter
         then
            --  We are escaping the Substitution_Char by doubling it

            Append (Result, Delimiter);
            Found := True;

         elsif not Found and then Callback /= null then
            begin
               declare
                  Sub : constant String := Callback
                    (Str (Identifier_First .. Identifier_Last), Quoted);
               begin
                  if Recursive then
                     Append
                       (Result, Substitute
                          (Str        => Sub,
                           Substrings => Substrings,
                           Callback   => Callback,
                           Delimiter  => Delimiter,
                           Recursive  => Recursive));
                  else
                     Append (Result, Sub);
                  end if;

                  Found := True;
               end;
            exception
               when Invalid_Substitution =>
                  Found := False;
            end;
         end if;

         --  If still not found, try the default value if it was specified

         if not Found
           and then Identifier_Last < Last
         then
            Append (Result, Str (Identifier_Last + 3 .. Last));
            Found := True;
         end if;

         if not Found then
            case Errors is
               when Keep_As_Is =>
                  Append (Result, Str (First - 1 .. First_After - 1));

               when Replace_With_Empty =>
                  null;

               when Report_Error =>
                  raise Invalid_Substitution;
            end case;
         end if;

         First := First_After;
      end loop;

      return To_String (Result);
   end Substitute;

end GNATCOLL.Templates;
