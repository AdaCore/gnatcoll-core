------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2015, AdaCore                     --
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

pragma Ada_2012;

with Ada.Characters.Wide_Wide_Latin_1; use Ada.Characters.Wide_Wide_Latin_1;
with Interfaces;                       use Interfaces;

pragma Warnings (Off, "*internal GNAT unit*");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "*internal GNAT unit*");

with GNAT.Encode_UTF8_String;
with GNAT.Decode_UTF8_String;

package body GNATCOLL.JSON.Utility is

   use Ada.Strings.Unbounded;

   To_Hex : constant array (Unsigned_16 range 0 .. 15) of Character :=
     "0123456789ABCDEF";

   --------------------------------
   -- Escape_Non_Print_Character --
   --------------------------------

   function Escape_Non_Print_Character
     (C : Wide_Wide_Character) return String
   is
      Code  : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
      Buf   : String (1 .. 12);
      Last  : Natural := Buf'First - 1;

      procedure Append_Escaped (Code : Unsigned_16);

      --------------------
      -- Append_Escaped --
      --------------------

      procedure Append_Escaped (Code : Unsigned_16) is
      begin
         Last := Last + 6;
         Buf (Last - 5 .. Last - 4) := "\u";
         Buf (Last - 3) := To_Hex ((Code / 16#1000#) mod 16#10#);
         Buf (Last - 2) := To_Hex ((Code / 16#100#) mod 16#10#);
         Buf (Last - 1) := To_Hex ((Code / 16#10#) mod 16#10#);
         Buf (Last) := To_Hex (Code mod 16#10#);
      end Append_Escaped;

   begin
      if Code <= 16#FFFF# then
         Append_Escaped (Unsigned_16 (Code));

      else
         --  Represent character as surrogate pair

         Append_Escaped
           (16#D800# + Unsigned_16 ((Code - 16#1_0000#) / 16#400#));
         Append_Escaped (16#DC00# + Unsigned_16 (Code mod 16#400#));
      end if;

      return Buf (Buf'First .. Last);
   end Escape_Non_Print_Character;

   -------------------
   -- Escape_String --
   -------------------

   function Escape_String
     (Text : UTF8_Unbounded_String) return Unbounded_String
   is
      use Ada.Strings.Unbounded.Aux;

      Str         : Big_String_Access;
      Text_Length : Natural;
      Ret         : Unbounded_String;
      Low         : Natural;
      W_Chr       : Wide_Wide_Character;

   begin
      Get_String (Text, Str, Text_Length);

      Append (Ret, '"');
      Low := 1;

      while Low <= Text_Length loop
         --  UTF-8 sequence is maximum 4 characters long according to RFC3629

         begin
            GNAT.Decode_UTF8_String.Decode_Wide_Wide_Character
              (Str (Low .. Natural'Min (Text_Length, Low + 3)), Low, W_Chr);
         exception
            when Constraint_Error =>
               --  Skip the character even if it is invalid.
               Low := Low + 1;
               W_Chr := NUL;
         end;

         case W_Chr is
            when NUL =>
               null;
            when '"' =>
               Append (Ret, "\""");
            when '\' =>
               Append (Ret, "\\");
            when BS =>
               Append (Ret, "\b");
            when FF =>
               Append (Ret, "\f");
            when LF =>
               Append (Ret, "\n");
            when CR =>
               Append (Ret, "\r");
            when HT =>
               Append (Ret, "\t");
            when others =>
               if Wide_Wide_Character'Pos (W_Chr) >= 16#80# then
                  Append (Ret, Escape_Non_Print_Character (W_Chr));
               else
                  Append
                    (Ret,
                     "" & Character'Val (Wide_Wide_Character'Pos (W_Chr)));
               end if;
         end case;
      end loop;

      Append (Ret, '"');
      return Ret;
   end Escape_String;

   ----------------------
   -- Un_Escape_String --
   ----------------------

   function Un_Escape_String
     (Text : Unbounded_String;
      Low  : Natural;
      High : Natural) return UTF8_Unbounded_String
   is
      First : Integer;
      Last  : Integer;
      Unb   : Unbounded_String;
      Idx   : Natural;

   begin
      First := Low;
      Last  := High;

      --  Trim blanks and double quotes

      while First <= High and then Element (Text, First) = ' ' loop
         First := First + 1;
      end loop;
      if First <= High and then Element (Text, First) = '"' then
         First := First + 1;
      end if;

      while Last >= Low and then Element (Text, Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if Last >= Low and then Element (Text, Last) = '"' then
         Last := Last - 1;
      end if;

      Idx := First;
      while Idx <= Last loop
         if Element (Text, Idx) = '\' then
            Idx := Idx + 1;

            if Idx > High then
               raise Invalid_JSON_Stream with
                 "Unexpected escape character at end of line";
            end if;

            --  See http://tools.ietf.org/html/rfc4627 for the list of
            --  characters that can be escaped.

            case Element (Text, Idx) is
               when 'u' | 'U' =>
                  declare
                     Lead : constant Unsigned_16 :=
                       Unsigned_16'Value
                         ("16#" & Slice (Text, Idx + 1, Idx + 4) & "#");
                     Trail : Unsigned_16;
                     Char  : Wide_Wide_Character;

                  begin
                     Char := Wide_Wide_Character'Val (Lead);

                     --  If character is high surrogate and next character is
                     --  low surrogate then them represent one non-BMP
                     --  character.

                     if Lead in 16#D800# .. 16#DBFF#
                       and then Element (Text, Idx + 5) = '\'
                       and then Element (Text, Idx + 6) in 'u' | 'U'
                     then
                        Trail := Unsigned_16'Value
                          ("16#" & Slice (Text, Idx + 7, Idx + 10) & '#');
                        Char := Wide_Wide_Character'Val
                          (16#1_0000#
                           + Unsigned_32 (Lead and 16#03FF#) * 16#0400#
                           + Unsigned_32 (Trail and 16#03FF#));
                        Idx := Idx + 6;
                     end if;

                     Append
                       (Unb,
                        GNAT.Encode_UTF8_String.Encode_Wide_Wide_String
                          ((1 => Char)));
                     Idx := Idx + 4;
                  end;

               when '"' =>
                  Append (Unb, '"');
               when '/' =>
                  Append (Unb, '/');
               when '\' =>
                  Append (Unb, '\');
               when 'b' =>
                  Append (Unb, ASCII.BS);
               when 'f' =>
                  Append (Unb, ASCII.FF);
               when 'n' =>
                  Append (Unb, ASCII.LF);
               when 'r' =>
                  Append (Unb, ASCII.CR);
               when 't' =>
                  Append (Unb, ASCII.HT);
               when others =>
                  raise Invalid_JSON_Stream with
                    "Unexpected escape sequence '\" &
                    Element (Text, Idx) & "'";
            end case;

         else
            Append (Unb, Element (Text, Idx));
         end if;

         Idx := Idx + 1;
      end loop;

      return Unb;
   end Un_Escape_String;

end GNATCOLL.JSON.Utility;
