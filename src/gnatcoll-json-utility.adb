------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2014, AdaCore                     --
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

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;  use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;

pragma Warnings (Off, "*internal GNAT unit*");
with System.Unsigned_Types;       use System.Unsigned_Types;
pragma Warnings (On, "*internal GNAT unit*");

with GNAT.Encode_UTF8_String;
with GNAT.Decode_UTF8_String;

package body GNATCOLL.JSON.Utility is

   --------------------------------
   -- Escape_Non_Print_Character --
   --------------------------------

   function Escape_Non_Print_Character (C : Wide_Character) return String
   is
      Int : constant Integer := Wide_Character'Pos (C);
      Str : String (1 .. 8);
      First, Last : Natural;

   begin
      Ada.Integer_Text_IO.Put (Str, Int, 16);
      First := Ada.Strings.Fixed.Index (Str, "16#") + 3;
      Last := Ada.Strings.Fixed.Index (Str, "#", Ada.Strings.Backward) - 1;

      --  Make sure we have 4 characters, prefixed with '0's
      Str (Last - 3 .. First - 1) := (others => '0');
      First := Last - 3;

      return "\u" & Str (First .. Last);
   end Escape_Non_Print_Character;

   -------------------
   -- Escape_String --
   -------------------

   function Escape_String
     (Text : UTF8_Unbounded_String) return Unbounded_String
   is
      Ret         : Unbounded_String;
      WS          : Unbounded_Wide_String;
      Low         : Natural;
      High        : Natural;
      Text_Length : constant Natural := Ada.Strings.Unbounded.Length (Text);
      W_Chr       : Wide_Character;

   begin
      --  First decode the UTF-8 String
      Low := 1;
      while Low <= Text_Length loop
         --  UTF-8 sequence is maximum 4 characters long according to RFC3629
         if Low + 4 > Text_Length then
            High := Text_Length;
         else
            High := Low + 4;
         end if;

         declare
            Slice : constant String :=
                      Ada.Strings.Unbounded.Slice (Text, Low, High);
            Idx   : Natural := Slice'First;

         begin
            GNAT.Decode_UTF8_String.Decode_Wide_Character (Slice, Idx, W_Chr);
            Append (WS, W_Chr);
            Low := Low + Idx - Slice'First;
         end;
      end loop;

      Append (Ret, '"');

      for J in 1 .. Length (WS) loop
         W_Chr := Element (WS, J);

         case W_Chr is
            when '"' =>
               Append (Ret, "\""");
            when '\' =>
               Append (Ret, "\\");
            when Wide_Character'Val (Character'Pos (ASCII.BS)) =>
               Append (Ret, "\b");
            when Wide_Character'Val (Character'Pos (ASCII.FF)) =>
               Append (Ret, "\f");
            when Wide_Character'Val (Character'Pos (ASCII.LF)) =>
               Append (Ret, "\n");
            when Wide_Character'Val (Character'Pos (ASCII.CR)) =>
               Append (Ret, "\r");
            when Wide_Character'Val (Character'Pos (ASCII.HT)) =>
               Append (Ret, "\t");
            when others =>
               if Wide_Character'Pos (W_Chr) > 128 then
                  Append (Ret, Escape_Non_Print_Character (W_Chr));
               else
                  Append
                    (Ret, "" & Character'Val (Wide_Character'Pos (W_Chr)));
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

      --  Trim blancks and the double quotes

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
                     I : constant Short_Unsigned :=
                           Short_Unsigned'Value
                             ("16#" & Slice (Text, Idx + 1, Idx + 4) & "#");
                     function Unch is new Ada.Unchecked_Conversion
                       (Short_Unsigned, Wide_Character);
                  begin
                     Append
                       (Unb,
                        GNAT.Encode_UTF8_String.Encode_Wide_String
                          ("" & Unch (I)));
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
            Append
              (Unb, Element (Text, Idx));
         end if;

         Idx := Idx + 1;
      end loop;

      return Unb;
   end Un_Escape_String;

end GNATCOLL.JSON.Utility;
