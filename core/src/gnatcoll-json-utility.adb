------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2020, AdaCore                     --
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

with Interfaces;                       use Interfaces;

with GNAT.Encode_UTF8_String;

with GNATCOLL.JSON.Serializers;
with GNATCOLL.Strings;
with GNATCOLL.Strings_Impl;

package body GNATCOLL.JSON.Utility is

   use Ada.Strings.Unbounded;

   -------------------
   -- Escape_String --
   -------------------

   function Escape_String (Text : UTF8_XString) return Unbounded_String is

      type Encoder is new Serializers.Serializer with record
         Ret : Unbounded_String;
      end record;

      overriding procedure Write (Self : in out Encoder; Text : String);

      overriding procedure Write (Self : in out Encoder; Text : String) is
      begin
         Append (Self.Ret, Text);
      end Write;

      Enc : Encoder;
   begin
      Enc.Encode_String (Text);
      Enc.Close;
      return Enc.Ret;
   end Escape_String;

   ----------------------
   -- Un_Escape_String --
   ----------------------

   function Un_Escape_String
     (Text : String;
      Low  : Natural;
      High : Natural) return UTF8_XString
   is
      First : Integer;
      Last  : Integer;
      Unb   : UTF8_XString;
      Idx   : Natural;

   begin
      First := Low;
      Last  := High;

      --  Trim blanks and double quotes

      while First <= High and then Text (First) = ' ' loop
         First := First + 1;
      end loop;
      if First <= High and then Text (First) = '"' then
         First := First + 1;
      end if;

      while Last >= Low and then Text (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if Last >= Low and then Text (Last) = '"' then
         Last := Last - 1;
      end if;

      --  Every escape shrinks, so the span is an upper bound on the result

      if Last >= First then
         Unb.Reserve (GNATCOLL.Strings_Impl.String_Size (Last - First + 1));
      end if;

      Idx := First;
      while Idx <= Last loop
         if Text (Idx) = '\' then
            Idx := Idx + 1;

            if Idx > High then
               raise Invalid_JSON_Stream with
                 "Unexpected escape character at end of line";
            end if;

            --  See http://tools.ietf.org/html/rfc4627 for the list of
            --  characters that can be escaped.

            case Text (Idx) is
               when 'u' | 'U' =>
                  declare
                     Lead : constant Unsigned_16 :=
                       Unsigned_16'Value
                         ("16#" & Text (Idx + 1 .. Idx + 4) & "#");
                     Trail : Unsigned_16;
                     Char  : Wide_Wide_Character;

                  begin
                     Char := Wide_Wide_Character'Val (Lead);

                     --  If character is high surrogate and next character is
                     --  low surrogate then them represent one non-BMP
                     --  character.

                     if Lead in 16#D800# .. 16#DBFF#
                       and then Text (Idx + 5) = '\'
                       and then Text (Idx + 6) in 'u' | 'U'
                     then
                        Trail := Unsigned_16'Value
                          ("16#" & Text (Idx + 7 .. Idx + 10) & '#');
                        Char := Wide_Wide_Character'Val
                          (16#1_0000#
                           + Unsigned_32 (Lead and 16#03FF#) * 16#0400#
                           + Unsigned_32 (Trail and 16#03FF#));
                        Idx := Idx + 6;
                     end if;

                     Unb.Append
                       (GNAT.Encode_UTF8_String.Encode_Wide_Wide_String
                          ((1 => Char)));
                     Idx := Idx + 4;
                  end;

               when '"' =>
                  Unb.Append ('"');
               when '/' =>
                  Unb.Append ('/');
               when '\' =>
                  Unb.Append ('\');
               when 'b' =>
                  Unb.Append (ASCII.BS);
               when 'f' =>
                  Unb.Append (ASCII.FF);
               when 'n' =>
                  Unb.Append (ASCII.LF);
               when 'r' =>
                  Unb.Append (ASCII.CR);
               when 't' =>
                  Unb.Append (ASCII.HT);
               when others =>
                  raise Invalid_JSON_Stream with
                    "Unexpected escape sequence '\" &
                    Text (Idx) & "'";
            end case;

         else
            --  Append the run at once: one character at a time costs a
            --  copy-on-write check per character

            declare
               Run_First : constant Natural := Idx;
            begin
               while Idx < Last and then Text (Idx + 1) /= '\' loop
                  Idx := Idx + 1;
               end loop;

               Unb.Append (Text (Run_First .. Idx));
            end;
         end if;

         Idx := Idx + 1;
      end loop;

      return Unb;
   end Un_Escape_String;

end GNATCOLL.JSON.Utility;
