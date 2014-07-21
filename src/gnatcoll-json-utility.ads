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

private package GNATCOLL.JSON.Utility is

   JsonMimeType           : constant String := "application/json";

   function Escape_Non_Print_Character (C : Wide_Wide_Character) return String;

   function Escape_String
     (Text : UTF8_Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Translates an UTF-8 encoded unbounded string into a JSON-escaped string

   function Un_Escape_String
     (Text : Ada.Strings.Unbounded.Unbounded_String;
      Low  : Natural;
      High : Natural)
      return UTF8_Unbounded_String;
   --  Translates a JSON-escaped string into an UTF-8 encoded unbounded string
   --  Low represents the lower bound of the JSON string in Text
   --  High represents the higher bound of the JSON string in Text

end GNATCOLL.JSON.Utility;
