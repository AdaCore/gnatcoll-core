------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

--  Add support for specific field types in databases

with GNATCOLL.SQL_Impl;    use GNATCOLL.SQL_Impl;
with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Exec;    use GNATCOLL.SQL.Exec;

package GNATCOLL.SQL_Fields is

   -----------------
   -- JSON fields --
   -----------------

   function Json_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String;

   type SQL_Parameter_Json is new SQL_Parameter_Text with null record;
   overriding function Type_String
     (Self   : SQL_Parameter_Json;
      Index  : Positive;
      Format : Formatter'Class) return String
     is (Format.Parameter_String (Index, "json"));
   overriding function Image
     (Self   : SQL_Parameter_Json;
      Format : Formatter'Class) return String
     is (Json_To_SQL (Format, To_String (Self), Quote => False));

   type Field_Type_Json is new Field_Type with null record;
   overriding function Type_To_SQL
     (Self         : Field_Type_Json;
      Format       : access Formatter'Class := null;
      For_Database : Boolean := True) return String
     is (if For_Database
         then "Json"
         else "GNATCOLL.SQL_Fields.SQL_Field_Json");
   overriding function Type_From_SQL
     (Self : in out Field_Type_Json; Str : String) return Boolean
     is (Str = "json");
   overriding function Parameter_Type
     (Self : Field_Type_Json) return SQL_Parameter_Type'Class
     is (SQL_Parameter_Json'(others => <>));

   package Json_Fields is new Field_Types
     (String, Json_To_SQL, SQL_Parameter_Json);
   type SQL_Field_Json is new Json_Fields.Field with null record;
   Null_Field_Json : constant SQL_Field_Json :=
     (Json_Fields.Null_Field with null record);
   function Json_Param (Index : Positive) return Json_Fields.Field'Class
     renames Json_Fields.Param;

   function Json_Text_Value
     (Self  : Forward_Cursor'Class; Field : Field_Index) return String
     is (Self.Value (Field));

   ----------------
   -- XML fields --
   ----------------

   function XML_To_SQL
     (Self : Formatter'Class; Value : String; Quote : Boolean) return String;

   type SQL_Parameter_XML is new SQL_Parameter_Text with null record;
   overriding function Type_String
     (Self   : SQL_Parameter_XML;
      Index  : Positive;
      Format : Formatter'Class) return String
     is (Format.Parameter_String (Index, "xml"));
   overriding function Image
     (Self   : SQL_Parameter_XML;
      Format : Formatter'Class) return String
     is (XML_To_SQL (Format, To_String (Self), Quote => False));

   type Field_Type_XML is new Field_Type with null record;
   overriding function Type_To_SQL
     (Self         : Field_Type_XML;
      Format       : access Formatter'Class := null;
      For_Database : Boolean := True) return String
     is (if For_Database
         then "XML"
         else "GNATCOLL.SQL_Fields.SQL_Field_XML");
   overriding function Type_From_SQL
     (Self : in out Field_Type_XML; Str : String) return Boolean
     is (Str = "xml");
   overriding function Parameter_Type
     (Self : Field_Type_XML) return SQL_Parameter_Type'Class
     is (SQL_Parameter_XML'(others => <>));

   package XML_Fields is new Field_Types
     (String, XML_To_SQL, SQL_Parameter_XML);
   type SQL_Field_XML is new XML_Fields.Field with null record;
   Null_Field_XML : constant SQL_Field_XML :=
     (XML_Fields.Null_Field with null record);
   function XML_Param (Index : Positive) return XML_Fields.Field'Class
                       renames XML_Fields.Param;

   function XML_Text_Value
     (Self  : Forward_Cursor'Class; Field : Field_Index) return String
     is (Self.Value (Field));

end GNATCOLL.SQL_Fields;
