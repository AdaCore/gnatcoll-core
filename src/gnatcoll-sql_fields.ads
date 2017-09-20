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

   ----------------------
   -- Double precision --
   ----------------------

   function Long_Float_To_SQL is new Any_Float_To_SQL (Long_Long_Float);

   package Long_Float_Parameters is new Scalar_Parameters
      (Long_Long_Float, "double precision", Long_Float_To_SQL);
   subtype SQL_Parameter_Long_Float is Long_Float_Parameters.SQL_Parameter;

   package Long_Float_Field_Mappings is new Simple_Field_Mappings
      ("double precision",
       "GNATCOLL.SQL_Fields.SQL_Field_Long_Float",
       SQL_Parameter_Long_Float);

   package Long_Float_Fields is new Field_Types
     (Long_Long_Float, Long_Float_To_SQL, SQL_Parameter_Long_Float);

   type SQL_Field_Long_Float is new Long_Float_Fields.Field with null record;
   Null_Field_Long_Float : constant SQL_Field_Long_Float :=
     (Long_Float_Fields.Null_Field with null record);
   function Long_Float_Param (Index : Positive)
      return Long_Float_Fields.Field'Class
      renames Long_Float_Fields.Param;

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
   overriding function Internal_Image
     (Self   : SQL_Parameter_Json;
      Format : Formatter'Class) return String
     is (Json_To_SQL (Format, To_String (Self), Quote => False));

   package JSON_Field_Mappings is new Simple_Field_Mappings
      ("json",
       "GNATCOLL.SQL_Fields.SQL_Field_Json",
       SQL_Parameter_Json);

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
   overriding function Internal_Image
     (Self   : SQL_Parameter_XML;
      Format : Formatter'Class) return String
     is (XML_To_SQL (Format, To_String (Self), Quote => False));

   package XML_Field_Mappings is new Simple_Field_Mappings
      ("xml",
       "GNATCOLL.SQL_Fields.SQL_Field_XML",
       SQL_Parameter_XML);

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
