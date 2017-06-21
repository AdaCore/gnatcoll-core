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
with GNATCOLL.Strings;     use GNATCOLL.Strings;

package GNATCOLL.SQL_Fields is

   ----------------------
   -- Double precision --
   ----------------------
   --  maps sql types to Ada long_float. This includes
   --  "double precision", "float", "numeric", "numeric(position,scale>0)"

   function Long_Float_To_SQL is new Any_Float_To_SQL (Long_Long_Float);
   function Long_Float_From_SQL is new Any_Float_Value (Long_Long_Float);
   function Identity (Val : Long_Long_Float) return Long_Long_Float
      with Inline;
   function Maps_Long_Float
      (Schema : String; Value : out Null_Record) return Boolean;
   function Float_SQL_Type (Data : Null_Record) return String
      is ("double precision");
   package Long_Float_Fields is new Field_Types
      (Ada_Type          => Long_Long_Float,
       To_SQL            => Long_Float_To_SQL,
       From_SQL          => Long_Float_From_SQL,
       Stored_Ada_Type   => Long_Long_Float,
       Stored_To_Ada     => Identity,
       Ada_To_Stored     => Identity,
       Field_Data        => Null_Record,
       SQL_Type          => Float_SQL_Type,
       Ada_Field_Type    => "GNATCOLL.SQL_Fields.SQL_Field_Long_Float",
       Schema_Type_Check => Maps_Long_Float);
   subtype SQL_Parameter_Long_Float is Long_Float_Fields.Parameter;
   type SQL_Field_Long_Float is new Long_Float_Fields.Field with null record;
   Null_Field_Long_Float : constant SQL_Field_Long_Float;

   function Long_Float_Param (Index : Positive)
      return Long_Float_Fields.Field'Class
      renames Long_Float_Fields.Param;
   function Expression
     (Value : Long_Long_Float) return Long_Float_Fields.Field'Class
      renames Long_Float_Fields.Expression;

   function As_Long_Float (Value : Long_Long_Float) return SQL_Parameter
      renames Long_Float_Fields.As_Param;
   --  Set the value of a parameter in a SQL query.
   --  Used when executing the query.

   -----------------
   -- JSON fields --
   -----------------
   --  maps to "json" sql type, only supported by some DBMS systems.

   function Json_To_SQL
     (Self : Formatter'Class; Value : XString; Quote : Boolean) return String;
   function Json_From_SQL
      (Self : Formatter'Class; Value : String) return String is (Value);
   function Maps_JSON (Schema : String; Value : out Null_Record) return Boolean
      is (Schema = "json");
   function JSON_SQL_Type (Data : Null_Record) return String
      is ("json");
   package Json_Fields is new Field_Types
      (Ada_Type          => String,
       To_SQL            => Json_To_SQL,
       From_SQL          => Json_From_SQL,
       Stored_Ada_Type   => XString,
       Stored_To_Ada     => GNATCOLL.Strings.To_String,
       Ada_To_Stored     => GNATCOLL.Strings.To_XString,
       SQL_Type          => JSON_SQL_Type,
       Field_Data        => Null_Record,
       Ada_Field_Type    => "GNATCOLL.SQL_Fields.SQL_Field_Json",
       Schema_Type_Check => Maps_JSON);
   subtype SQL_Parameter_Json is Json_Fields.Parameter;
   type SQL_Field_Json is new Json_Fields.Field with null record;
   Null_Field_Json : constant SQL_Field_Json;

   function Json_Param (Index : Positive) return Json_Fields.Field'Class
     renames Json_Fields.Param;

   function Json_Text_Value
     (Self  : Forward_Cursor'Class; Field : Field_Index) return String
     is (Self.Value (Field));

   ----------------
   -- XML fields --
   ----------------
   --  maps to the sql "xml" type, only supported by some DBMS systems.

   function XML_To_SQL
     (Self : Formatter'Class; Value : XString; Quote : Boolean) return String;
   function XML_From_SQL
      (Self : Formatter'Class; Value : String) return String is (Value);
   function Maps_XML (Schema : String; Value : out Null_Record) return Boolean
      is (Schema = "xml");
   function XML_SQL_Type (Data : Null_Record) return String is ("xml");
   package XML_Fields is new Field_Types
      (Ada_Type          => String,
       To_SQL            => XML_To_SQL,
       From_SQL          => XML_From_SQL,
       Stored_Ada_Type   => XString,
       Stored_To_Ada     => GNATCOLL.Strings.To_String,
       Ada_To_Stored     => GNATCOLL.Strings.To_XString,
       SQL_Type          => XML_SQL_Type,
       Field_Data        => Null_Record,
       Ada_Field_Type    => "GNATCOLL.SQL_Fields.SQL_Field_XML",
       Schema_Type_Check => Maps_XML);
   subtype SQL_Parameter_XML is XML_Fields.Parameter;
   type SQL_Field_XML is new XML_Fields.Field with null record;
   Null_Field_XML : constant SQL_Field_XML;

   function XML_Param (Index : Positive) return XML_Fields.Field'Class
                       renames XML_Fields.Param;
   function XML_Text_Value
     (Self  : Forward_Cursor'Class; Field : Field_Index) return String
     is (Self.Value (Field));

private
   Null_Field_Long_Float : constant SQL_Field_Long_Float :=
       (Long_Float_Fields.Null_Field with null record);
   Null_Field_Json : constant SQL_Field_Json :=
      (Json_Fields.Null_Field with null record);
   Null_Field_XML : constant SQL_Field_XML :=
      (XML_Fields.Null_Field with null record);
end GNATCOLL.SQL_Fields;
