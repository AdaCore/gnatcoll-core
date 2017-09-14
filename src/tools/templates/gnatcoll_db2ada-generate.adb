------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with AWS.Templates;              use AWS.Templates;

separate (Gnatcoll_Db2Ada)
procedure Generate (Spec_File, Body_File : File_Type) is
   T                 : Translate_Set;
   C                 : Tables_Maps.Cursor;
   A                 : Attribute_Lists.Cursor;
   T_Descr           : Table_Description;
   Table_Names       : Tag;
   Descriptions      : Tag;
   Fields            : Tag;
   Fields_Descr      : Tag;
   Types             : Tag;
   Foreign_Attrs, Referenced_Table : Tag;
   Referenced_Tables, Referenced_Attrs : Tag;

begin
   --  Process enumerations

   declare
      C     : Enumeration_Lists.Cursor := First (Enumerations);
      Ids, Names, Base_Types, Types, Local_Id, Local_Name   : Tag;
      Enum      : Dumped_Enums;
      C2, C3    : String_Lists.Cursor;
   begin
      while Has_Element (C) loop
         Enum := Element (C);
         Append (Types,      Enum.Type_Name);
         Append (Base_Types, Enum.Base_Type);

         C2 := First (Enum.Names);
         C3 := First (Enum.Values);
         Clear (Local_Name);
         Clear (Local_Id);
         while Has_Element (C2) loop
            Append (Local_Name, Element (C2));
            Append (Local_Id,   Element (C3));
            Next (C2);
            Next (C3);
         end loop;

         Append (Ids,   Local_Id);
         Append (Names, Local_Name);
         Next (C);
      end loop;

      Insert (T, Assoc ("TYPE_NAMES",   Types));
      Insert (T, Assoc ("TYPES",        Base_Types));
      Insert (T, Assoc ("NAMES",        Names));
      Insert (T, Assoc ("IDS",          Ids));
   end;

   --  Process variables

   declare
      C4 : Variables_List.Cursor := First (Variables);
      Var_Name, Var_Value, Var_Comment : Tag;
   begin

      while Has_Element (C4) loop
         Append (Var_Name, Element (C4).Name);
         Append (Var_Value, Element (C4).Value);
         Append (Var_Comment, Element (C4).Comment);
         Next (C4);
      end loop;

      Insert (T, Assoc ("VAR_NAMES",    Var_Name));
      Insert (T, Assoc ("VAR_VALUES",   Var_Value));
      Insert (T, Assoc ("VAR_COMMENTS", Var_Comment));
   end;

   --  Process tables

   C := First (Tables);
   while Has_Element (C) loop
      T_Descr      := Element (C);

      declare
         T_Fields      : Tag;
         T_Types       : Tag;
         T_Field_Descr : Tag;
      begin
         A := First (T_Descr.Attributes);
         while Has_Element (A) loop
            T_Fields      := T_Fields      & Element (A).Name;
            T_Field_Descr := T_Field_Descr & Element (A).Description;
            T_Types       := T_Types       & Element (A).Ada_Type;
            Next (A);
         end loop;

         Table_Names  := Table_Names  & Key (C);
         Descriptions := Descriptions & T_Descr.Description;
         Fields       := Fields       & T_Fields;
         Types        := Types        & T_Types;
         Fields_Descr := Fields_Descr & T_Field_Descr;

         declare
            Local_Ref_Table, Local_Ref_Attrs, Local_Attrs : Tag;
            K  : Foreign_Keys.Cursor;

            procedure On_FK (FK : Foreign_Key_Description) is
               S  : String_Lists.Cursor;
            begin
               Append (Local_Ref_Table, FK.To_Table);
               S := First (FK.From_Attributes);
               while Has_Element (S) loop
                  Append (Local_Attrs, Element (S));
                  Next (S);
               end loop;

               S := First (FK.To_Attributes);
               while Has_Element (S) loop
                  Append (Local_Ref_Attrs, Element (S));
                  Next (S);
               end loop;
            end On_FK;

         begin
            K := First (T_Descr.Foreign);
            while Has_Element (K) loop
               Query_Element (K, On_FK'Access);
               Next (K);
            end loop;

            Append (Foreign_Attrs,    Local_Ref_Attrs);
            Append (Referenced_Table, Local_Ref_Table);
            Append (Referenced_Attrs, Local_Attrs);
         end;
      end;

      Next (C);
   end loop;

   Insert (T, Assoc ("TABLE_NAME",        Table_Names));
   Insert (T, Assoc ("DESCRIPTION",       Descriptions));
   Insert (T, Assoc ("FIELD_NAME",        Fields));
   Insert (T, Assoc ("FIELD_TYPE",        Types));
   Insert (T, Assoc ("FIELD_DESCR",       Fields_Descr));
   Insert (T, Assoc ("FOREIGN_ATTRS",     Foreign_Attrs));
   Insert (T, Assoc ("REFERENCED_TABLE",  Referenced_Table));
   Insert (T, Assoc ("REFERENCED_TABLES", Referenced_Tables));
   Insert (T, Assoc ("REFERENCED_ATTRS",  Referenced_Attrs));

   Put_Line (Spec_File, Parse ("database.tads", T));
   Put_Line (Body_File, Parse ("database.tadb", T));
end Generate;
