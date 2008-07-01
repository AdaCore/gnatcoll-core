-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2008, AdaCore                       --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;          use Ada.Containers;
with Ada.Text_IO;             use Ada.Text_IO;

separate (Gnatcoll_Db2Ada)
procedure Generate (Spec_File, Body_File : File_Type) is
   function Capitalize (Name : String) return String;
   function Capitalize (Name : Unbounded_String) return String;
   --  Make a name suitable for display

   procedure Print_Comment (Indent : String; Comment : String);
   --  Print a multi-line comment to the Spec_File;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (Name : Unbounded_String) return String is
   begin
      return Capitalize (To_String (Name));
   end Capitalize;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (Name : String) return String is
      Result : String (Name'Range);
      J : Integer := Result'First;
   begin
      for N in Name'Range loop
         if Name (N) = '+' then
            Result (J) := 'p';
            J := J + 1;
         elsif Name (N) = '?' then
            Result (J) := 'U';
            J := J + 1;
         elsif Name (N) = '_'
           and then N > Name'First
           and then Name (N - 1) = '_'
         then
            null;
         elsif Name (N) >= ' ' and then Name (N) <= '/' then
            Result (J) := '_';
            J := J + 1;
         elsif J = Result'First
           or else Result (J - 1) = '_'
         then
            Result (J) := To_Upper (Name (N));
            J := J + 1;

         else
            Result (J) := To_Lower (Name (N));
            J := J + 1;
         end if;
      end loop;
      return Result (Result'First .. J - 1);
   end Capitalize;

   -------------------
   -- Print_Comment --
   -------------------

   procedure Print_Comment (Indent : String; Comment : String) is
      Start : Integer := Comment'First;
   begin
      for C in Comment'Range loop
         if Comment (C) = ASCII.LF then
            Put_Line (Spec_File, Indent & "--  " & Comment (Start .. C - 1));
            Start := C + 1;
         end if;
      end loop;

      if Comment'Length /= 0 then
         Put_Line
           (Spec_File, Indent & "--  " & Comment (Start .. Comment'Last));
      end if;
   end Print_Comment;

   C         : Tables_Maps.Cursor;
   A         : Attribute_Lists.Cursor;
   T_Descr   : Table_Description;
   K         : Foreign_Keys.Cursor;
   FK        : Foreign_Key_Description;
   S1, S2    : String_Lists.Cursor;

begin
   --  This version creates the output via a simple list of calls to Put_Line.
   --  A more advanced version using the templates_parser is also available,
   --  but requires the installation of the latter on the machine

   --  Print header

   Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");
   Put_Line (Spec_File, "package Database is");
   Put_Line (Spec_File, "   pragma Style_Checks (Off);");

   Put_Line (Body_File, "package body Database is");
   Put_Line (Body_File, "   pragma Style_Checks (Off);");

   --  Process enumerations

   declare
      C      : Enumeration_Lists.Cursor := First (Enumerations);
      Enum   : Dumped_Enums;
      C2, C3 : String_Lists.Cursor;
   begin
      while Has_Element (C) loop
         Enum := Element (C);

         New_Line (Spec_File);
         Put_Line (Spec_File, "   subtype " & Capitalize (Enum.Type_Name)
                   & " is " & Capitalize (Enum.Base_Type) & ";");
         C2 := First (Enum.Names);
         C3 := First (Enum.Values);
         while Has_Element (C2) loop
            if Enum.Base_Type = "String" then
               Put_Line (Spec_File, "   " & Capitalize (Element (C2))
                         & " : constant "
                         & Capitalize (Enum.Type_Name)
                         & " := """ & Element (C3) & """;");
            else
               Put_Line (Spec_File, "   " & Capitalize (Element (C2))
                         & " : constant "
                         & Capitalize (Enum.Type_Name)
                         & " := " & Element (C3) & ";");
            end if;
            Next (C2);
            Next (C3);
         end loop;

         Next (C);
      end loop;
   end;

   --  Process variables

   New_Line (Spec_File);

   declare
      C4 : Variables_List.Cursor := First (Variables);
   begin
      while Has_Element (C4) loop
         Put_Line (Spec_File, "   " & Capitalize (Element (C4).Name)
                   & " : constant := " & To_String (Element (C4).Value) & ";");
         Print_Comment ("   ", To_String (Element (C4).Comment));
         Next (C4);
      end loop;
   end;

   --  Process tables

   New_Line (Spec_File);
   C := First (Tables);
   while Has_Element (C) loop
      Put_Line (Spec_File, "   Ta_" & Capitalize (Key (C))
                & " : aliased constant String := """
                & Key (C) & """;");
      Next (C);
   end loop;

   C := First (Tables);
   while Has_Element (C) loop
      T_Descr      := Element (C);

      New_Line (Spec_File);
      Put_Line (Spec_File, "   package T_" & Capitalize (Key (C)) & " is");

      A := First (T_Descr.Attributes);
      while Has_Element (A) loop
         Put_Line (Spec_File, "      N_" & Capitalize (Element (A).Name)
                   & " : aliased constant String := """
                   & To_String (Element (A).Name) & """;");
         Next (A);
      end loop;

      New_Line (Spec_File);
      Put_Line (Spec_File, "      type Table (Instance : Cst_String_Access)");
      Put_Line (Spec_File, "         is new SQL_Table (Ta_"
                & Capitalize (Key (C)) & "'Access, Instance) with");
      Put_Line (Spec_File, "      record");

      A := First (T_Descr.Attributes);
      while Has_Element (A) loop
         Put_Line (Spec_File, "         "
                   & Capitalize (Element (A).Name)
                   & " : SQL_Field_" & To_String (Element (A).Ada_Type));
         Put_Line (Spec_File, "            "
                   & "(Ta_" & Capitalize (Key (C)) & "'Access, Instance, N_"
                   & Capitalize (Element (A).Name) & "'Access);");
         Print_Comment ("         ", To_String (Element (A).Description));
         New_Line (Spec_File);
         Next (A);
      end loop;

      Put_Line (Spec_File, "      end record;");
      Print_Comment ("      ", To_String (T_Descr.Description));

      if Length (T_Descr.Foreign) /= 0 then
         New_Line (Spec_File);
         Put_Line (Spec_File, "      overriding function FK (Self : Table;"
                   & " Foreign : SQL_Table'Class) return SQL_Criteria;");
      end if;

      Put_Line (Spec_File, "   end T_" & Capitalize (Key (C)) & ";");

      New_Line (Spec_File);
      Put_Line (Spec_File, "   " & Capitalize (Key (C))
                & " : T_" & Capitalize (Key (C)) & ".Table (null);");

      if Length (T_Descr.Foreign) /= 0 then
         New_Line (Body_File);
         Put_Line
           (Body_File, "   package body T_" & Capitalize (Key (C)) & " is");
         Put_Line (Body_File, "      function FK");
         Put_Line (Body_File,
                   "        (Self : Table; Foreign : SQL_Table'Class)"
                   & " return SQL_Criteria is");
         Put_Line (Body_File, "      begin");

         K := First (T_Descr.Foreign);
         while Has_Element (K) loop
            FK := Element (K);
            Put_Line (Body_File, "         if Foreign.Table_Name = Ta_"
                      & Capitalize (FK.To_Table) & "'Access then");

            S1 := First (FK.From_Attributes);
            S2 := First (FK.To_Attributes);
            while Has_Element (S1) loop
               if S1 = First (FK.From_Attributes) then
                  Put (Body_File, "            return Self.");
               else
                  Put (Body_File, "               and Self.");
               end if;

               Put (Body_File,
                    Capitalize (Element (S1))
                    & " = T_" & Capitalize (FK.To_Table)
                    & ".Table (Foreign)."
                    & Capitalize (Element (S2)));

               if S1 = Last (FK.From_Attributes) then
                  Put_Line (Body_File, ";");
               else
                  New_Line (Body_File);
               end if;

               Next (S1);
               Next (S2);
            end loop;

            Put_Line (Body_File, "         end if;");
            Next (K);
         end loop;

         Put_Line (Body_File, "         return No_Criteria;");
         Put_Line (Body_File, "      end FK;");
         Put_Line (Body_File, "   end T_" & Capitalize (Key (C)) & ";");
      end if;

      Next (C);
   end loop;

--        declare
--           T_Fields      : Tag;
--           T_Types       : Tag;
--           T_Field_Descr : Tag;
--        begin
--           A := First (T_Descr.Attributes);
--           while Has_Element (A) loop
--              T_Fields      := T_Fields      & Element (A).Name;
--              T_Field_Descr := T_Field_Descr & Element (A).Description;
--              T_Types       := T_Types       & Element (A).Ada_Type;
--              Next (A);
--           end loop;
--
--           Table_Names  := Table_Names  & Key (C);
--           Descriptions := Descriptions & T_Descr.Description;
--           Fields       := Fields       & T_Fields;
--           Types        := Types        & T_Types;
--           Fields_Descr := Fields_Descr & T_Field_Descr;
--
--           declare
--              Local_Ref_Table, Local_Ref_Attrs, Local_Attrs : Tag;
--              K  : Foreign_Keys.Cursor;
--
--              procedure On_FK (FK : Foreign_Key_Description) is
--                 S  : String_Lists.Cursor;
--              begin
--                 Append (Local_Ref_Table, FK.To_Table);
--                 S := First (FK.From_Attributes);
--                 while Has_Element (S) loop
--                    Append (Local_Attrs, Element (S));
--                    Next (S);
--                 end loop;
--
--                 S := First (FK.To_Attributes);
--                 while Has_Element (S) loop
--                    Append (Local_Ref_Attrs, Element (S));
--                    Next (S);
--                 end loop;
--              end On_FK;
--
--           begin
--              K := First (T_Descr.Foreign);
--              while Has_Element (K) loop
--                 Query_Element (K, On_FK'Access);
--                 Next (K);
--              end loop;
--
--              Append (Foreign_Attrs,    Local_Ref_Attrs);
--              Append (Referenced_Table, Local_Ref_Table);
--              Append (Referenced_Attrs, Local_Attrs);
--           end;
--        end;
--
--        Next (C);
--     end loop;
--
--     Insert (T, Assoc ("TABLE_NAME",        Table_Names));
--     Insert (T, Assoc ("DESCRIPTION",       Descriptions));
--     Insert (T, Assoc ("FIELD_NAME",        Fields));
--     Insert (T, Assoc ("FIELD_TYPE",        Types));
--     Insert (T, Assoc ("FIELD_DESCR",       Fields_Descr));
--     Insert (T, Assoc ("FOREIGN_ATTRS",     Foreign_Attrs));
--     Insert (T, Assoc ("REFERENCED_TABLE",  Referenced_Table));
--     Insert (T, Assoc ("REFERENCED_TABLES", Referenced_Tables));
--     Insert (T, Assoc ("REFERENCED_ATTRS",  Referenced_Attrs));
--
--     Put_Line (Spec_File, Parse ("database.tads", T));
--     Put_Line (Body_File, Parse ("database.tadb", T));

   --  Print footer

   Put_Line (Spec_File, "end Database;");
   Put_Line (Body_File, "end Database;");

end Generate;
