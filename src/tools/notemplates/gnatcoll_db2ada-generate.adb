-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2008-2011, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Text_IO;             use Ada.Text_IO;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

separate (GNATCOLL_Db2Ada)
procedure Generate (Generated : String) is

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (String, "<", "=");
   use String_Sets;

   Spec_File, Body_File : File_Type;

   function Capitalize (Name : Unbounded_String) return String;
   --  Make a name suitable for display

   procedure Print_Comment
     (File : File_Type; Indent : String; Comment : String);
   --  Print a multi-line comment to the File

   Process_Abstract_Tables : Boolean;
   procedure Print_Table_Spec (T_Descr : in out Table_Description);
   --  Print the specs for a table.
   --  If Process_Abstract_Tables is True, only abstract tables are processed.
   --  Otherwise they are ignored.

   Names     : String_Sets.Set;
   procedure Print_String_Constants (Table : in out Table_Description);
   --  Process a table, and generate string constants for it and its fields.
   --  Names is used to avoid duplicates

   procedure Print_Table_Global (Table : in out Table_Description);
   --  Print the global constant for the table.

   procedure Print_FK (Table : in out Table_Description);
   --  Print the FK subprograms

   -------------------
   -- Print_Comment --
   -------------------

   procedure Print_Comment
     (File : File_Type; Indent : String; Comment : String)
   is
      Reflow : constant Boolean := True;
      --  If True, ASCII.LF are ignored in the initial comment, and the text is
      --  reflowed.

      Str   : String := Comment;
      Start : Integer := Str'First;
      Last_Space : Natural := Str'First;
   begin
      for C in Str'Range loop
         if not Reflow and then Str (C) = ASCII.LF then
            Put_Line (File, Indent & "--  " & Str (Start .. C - 1));
            Start := C + 1;
            Last_Space := Start;

         else
            if Str (C) = ASCII.LF or else Str (C) = ' ' then
               Str (C) := ' ';
               Last_Space := C;
               if C = Start then
                  Start := C + 1;
               end if;
            end if;

            if C - Start >= 78 - 4 - Indent'Length then
               Put_Line
                 (File, Indent & "--  " & Str (Start .. Last_Space - 1));
               Start := Last_Space + 1;
            end if;
         end if;
      end loop;

      if Start < Str'Last then
         Put_Line (File, Indent & "--  " & Str (Start .. Str'Last));
      end if;
   end Print_Comment;

   ----------------------
   -- Print_Table_Spec --
   ----------------------

   procedure Print_Table_Spec (T_Descr : in out Table_Description) is
      procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field);
      procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field) is
      begin
         Put (Spec_File, "      "
              & Capitalize (F.Name)
              & " : SQL_Field_" & To_SQL (F.Get_Type));

         if T_Descr.Is_Abstract then
            Put (Spec_File, " (Table_Name");
         else
            Put (Spec_File, " (Ta_" & Capitalize (T_Descr.Name));
         end if;

         Put_Line (Spec_File,
                   ", Instance, N_" & Capitalize (F.Name) & ", Index);");

         if F.Description /= "" then
            Print_Comment (Spec_File, "      ", F.Description);
            New_Line (Spec_File);
         end if;
      end For_Field;

   begin
      if (Process_Abstract_Tables and then not T_Descr.Is_Abstract)
        or else (not Process_Abstract_Tables and then T_Descr.Is_Abstract)
      then
         return;
      end if;

      New_Line (Spec_File);

      if T_Descr.Is_Abstract then
         Put_Line (Spec_File, "   type T_" & Capitalize (T_Descr.Name));
         Put (Spec_File, "      (Table_Name, Instance : Cst_String_Access;");
         Put_Line (Spec_File, " Index : Integer)");
         Put (Spec_File, "      is abstract new ");

         if T_Descr.Super_Table /= No_Table then
            Put (Spec_File, "T_" & Capitalize (T_Descr.Super_Table.Name));
         else
            Put (Spec_File, "SQL_Table");
         end if;

         Put_Line (Spec_File,
                   " (Table_Name, Instance, Index) with");

      else
         Put_Line (Spec_File, "   type T_Abstract_" & Capitalize (T_Descr.Name)
                   & " (Instance : Cst_String_Access; Index : Integer)");
         Put (Spec_File, "      is abstract new ");

         if T_Descr.Super_Table /= No_Table then
            Put (Spec_File, "T_" & Capitalize (T_Descr.Super_Table.Name));
         else
            Put (Spec_File, "SQL_Table");
         end if;

         Put_Line (Spec_File,
                   " (Ta_" & Capitalize (T_Descr.Name)
                   & ", Instance, Index) with");
      end if;

      Put_Line (Spec_File, "   record");
      For_Each_Field (T_Descr, For_Field'Access, False);
      Put_Line (Spec_File, "   end record;");

      Print_Comment (Spec_File, "   ", T_Descr.Description);

      if not T_Descr.Is_Abstract then
         New_Line (Spec_File);
         Put_Line (Spec_File, "   type T_" & Capitalize (T_Descr.Name)
                   & " (Instance : Cst_String_Access)");
         Put (Spec_File,
              "      is new T_Abstract_" & Capitalize (T_Descr.Name));
         Put_Line (Spec_File, " (Instance, -1) with null record;");

         Put_Line (Spec_File, "   type T_Numbered_" & Capitalize (T_Descr.Name)
                   & " (Index : Integer)");
         Put (Spec_File,
              "      is new T_Abstract_" & Capitalize (T_Descr.Name));
         Put_Line (Spec_File, " (null, Index) with null record;");
      end if;
   end Print_Table_Spec;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (Name : Unbounded_String) return String is
   begin
      return Capitalize (To_String (Name));
   end Capitalize;

   ----------------------------
   -- Print_String_Constants --
   ----------------------------

   procedure Print_String_Constants (Table : in out Table_Description) is
      procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field);
      procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field) is
      begin
         Names.Include (F.Name);
      end For_Field;

   begin
      Put_Line (Spec_File, "   TC_" & Capitalize (Table.Name)
                & " : aliased constant String := """ & Table.Name & """;");
      Put_Line (Spec_File, "   Ta_" & Capitalize (Table.Name)
                & " : constant Cst_String_Access := TC_"
                & Capitalize (Table.Name) & "'Access;");

      For_Each_Field (Table, For_Field'Access, True);
   end Print_String_Constants;

   ------------------------
   -- Print_Table_Global --
   ------------------------

   procedure Print_Table_Global (Table : in out Table_Description) is
   begin
      if not Table.Is_Abstract then
         Put_Line (Spec_File, "   " & Capitalize (Table.Name)
                   & " : T_" & Capitalize (Table.Name) & " (null);");
      end if;
   end Print_Table_Global;

   --------------
   -- Print_FK --
   --------------

   procedure Print_FK (Table : in out Table_Description) is
      Prev_Id : Integer := -1;

      procedure For_FK
        (From, To : GNATCOLL.SQL.Inspect.Field;
         Id : Natural; Ambiguous : Boolean);

      procedure Finish_Subprogram (Id : Integer);

      procedure Finish_Subprogram (Id : Integer) is
      begin
         if Id /= Prev_Id then
            Put_Line (Body_File, ";");
            Put_Line (Body_File, "   end FK;");
         end if;
      end Finish_Subprogram;

      procedure For_FK
        (From, To : GNATCOLL.SQL.Inspect.Field;
         Id : Natural; Ambiguous : Boolean) is
      begin
         if not Ambiguous then
            if Id /= Prev_Id then
               if Prev_Id /= -1 then
                  Finish_Subprogram (Id);
               end if;

               Put_Line
                 (Spec_File, "   function FK (Self : T_" &
                    Capitalize (Table.Name) &
                    "'Class; Foreign : T_" & Capitalize (To.Get_Table.Name) &
                    "'Class) return SQL_Criteria;");

               New_Line (Body_File);
               Put_Line
                 (Body_File, "   function FK (Self : T_" &
                    Capitalize (Table.Name) &
                    "'Class; Foreign : T_" & Capitalize (To.Get_Table.Name) &
                    "'Class) return SQL_Criteria is");
               Put_Line (Body_File, "   begin");
               Put (Body_File, "      return Self.");
            else
               New_Line (Body_File);
               Put (Body_File, "         and Self.");
            end if;

            Put (Body_File,
                 Capitalize (From.Name)
                 & " = Foreign." & Capitalize (To.Name));
            Prev_Id := Id;
         end if;
      end For_FK;

   begin
      For_Each_FK (Table, For_FK'Access);
      Finish_Subprogram (-1);
   end Print_FK;

   N : String_Sets.Cursor;

begin
   --  This version creates the output via a simple list of calls to Put_Line.
   --  A more advanced version using the templates_parser is also available,
   --  but requires the installation of the latter on the machine

   --  Create the database_names package

   if Output (Output_Ada_Specs) then
      Create (Spec_File, Name => To_Lower (Generated) & "_names.ads");
      Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");
      Put_Line (Spec_File, "package " & Generated & "_Names is");
      Put_Line (Spec_File, "   pragma Style_Checks (Off);");

      For_Each_Table (Schema, Print_String_Constants'Access);

      New_Line (Spec_File);

      N := First (Names);
      while Has_Element (N) loop
         Put_Line (Spec_File, "   NC_" & Capitalize (Element (N))
                   & " : aliased constant String := """
                   & Element (N) & """;");
         Put_Line (Spec_File, "   N_" & Capitalize (Element (N))
                   & " : constant Cst_String_Access := NC_"
                   & Element (N) & "'Access;");
         Next (N);
      end loop;

      Put_Line (Spec_File, "end " & Generated & "_Names;");
      Close (Spec_File);
   end if;

   --  Create the database package

   Create (Spec_File, Name => To_Lower (Generated) & ".ads");
   Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");

   if Output (Output_Ada_Specs) then
      Put_Line (Spec_File, "with " & Generated & "_Names;"
                & " use " & Generated & "_Names;");
   end if;

   Put_Line (Spec_File, "package " & Generated & " is");
   Put_Line (Spec_File, "   pragma Style_Checks (Off);");
   Put_Line (Spec_File, "   pragma Elaborate_Body;");

   if Output (Output_Ada_Specs) then
      Create (Body_File, Name => To_Lower (Generated) & ".adb");
      Put_Line (Body_File, "package body " & Generated & " is");
      Put_Line (Body_File, "   pragma Style_Checks (Off);");
      Put_Line (Body_File, "   use type Cst_String_Access;");
   end if;

   --  Process enumerations

   if Output (Output_Ada_Enums) then
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

      declare
         C4 : Variables_List.Cursor := First (Variables);
      begin
         if Has_Element (C4) then
            New_Line (Spec_File);
         end if;

         while Has_Element (C4) loop
            Put_Line
              (Spec_File, "   " & Capitalize (Element (C4).Name)
               & " : constant := " & To_String (Element (C4).Value) & ";");
            Print_Comment (Spec_File, "   ", To_String (Element (C4).Comment));
            Next (C4);
         end loop;
      end;
   end if;

   if Output (Output_Ada_Specs) then
      Process_Abstract_Tables := True;
      For_Each_Table (Schema, Print_Table_Spec'Access);
      Process_Abstract_Tables := False;
      For_Each_Table (Schema, Print_Table_Spec'Access);

      New_Line (Spec_File);
      For_Each_Table (Schema, Print_FK'Access);

      New_Line (Spec_File);
      For_Each_Table (Schema, Print_Table_Global'Access);
   end if;

   Put_Line (Spec_File, "end " & Generated & ";");
   Close (Spec_File);

   if Output (Output_Ada_Specs) then
      Put_Line (Body_File, "end " & Generated & ";");
      Close (Body_File);
   end if;
end Generate;
