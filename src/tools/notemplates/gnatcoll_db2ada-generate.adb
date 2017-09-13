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

pragma Ada_2012;

with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Text_IO;             use Ada.Text_IO;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

--  Register advanced types
pragma Warnings (Off, "no entities of * are referenced");
with GNATCOLL.SQL_Fields;     use GNATCOLL.SQL_Fields;
pragma Warnings (On, "no entities of * are referenced");

separate (GNATCOLL_Db2Ada)
procedure Generate
  (Connection    : not null Database_Connection;
   Generated     : String;
   Include_Database_Create : Boolean)
is
   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (String, "<", "=");
   use String_Sets;

   Spec_File, Body_File : File_Type;

   Seen_Withs : String_Sets.Set;
   --  The 'with' clauses that have already been output, to avoid
   --  duplicates.

   function Cleanup (Str : String) return String;
   --  Remove all characters that are not valid for an Ada identifier,
   --  and capitalize the result.

   function Get_Namespace (T_Descr : Table_Description) return String;
   --  Return the name of the namespace for the table

   procedure Print_Comment
     (File : File_Type; Indent : String; Comment : String);
   --  Print a multi-line comment to the File

   procedure Print_Withs (T_Descr : in out Table_Description);
   --  Print all withs needed for the given table

   Names     : String_Sets.Set;
   procedure Print_String_Constants (Table : in out Table_Description);
   --  Process a table, and generate string constants for it and its fields.
   --  Names is used to avoid duplicates

   procedure Print_Database_Create;
   --  Print the Ada subprogram that recreates the database and its initial
   --  contents.

   procedure Process_Tables;
   --  Generate the Ada mapping for all the SQL tables

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

   -----------------
   -- Print_Withs --
   -----------------

   procedure Print_Withs (T_Descr : in out Table_Description) is

      procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field);
      procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field) is
         Typ : constant String := F.Get_Actual_Type.Ada_Field_Type_Name;
      begin
         for T in reverse Typ'Range loop
            if Typ (T) = '.' then
               declare
                  N : constant String := Typ (Typ'First .. T - 1);
               begin
                  if not Seen_Withs.Contains (N) then
                     Put_Line
                        (Spec_File,
                         "with " & N & "; use " & N & ";");
                     Seen_Withs.Include (N);
                  end if;
               end;
               exit;
            end if;
         end loop;
      end For_Field;
   begin
      For_Each_Field (T_Descr, For_Field'Access, False);
   end Print_Withs;

   -------------------
   -- Get_Namespace --
   -------------------

   function Get_Namespace (T_Descr : Table_Description) return String is
      N : constant String := T_Descr.Name;
   begin
      for J in N'Range loop
         if N (J) = '.' then
            return Cleanup (N (N'First .. J - 1));
         end if;
      end loop;
      return "";
   end Get_Namespace;

   --------------------
   -- Process_Tables --
   --------------------

   procedure Process_Tables is
      Namespaces              : String_Sets.Set;
      Indent_Level            : Natural;
      Process_Abstract_Tables : Boolean;
      Current_Namespace       : XString;

      procedure Add_Namespace (T_Descr : in out Table_Description);
      --  Add the namespace for T_Descr

      procedure Print_Table_Spec (T_Descr : in out Table_Description);
      --  Print the specs for a table.
      --  If Process_Abstract_Tables is True, only abstract tables are
      --  processed. Otherwise they are ignored.

      procedure Print_Table_Global (Table : in out Table_Description);
      --  Print the global constant for the table.

      procedure Print_FK (Table : in out Table_Description);
      --  Print the FK subprograms

      function Table_Matches (T_Descr : Table_Description) return Boolean
         is (Get_Namespace (T_Descr) = Current_Namespace
             and then
                ((Process_Abstract_Tables and then T_Descr.Is_Abstract)
                 or else (not Process_Abstract_Tables
                          and then not T_Descr.Is_Abstract)));
      --  Whether the table matches the current filtering

      -------------------
      -- Add_Namespace --
      -------------------

      procedure Add_Namespace (T_Descr : in out Table_Description) is
      begin
         Namespaces.Include (Get_Namespace (T_Descr));
      end Add_Namespace;

      ----------------------
      -- Print_Table_Spec --
      ----------------------

      procedure Print_Table_Spec (T_Descr : in out Table_Description) is
         Indent : constant String := (1 .. Indent_Level => ' ');

         procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field);
         procedure For_Field (F : in out GNATCOLL.SQL.Inspect.Field) is
            Typ : constant String := F.Get_Actual_Type.Ada_Field_Type_Name;
         begin
            Put
               (Spec_File,
                Indent & "   " & Cleanup (F.Name) & " : " & Typ);

            Put_Line (Spec_File,
                      "(Table_Name, Instance, N_"
                      & Cleanup (F.Name) & ", Index);");

            if F.Description /= "" then
               Print_Comment (Spec_File, Indent & "   ", F.Description);
               New_Line (Spec_File);
            end if;
         end For_Field;

      begin
         if not Table_Matches (T_Descr) then
            return;
         end if;

         New_Line (Spec_File);

         if T_Descr.Is_Abstract then
            Put_Line
               (Spec_File,
                Indent & "type T_" & Cleanup (T_Descr.Name));
            Put_Line
               (Spec_File,
                Indent & "   (Table_Name : Cst_String_Access;");
            Put_Line
               (Spec_File,
                Indent & "    Instance   : Cst_String_Access;");
            Put_Line
               (Spec_File,
                Indent & "    Index      : Integer)");
            Put
               (Spec_File,
                Indent & "is abstract new ");

            if T_Descr.Super_Table /= No_Table then
               Put (Spec_File, "T_" & Cleanup (T_Descr.Super_Table.Name));
            else
               Put (Spec_File, "SQL_Table");
            end if;

            Put_Line (Spec_File, " (Table_Name, Instance, Index) with");

         else
            Put_Line
               (Spec_File,
                Indent & "type T_Abstract_" & Cleanup (T_Descr.Name));
            Put_Line
               (Spec_File,
                Indent &
                "   (Table_Name : Cst_String_Access; --  Name of the table");
            Put_Line
               (Spec_File,
                Indent &
                "    Instance   : Cst_String_Access; " &
                "--  if null, use table name");
            Put_Line
               (Spec_File,
                Indent & "    Index      : Integer)  " &
                "--  Create numbered aliases");
            Put
               (Spec_File,
                Indent & "is new ");

            if T_Descr.Super_Table /= No_Table then
               Put (Spec_File, "T_" & Cleanup (T_Descr.Super_Table.Name));
            else
               Put (Spec_File, "SQL_Table");
            end if;

            Put_Line (Spec_File, " (Table_Name, Instance, Index) with");
         end if;

         Put_Line (Spec_File, Indent & "record");
         For_Each_Field (T_Descr, For_Field'Access, False);
         Put_Line (Spec_File, Indent & "end record;");

         Print_Comment (Spec_File, Indent, T_Descr.Description);
         Put_Line
            (Spec_File,
             "   --  Use this table directly if you use clones of this table");
         Put_Line
            (Spec_File,
             "   --  in your code, for instance in a temporary table");

         if not T_Descr.Is_Abstract then
            New_Line (Spec_File);
            Put_Line (Spec_File, Indent & "type T_" & Cleanup (T_Descr.Name)
                      & " (Instance : Cst_String_Access)");
            Put (Spec_File,
                 Indent & "   is new T_Abstract_" & Cleanup (T_Descr.Name));
            Put_Line (Spec_File,
               " (Ta_" & Cleanup (T_Descr.Name)
               & ", Instance, -1) with null record;");
            Put_Line
               (Spec_File,
                Indent & "--  To use named aliases of the table in a query");
            Put_Line
               (Spec_File,
                Indent & "--  Use Instance=>null to use the default name.");

            New_Line (Spec_File);
            Put_Line
               (Spec_File,
                Indent & "type T_Numbered_" & Cleanup (T_Descr.Name)
                & " (Index : Integer)");
            Put (Spec_File,
                Indent &  "   is new T_Abstract_" & Cleanup (T_Descr.Name));
            Put_Line (Spec_File, " (Ta_" & Cleanup (T_Descr.Name)
               & ", null, Index) with null record;");
            Put_Line
               (Spec_File,
                Indent & "--  To use aliases in the form name1, name2,...");
         end if;
      end Print_Table_Spec;

      --------------
      -- Print_FK --
      --------------

      procedure Print_FK (Table : in out Table_Description) is
         Indent : constant String := (1 .. Indent_Level => ' ');
         Prev_Id : Integer := -1;

         procedure For_FK
           (From, To : GNATCOLL.SQL.Inspect.Field;
            Id : Natural; Ambiguous : Boolean);

         procedure Finish_Subprogram (Id : Integer);

         procedure Finish_Subprogram (Id : Integer) is
         begin
            if Id /= Prev_Id then
               Put_Line (Body_File, ";");
               Put_Line (Body_File, Indent & "end FK;");
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
                    (Spec_File, Indent & "function FK (Self : T_" &
                       Cleanup (Table.Name) &
                       "'Class; Foreign : T_" & Cleanup (To.Get_Table.Name)
                       & "'Class) return SQL_Criteria;");

                  New_Line (Body_File);
                  Put_Line
                    (Body_File, Indent & "function FK (Self : T_" &
                       Cleanup (Table.Name) &
                       "'Class; Foreign : T_" & Cleanup (To.Get_Table.Name)
                       & "'Class) return SQL_Criteria is");
                  Put_Line (Body_File, Indent & "begin");
                  Put (Body_File, Indent & "   return Self.");
               else
                  New_Line (Body_File);
                  Put (Body_File, Indent & "      and Self.");
               end if;

               Put (Body_File,
                    Cleanup (From.Name)
                    & " = Foreign." & Cleanup (To.Name));
               Prev_Id := Id;
            end if;
         end For_FK;

      begin
         if Get_Namespace (Table) = Current_Namespace then
            For_Each_FK (Table, For_FK'Access);
            Finish_Subprogram (-1);
         end if;
      end Print_FK;

      ------------------------
      -- Print_Table_Global --
      ------------------------

      procedure Print_Table_Global (Table : in out Table_Description) is
         Indent : constant String := (1 .. Indent_Level => ' ');
         NS     : constant String := Get_Namespace (Table);
      begin
         if not Table.Is_Abstract
            and then NS = Current_Namespace
         then
            declare
               --  The namespace's name is already part of the enclosing
               --  package, so don't duplicate it in the table's name.
               N : constant String := Cleanup (Table.Name);
               N2 : constant String :=
                  N (N'First
                     + (if NS'Length = 0 then 0 else NS'Length + 1)
                     .. N'Last);
            begin
               Put_Line
                  (Spec_File, Indent & N2 & " : T_" & N & " (null);");
            end;
         end if;
      end Print_Table_Global;

   begin
      --  Compute the namespaces
      For_Each_Table (Schema, Add_Namespace'Access);

      for Namespace of Namespaces loop
         Current_Namespace := To_XString (Namespace);

         if Namespace /= "" then
            New_Line (Spec_File);
            Put_Line (Spec_File,
                      "   ---" & (1 .. Namespace'Length => '-') & "---");
            Put_Line (Spec_File,
                      "   -- " & Namespace & " --");
            Put_Line (Spec_File,
                      "   ---" & (1 .. Namespace'Length => '-') & "---");
            New_Line (Spec_File);

            Put_Line (Spec_File, "   package " & Namespace & " is");
            Put_Line (Body_File, "   package body " & Namespace & " is");
            Indent_Level := 6;
         else
            Indent_Level := 3;
         end if;

         Process_Abstract_Tables := True;
         For_Each_Table (Schema, Print_Table_Spec'Access);

         Process_Abstract_Tables := False;
         For_Each_Table (Schema, Print_Table_Spec'Access);

         New_Line (Spec_File);
         For_Each_Table (Schema, Print_FK'Access);

         For_Each_Table (Schema, Print_Table_Global'Access);

         if Namespace /= "" then
            Put_Line (Spec_File, "   end " & Namespace & ";");
            Put_Line (Body_File, "   end " & Namespace & ";");
         end if;
      end loop;
   end Process_Tables;

   -------------
   -- Cleanup --
   -------------

   function Cleanup (Str : String) return String is
      Result : String (Str'Range);
      Idx    : Natural := Result'First;
   begin
      for C in Str'Range loop
         if GNATCOLL.Utils.Is_Identifier (Str (C))
            or else Str (C) = '.'   --  handling of namespaces
         then
            Result (Idx) := Str (C);
            Idx := Idx + 1;
         end if;
      end loop;
      return Capitalize (Result (Result'First .. Idx - 1));
   end Cleanup;

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
      Put_Line (Spec_File, "   TC_" & Cleanup (Table.Name)
                & " : aliased constant String := """
                & Ada_Quote (Quote_Keyword (Table.Name)) & """;");
      Put_Line (Spec_File, "   Ta_" & Cleanup (Table.Name)
                & " : constant Cst_String_Access := TC_"
                & Cleanup (Table.Name) & "'Access;");

      For_Each_Field (Table, For_Field'Access, True);
   end Print_String_Constants;

   ---------------------------
   -- Print_Database_Create --
   ---------------------------

   procedure Print_Database_Create is
      Spec : constant String :=
        "   procedure Create_Database" & ASCII.LF
        & "      (DB : not null access"
        &  " GNATCOLL.SQL.Exec.Database_Connection_Record'Class)";

      procedure Puts (Data : String);
      --  Format Data as an Ada String

      procedure Puts (Data : String) is
      begin
         for D in Data'Range loop
            if Data (D) = '"' then
               Put (Body_File, """""");
            elsif Data (D) = ASCII.LF then
               Put_Line (Body_File, """ & ASCII.LF");
               Put (Body_File, "         & """);
            else
               Put (Body_File, Data (D));
            end if;
         end loop;
      end Puts;

      F : File_Schema_IO;
   begin
      Put_Line (Spec_File, ASCII.LF & Spec & ";");
      Put_Line
        (Spec_File, "   --  Create the database and its initial contents");
      Put_Line
        (Spec_File, "   --  The SQL is not automatically committed");

      Put_Line (Body_File, ASCII.LF & Spec & ASCII.LF & "   is");

      Put (Body_File, "      DbSchema : constant String := """);
      F.File := GNATCOLL.VFS.No_File;
      Write_Schema
        (F, Connection, Schema, Puts => Puts'Access, Align_Columns => False,
         Show_Comments => False);
      Put_Line (Body_File, """;");

      if Load_File /= GNATCOLL.VFS.No_File then
         Put (Body_File, "      Data : constant String := """);
         Load_Data (Load_File, Puts'Access);
         Put_Line (Body_File, """;");
      end if;

      Put_Line (Body_File, "      F : File_Schema_IO;");
      Put_Line (Body_File, "      D : DB_Schema_IO;");
      Put_Line (Body_File, "      Schema : DB_Schema;");
      Put_Line (Body_File, "   begin");
      Put_Line (Body_File, "      Schema := Read_Schema (F, DbSchema);");
      Put_Line
         (Body_File,
          "      Write_Schema (D, Database_Connection (DB), Schema);");

      if Load_File /= GNATCOLL.VFS.No_File then
         Put_Line (Body_File, "      if DB.Success then");
         Put_Line (Body_File, "         Load_Data (DB, Data, Schema);");
         Put_Line (Body_File, "      end if;");
      end if;

      Put_Line (Body_File, "   end Create_Database;");
   end Print_Database_Create;

   N : String_Sets.Cursor;
   F : Virtual_File;

   Base_File : constant Filesystem_String :=
     +To_Lower (Translate (Generated, To_Mapping (".", "-")));

begin
   Seen_Withs.Include ("GNATCOLL.SQL");

   --  This version creates the output via a simple list of calls to Put_Line.
   --  A more advanced version using the templates_parser is also available,
   --  but requires the installation of the latter on the machine

   --  Create the database_names package

   if Output (Output_Ada_Specs) then
      F := Create_From_Dir
        (Dir => Output_Dir, Base_Name => Base_File & "_names.ads");

      Create (Spec_File, Name => F.Display_Full_Name);
      Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");

      Put_Line (Spec_File, "package " & Generated & "_Names is");
      Put_Line (Spec_File, "   pragma Style_Checks (Off);");

      For_Each_Table (Schema, Print_String_Constants'Access);

      New_Line (Spec_File);

      N := First (Names);
      while Has_Element (N) loop
         Put_Line (Spec_File, "   NC_" & Cleanup (Element (N))
                   & " : aliased constant String := """
                   & Ada_Quote (Quote_Keyword (Element (N))) & """;");
         Put_Line (Spec_File, "   N_" & Cleanup (Element (N))
                   & " : constant Cst_String_Access := NC_"
                   & Cleanup (Element (N)) & "'Access;");
         Next (N);
      end loop;

      Put_Line (Spec_File, "end " & Generated & "_Names;");
      Close (Spec_File);
   end if;

   --  Create the database package

   F := Create_From_Dir (Dir => Output_Dir, Base_Name => Base_File & ".ads");
   Create (Spec_File, Name => F.Display_Full_Name);

   Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");

   Put_Line
     (Spec_File,
      "pragma Warnings (Off, ""no entities of * are referenced"");");
   For_Each_Table (Schema, Print_Withs'Access);
   Put_Line
     (Spec_File,
      "pragma Warnings (On, ""no entities of * are referenced"");");

   if Include_Database_Create then
      Put_Line (Spec_File, "with GNATCOLL.SQL.Exec;");
   end if;

   if Output (Output_Ada_Specs) then
      Put_Line (Spec_File, "with " & Generated & "_Names;"
                & " use " & Generated & "_Names;");
   end if;

   Put_Line (Spec_File, "package " & Generated & " is");
   Put_Line (Spec_File, "   pragma Style_Checks (Off);");

   if Output (Output_Ada_Specs) then
      Put_Line (Spec_File, "   pragma Elaborate_Body;");

      F := Create_From_Dir
        (Dir => Output_Dir, Base_Name => Base_File & ".adb");
      Create (Body_File, Name => F.Display_Full_Name);

      if Include_Database_Create then
         Put_Line
           (Body_File, "with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;");
         Put_Line
           (Body_File, "with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;");
      end if;

      Put_Line (Body_File, "package body " & Generated & " is");
      Put_Line (Body_File, "   pragma Style_Checks (Off);");
   end if;

   --  Process enumerations

   if Output (Output_Ada_Enums) then
      declare
         C      : Enumeration_Lists.Cursor := First (Enumerations);
         Enum   : Dumped_Enums;
         C2, C3 : String_Lists.Cursor;

         Max_Name_Len, Max_Value_Len : Integer;

      begin
         while Has_Element (C) loop
            Enum := Element (C);

            declare
               Type_Name : constant String :=
                 Cleanup (Enum.Type_Name.To_String);
               Base_Type : constant String :=
                 Cleanup (Enum.Base_Type.To_String);
               Is_String : constant Boolean :=
                 Base_Type = "String";
            begin
               New_Line (Spec_File);
               Put_Line (Spec_File, "   subtype " & Type_Name
                         & " is " & Base_Type & ";");

               Max_Name_Len  := 0;
               for N of Enum.Names loop
                  if N'Length > Max_Name_Len then
                     Max_Name_Len := N'Length;
                  end if;
               end loop;

               Max_Value_Len := 6; --  "others"
               for V of Enum.Values loop
                  if V'Length > Max_Value_Len then
                     Max_Value_Len := V'Length;
                  end if;
               end loop;

               C2 := First (Enum.Names);
               C3 := First (Enum.Values);
               while Has_Element (C2) loop
                  Put_Line (Spec_File,
                    "   "
                    & Cleanup (To_String (Enum.Prefix)) & '_'
                    & Head (Cleanup (Element (C2)), Max_Name_Len)
                    & " : constant " & Type_Name & " := "
                    & (if Is_String
                       then """" & Element (C3) & """"
                       else Element (C3))
                    & ";");

                  Next (C2);
                  Next (C3);
               end loop;

               if Output (Output_Ada_Enums_Image)
                 and then not Is_String
               then
                  New_Line (Spec_File);
                  Put_Line (Spec_File,
                     "   function Image_" & Type_Name);
                  Put_Line (Spec_File, "     (X : "
                     & Cleanup (Enum.Type_Name.To_String) & ") return String");
                  Put_Line (Spec_File, "   is (case X is");

                  C2 := First (Enum.Names);
                  C3 := First (Enum.Values);
                  while Has_Element (C2) loop
                     Put_Line (Spec_File,
                       "          when " & Head (Element (C3), Max_Value_Len)
                       & " => """ & Element (C2) & """,");
                     Next (C2);
                     Next (C3);
                  end loop;
                  Put_Line (Spec_File,
                    "          when others =>");
                  Put_Line (Spec_File,
                    "             raise Constraint_Error");
                  Put_Line (Spec_File,
                    "               with ""invalid "
                    & Type_Name & " "" & X'Img);");
               end if;
            end;
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
              (Spec_File, "   " & Cleanup (Element (C4).Name.To_String)
               & " : constant := " & To_String (Element (C4).Value) & ";");
            Print_Comment (Spec_File, "   ", To_String (Element (C4).Comment));
            Next (C4);
         end loop;
      end;
   end if;

   if Output (Output_Ada_Specs) then
      Process_Tables;
   end if;

   if Include_Database_Create then
      Print_Database_Create;
   end if;

   Put_Line (Spec_File, "end " & Generated & ";");
   Close (Spec_File);

   if Output (Output_Ada_Specs) then
      Put_Line (Body_File, "end " & Generated & ";");
      Close (Body_File);
   end if;
end Generate;
