-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
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

with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Text_IO;             use Ada.Text_IO;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

separate (Gnatcoll_Db2Ada)
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

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (Name : Unbounded_String) return String is
   begin
      return Capitalize (To_String (Name));
   end Capitalize;

   Names     : String_Sets.Set;
   N         : String_Sets.Cursor;

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

   --  Create the database_names package

   Create (Spec_File, Name => To_Lower (Generated) & "_names.ads");
   Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");
   Put_Line (Spec_File, "package " & Generated & "_Names is");
   Put_Line (Spec_File, "   pragma Style_Checks (Off);");

   C := First (Tables);
   while Has_Element (C) loop
      Put_Line (Spec_File, "   TC_" & Capitalize (Key (C))
                & " : aliased constant String := """ & Key (C) & """;");
      Put_Line (Spec_File, "   Ta_" & Capitalize (Key (C))
                & " : constant Cst_String_Access := TC_"
                & Capitalize (Key (C)) & "'Access;");

      T_Descr := Element (C);
      A       := First (T_Descr.Attributes);
      while Has_Element (A) loop
         Names.Include (Capitalize (To_String (Element (A).Name)));
         Next (A);
      end loop;

      Next (C);
   end loop;

   New_Line (Spec_File);

   N := First (Names);
   while Has_Element (N) loop
      Put_Line (Spec_File, "   NC_" & Element (N)
                & " : aliased constant String := """
                & Element (N) & """;");
      Put_Line (Spec_File, "   N_" & Element (N)
                & " : constant Cst_String_Access := NC_"
                & Element (N) & "'Access;");
      Next (N);
   end loop;

   Put_Line (Spec_File, "end " & Generated & "_Names;");
   Close (Spec_File);

   --  Create the database package

   Create (Spec_File, Name => To_Lower (Generated) & ".ads");
   Create (Body_File, Name => To_Lower (Generated) & ".adb");

   --  Print header

   Put_Line (Spec_File, "with GNATCOLL.SQL; use GNATCOLL.SQL;");
   Put_Line (Spec_File, "with " & Generated & "_Names;"
             & " use " & Generated & "_Names;");
   Put_Line (Spec_File, "package " & Generated & " is");
   Put_Line (Spec_File, "   pragma Style_Checks (Off);");

   Put_Line (Body_File, "package body " & Generated & " is");
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
         Print_Comment (Spec_File, "   ", To_String (Element (C4).Comment));
         Next (C4);
      end loop;
   end;

   --  Process tables

   C := First (Tables);
   while Has_Element (C) loop
      T_Descr      := Element (C);

      New_Line (Spec_File);
      Put_Line (Spec_File, "   type T_" & Capitalize (Key (C))
                & " (Instance : Cst_String_Access)");
      Put_Line (Spec_File, "      is new SQL_Table (Ta_"
                & Capitalize (Key (C)) & ", Instance) with");
      Put_Line (Spec_File, "   record");

      A := First (T_Descr.Attributes);
      while Has_Element (A) loop
         Put_Line (Spec_File, "      "
                   & Capitalize (Element (A).Name)
                   & " : SQL_Field_" & To_String (Element (A).Field_Type)
                   & " (Ta_" & Capitalize (Key (C)) & ", Instance, N_"
                   & Capitalize (Element (A).Name) & ");");
         if Element (A).Description /= "" then
            Print_Comment (Spec_File,
                           "      ", To_String (Element (A).Description));
            New_Line (Spec_File);
         end if;

         Next (A);
      end loop;

      Put_Line (Spec_File, "   end record;");
      Print_Comment (Spec_File, "   ", To_String (T_Descr.Description));

      if Length (T_Descr.Foreign) /= 0 then
         New_Line (Spec_File);
         Put_Line (Spec_File, "   overriding function FK");
         Put_Line (Spec_File, "      (Self : T_"
                   & Capitalize (Key (C))
                   & "; Foreign : SQL_Table'Class) return SQL_Criteria;");
      end if;

      New_Line (Spec_File);
      Put_Line (Spec_File, "   " & Capitalize (Key (C))
                & " : T_" & Capitalize (Key (C)) & " (null);");

      if Length (T_Descr.Foreign) /= 0 then
         New_Line (Body_File);
         Put_Line (Body_File, "   function FK");
         Put_Line (Body_File,
                   "     (Self : T_" & Capitalize (Key (C))
                   & "; Foreign : SQL_Table'Class) return SQL_Criteria is");
         Put_Line (Body_File, "   begin");

         K := First (T_Descr.Foreign);
         while Has_Element (K) loop
            FK := Element (K);
            Put_Line (Body_File, "      if Foreign.Table_Name = Ta_"
                      & Capitalize (FK.To_Table) & " then");

            S1 := First (FK.From_Attributes);
            S2 := First (FK.To_Attributes);
            while Has_Element (S1) loop
               if S1 = First (FK.From_Attributes) then
                  Put (Body_File, "         return Self.");
               else
                  Put (Body_File, "            and Self.");
               end if;

               Put (Body_File,
                    Capitalize (Element (S1))
                    & " = T_" & Capitalize (FK.To_Table)
                    & " (Foreign)."
                    & Capitalize (Element (S2)));

               if S1 = Last (FK.From_Attributes) then
                  Put_Line (Body_File, ";");
               else
                  New_Line (Body_File);
               end if;

               Next (S1);
               Next (S2);
            end loop;

            Put_Line (Body_File, "      end if;");
            Next (K);
         end loop;

         Put_Line (Body_File, "      return No_Criteria;");
         Put_Line (Body_File, "   end FK;");
      end if;

      Next (C);
   end loop;

   --  Print footer

   Put_Line (Spec_File, "end Database;");
   Put_Line (Body_File, "end Database;");

   Close (Spec_File);
   Close (Body_File);
end Generate;
