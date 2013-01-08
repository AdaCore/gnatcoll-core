------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2013, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;           use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Text_IO;                 use Ada.Text_IO;
with GNAT.Strings;                use GNAT.Strings;
with GNATCOLL.Mmap;               use GNATCOLL.Mmap;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

package body GNATCOLL.SQL.Inspect is
   Me : constant Trace_Handle := Create ("SQL.INSPECT");

   use Tables_Maps, Field_Lists, Foreign_Refs;
   use Foreign_Keys, Pair_Lists, Tables_Lists;

   Invalid_Schema : exception;

   Deferred_FK : constant String := " DEFERRABLE INITIALLY DEFERRED";
   --  extra attribute set on foreign keys. If we want to check the constraints
   --  immediately, this should be set to "".

   use String_Lists;

   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash_Case_Insensitive,
      Ada.Strings.Equal_Case_Insensitive,
      Ada.Strings.Equal_Case_Insensitive);
   use String_Sets;

   Keywords : String_Sets.Set;

   Max_Fields_Per_Line : constant := 30;
   --  Maximum number of fields per line (separated by '|')

   procedure Parse_Line
     (Line         : in out String_List;
      Line_Number  : in out Natural;
      Fields_Count : out Natural;
      Data         : String;
      First        : in out Integer;
      Replace_Newline : Boolean := True);
   --  Parse the current line and set Line and Fields_Count as appropriate.
   --  Fields_Count is set to 0 if the current line is not part of a table
   --  and should be ignored.

   procedure Parse_Table
     (Self        : DB_Schema_IO'Class;
      Table       : Table_Description;
      Attributes  : in out Field_List);
   --  Get the attributes of the specified table

   procedure Mark_FK_As_Ambiguous
     (Table     : in out Table_Description;
      Foreign   : Table_Description;
      Ambiguous : out Boolean);
   --  Mark all foreign keys from Table to Foreign as ambiguous (ie there are
   --  multiple references to the same foreign table, so we need special care
   --  in code generation). Ambiguous is set to False if there was no such
   --  FK yet.

   function Get_To (FK : Foreign_Key; Pair : Field_Pair) return Field;
   --  Return the field this points to (possibly the primary key of another
   --  table if FK.To is unset)

   function EOW (Str : String; First : Integer) return Natural;
   --  Return the position of the next '|'

   procedure Append
     (List : in out String_List; Last : in out Natural; Str : String);
   --  Add a new element to the list

   procedure Format_Field
     (DB       : access Database_Connection_Record'Class;
      Value    : String;
      Typ      : Field_Type;
      Val      : out GNAT.Strings.String_Access;
      Param    : out SQL_Parameter;
      Has_Xref : Boolean);
   --  Format a value for proper use in SQL.
   --  This translates boolean values "true" and "false" as appropriate for
   --  the backend.

   ---------
   -- EOW --
   ---------

   function EOW (Str : String; First : Integer) return Natural is
   begin
      return Find_Char (Str (First .. Str'Last), '|');
   end EOW;

   ------------
   -- Get_To --
   ------------

   function Get_To (FK : Foreign_Key; Pair : Field_Pair) return Field is
   begin
      if Pair.To = No_Field then
         return Get_PK (FK.To_Table);
      else
         return Pair.To;
      end if;
   end Get_To;

   --------
   -- Id --
   --------

   function Id (Self : Field) return Positive is
   begin
      return Self.Get.Id;
   end Id;

   ----------
   -- Name --
   ----------

   function Name (Self : Field) return String is
   begin
      return Self.Get.Name.all;
   end Name;

   -----------------
   -- Description --
   -----------------

   function Description (Self : Field) return String is
      Descr : constant GNAT.Strings.String_Access := Self.Get.Description;
   begin
      if Descr = null then
         return "";
      else
         return Descr.all;
      end if;
   end Description;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table (Self : Field) return Table_Description'Class is
   begin
      return Table_Description'
        (Tables_Ref.Get (Self.Get.Table) with null record);
   end Get_Table;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Self : Field) return Field_Type is
      D  : constant Fields_Ref.Encapsulated_Access := Self.Get;
      FK : constant Field := Self.Is_FK;
      T  : Field_Type;
   begin
      if FK = No_Field then
         return D.Typ;
      else
         T := Get_Type (FK);
         if T.Kind = Field_Autoincrement then
            T := (Kind => Field_Integer);
         end if;

         return T;
      end if;
   end Get_Type;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active (Self : in out Field; Active : Boolean) is
   begin
      Self.Get.Active := Active;
   end Set_Active;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Self : Field) return Boolean is
   begin
      return Self.Get.Active;
   end Is_Active;

   -----------------
   -- Can_Be_Null --
   -----------------

   function Can_Be_Null (Self : Field) return Boolean is
   begin
      return not Self.Get.Props.Not_Null;
   end Can_Be_Null;

   -------------
   -- Default --
   -------------

   function Default (Self : Field) return String is
      Def : constant GNAT.Strings.String_Access := Self.Get.Default;
   begin
      if Def = null then
         return "";
      else
         return Def.all;
      end if;
   end Default;

   -----------
   -- Is_PK --
   -----------

   function Is_PK (Self : Field) return Boolean is
   begin
      return Self.Get.Props.PK;
   end Is_PK;

   ------------
   -- Get_PK --
   ------------

   function Get_PK (Self : Table_Description'Class) return Field is
      F  : Field_Lists.Cursor := TDR (Self.Get).Fields.First;
      PK : Field := No_Field;
   begin
      while Has_Element (F) loop
         if Element (F).Get.Props.PK then
            if PK = No_Field then
               PK := Element (F);
            else
               return No_Field;  --  Primary key is a tuple
            end if;
         end if;

         Next (F);
      end loop;

      return PK;
   end Get_PK;

   -----------
   -- Is_FK --
   -----------

   function Is_FK (Self : Field) return Field is
      T : Table_Description;
      C : Foreign_Keys.Cursor;
   begin
      if Self.Get.FK then
         T := Table_Description (Get_Table (Self));
         C := TDR (T.Get).FK.First;
         while Has_Element (C) loop
            declare
               FK : constant Foreign_Refs.Encapsulated_Access :=
                 Element (C).Get;
               A : Pair_Lists.Cursor := FK.Fields.First;
               P : Field_Pair;
            begin
               while Has_Element (A) loop
                  P := Element (A);
                  if P.From = Self then
                     return Get_To (Element (C), P);
                  end if;

                  Next (A);
               end loop;
            end;

            Next (C);
         end loop;
      end if;

      return No_Field;
   end Is_FK;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Field_Description) is
   begin
      Free (Self.Name);
      Free (Self.Description);
      Free (Self.Default);
   end Free;

   --------
   -- Id --
   --------

   function Id (Self : Table_Description) return Positive is
   begin
      return TDR (Self.Get).Id;
   end Id;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Self : Table_Description) return Relation_Kind is
   begin
      return TDR (Self.Get).Kind;
   end Get_Kind;

   ----------
   -- Name --
   ----------

   function Name (Self : Table_Description) return String is
   begin
      return TDR (Self.Get).Name.all;
   end Name;

   --------------
   -- Row_Name --
   --------------

   function Row_Name (Self : Table_Description) return String is
      Row : constant GNAT.Strings.String_Access := TDR (Self.Get).Row;
   begin
      if Row = null then
         return TDR (Self.Get).Name.all;
      else
         return Row.all;
      end if;
   end Row_Name;

   -----------------
   -- Description --
   -----------------

   function Description (Self : Table_Description) return String is
      Descr : constant GNAT.Strings.String_Access :=
        TDR (Self.Get).Description;
   begin
      if Descr = null then
         return "";
      else
         return Descr.all;
      end if;
   end Description;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Self : Table_Description) return Boolean is
   begin
      return TDR (Self.Get).Is_Abstract;
   end Is_Abstract;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active (Self : in out Table_Description; Active : Boolean) is
   begin
      TDR (Self.Get).Active := Active;
   end Set_Active;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Self : Table_Description) return Boolean is
   begin
      return TDR (Self.Get).Active;
   end Is_Active;

   -----------------
   -- Super_Table --
   -----------------

   function Super_Table (Self : Table_Description) return Table_Description is
   begin
      return TDR (Self.Get).Super_Table;
   end Super_Table;

   --------------------
   -- For_Each_Field --
   --------------------

   procedure For_Each_Field
     (Self              : Table_Description;
      Callback          : access procedure (F : in out Field);
      Include_Inherited : Boolean := False)
   is
      C      : Field_Lists.Cursor := TDR (Self.Get).Fields.First;
      F      : Field;
   begin
      while Has_Element (C) loop
         F := Element (C);
         Callback (F);
         Next (C);
      end loop;

      if Include_Inherited
        and then TDR (Self.Get).Super_Table /= No_Table
      then
         For_Each_Field (Self.Super_Table, Callback, Include_Inherited);
      end if;
   end For_Each_Field;

   ---------------------
   -- Field_From_Name --
   ---------------------

   function Field_From_Name
     (Self  : Table_Description'Class; Name  : String) return Field
   is
      Result : Field := No_Field;

      procedure For_Field (F : in out Field);
      procedure For_Field (F : in out Field) is
      begin
         if F.Name = Name then
            Result := F;
         end if;
      end For_Field;

   begin
      For_Each_Field (Self, For_Field'Access, Include_Inherited => True);
      return Result;
   end Field_From_Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Foreign_Key_Description) is
   begin
      Free (Self.Revert_Name);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Table_Description_Record) is
   begin
      Free (Self.Name);
      Free (Self.Row);
      Free (Self.Description);
   end Free;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table
     (Self : DB_Schema; Name : String) return Table_Description
   is
      C : constant Tables_Maps.Cursor := Self.Tables.Find (Name);
   begin
      if C = Tables_Maps.No_Element then
         raise Invalid_Table with
           "No such table: " & Name;
      end if;

      return Element (C);
   end Get_Table;

   --------------------
   -- For_Each_Table --
   --------------------

   procedure For_Each_Table
     (Self     : DB_Schema;
      Callback : access procedure (T : in out Table_Description);
      Alphabetical : Boolean := True)
   is
      T : Table_Description;
   begin
      if Alphabetical
        or else Self.Ordered_Tables.Is_Empty
      then
         declare
            C : Tables_Maps.Cursor := Self.Tables.First;
         begin
            while Has_Element (C) loop
               T := Element (C);
               Callback (T);
               Next (C);
            end loop;
         end;

      else
         declare
            C : Tables_Lists.Cursor := Self.Ordered_Tables.First;
         begin
            while Has_Element (C) loop
               T := Self.Tables.Element (Element (C));
               Callback (T);
               Next (C);
            end loop;
         end;
      end if;
   end For_Each_Table;

   -----------------
   -- For_Each_FK --
   -----------------

   procedure For_Each_FK
     (Self     : Table_Description;
      Callback : access procedure
        (From, To : Field; Id : Natural; Ambiguous : Boolean))
   is
      F  : Foreign_Keys.Cursor;
      P  : Pair_Lists.Cursor;
      Id : Integer := 1;
   begin
      F := TDR (Self.Get).FK.First;
      while Has_Element (F) loop
         P := Element (F).Get.Fields.First;
         while Has_Element (P) loop
            Callback (From      => Element (P).From,
                      To        => Get_To (Element (F), Element (P)),
                      Ambiguous => Element (F).Get.Ambiguous,
                      Id        => Id);
            Next (P);
         end loop;

         Id := Id + 1;
         Next (F);
      end loop;
   end For_Each_FK;

   ------------
   -- To_SQL --
   ------------

   function To_SQL
     (Typ          : Field_Type;
      For_Database : Boolean := True) return String is
   begin
      case Typ.Kind is
         when Field_Boolean => return "Boolean";
         when Field_Text    =>
            if Typ.Max_Length = Integer'Last
              or not For_Database
            then
               return "Text";
            else
               return "Character(" & Image (Typ.Max_Length, 1) & ")";
            end if;
         when Field_Json    => return "Json";
         when Field_Integer => return "Integer";
         when Field_Date    => return "Date";
         when Field_Timestamp =>
            if For_Database then
               return "timestamp with time zone";
            else
               return "Time";
            end if;
         when Field_Time    => return "Time";
         when Field_Float   => return "Float";
         when Field_Autoincrement =>
            --  These types are always mapped to an integer in all DBMS,
            --  even though they might be created with a different name like
            --  "SERIAL" and "INTEGER AUTOINCREMENT".
            return "Integer";
         when Field_Money   => return "Money";
      end case;
   end To_SQL;

   --------------
   -- From_SQL --
   --------------

   function From_SQL (SQL_Type : String) return Field_Type is
      T     : constant String := To_Lower (SQL_Type);
   begin
      if T = "boolean" then
         return (Kind => Field_Boolean);

      elsif T = "text"
         or else (T'Length >= 10    --  "character varying(...)"
                  and then T (T'First .. T'First + 9) = "character ")
      then
         return (Kind => Field_Text, Max_Length => Integer'Last);

      elsif T = "varchar" then
         return (Kind => Field_Text, Max_Length => Integer'Last);

      elsif T'Length >= 8
         and then T (T'First .. T'First + 7) = "varchar("
      then
         begin
            return (Kind => Field_Text,
                    Max_Length =>
                      Integer'Value (T (T'First + 10 .. T'Last - 1)));
         exception
            when Constraint_Error =>
               Put_Line ("Missing max length after 'varchar' in " & T);
               raise Invalid_Schema;
         end;

      elsif T'Length >= 10
        and then T (T'First .. T'First + 9) = "character("
      then
         begin
            return (Kind => Field_Text,
                    Max_Length =>
                      Integer'Value (T (T'First + 10 .. T'Last - 1)));
         exception
            when Constraint_Error =>
               Put_Line ("Missing max length after 'Character' in " & T);
               raise Invalid_Schema;
         end;

      elsif T = "json" then
         return (Kind => Field_Json);

      elsif T = "float" then
         return (Kind => Field_Float);

      elsif T = "integer"
        or else T = "smallint"
        or else T = "oid"
        or else (T'Length >= 7 and then T (T'First .. T'First + 6) = "numeric")
      then
         return (Kind => Field_Integer);

      elsif T = "date" then
         return (Kind => Field_Date);

      elsif T = "timestamp without time zone"
        or else T = "timestamp with time zone"
        or else T = "timestamp"
      then
         return (Kind => Field_Timestamp);

      elsif T = "time" then
         return (Kind => Field_Time);

      elsif T = "double precision" then
         return (Kind => Field_Float);

      elsif T = "autoincrement" then
         return (Kind => Field_Autoincrement);

      elsif T = "money" then
         return (Kind => Field_Money);
      else
         raise Invalid_Type
           with "Cannot convert """ & T & """ to Ada";
      end if;
   end From_SQL;

   -----------------
   -- Parse_Table --
   -----------------

   procedure Parse_Table
     (Self        : DB_Schema_IO'Class;
      Table       : Table_Description;
      Attributes  : in out Field_List)
   is
      procedure On_Field
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean);
      --  Called when a new field is discovered

      procedure On_Field
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean)
      is
         Descr : Field_Description;
         Ref   : Field;
      begin
         Descr := Field_Description'
           (Weak_Refcounted with
            Name        => new String'(Name),
            Typ         => From_SQL (Typ),
            Id          => Index,
            Description => new String'(Description),
            Default     => null,
            Props       => (PK       => Is_Primary_Key,
                            Indexed  => False,
                            Noindex  => False,
                            Not_Null => Not_Null or else Is_Primary_Key,
                            Case_Insensitive => False),
            FK          => False,
            Table       => Tables_Ref.Get_Weak_Ref (Table),
            Active      => True);

         if Default_Value'Length < 8
           or else Default_Value
             (Default_Value'First .. Default_Value'First + 7)
           /= "nextval("
         then
            Descr.Default  := new String'(Default_Value);
         end if;

         Set (Ref, Descr);
         Append (Attributes, Ref);
      end On_Field;

   begin
      Foreach_Field
        (Self.DB,
         Table_Name => Table.Name,
         Callback   => On_Field'Access);
   end Parse_Table;

   -----------------
   -- Read_Schema --
   -----------------

   overriding function Read_Schema
     (Self : DB_Schema_IO) return DB_Schema
   is
      Schema : DB_Schema;
      T : Natural := 0;

      procedure On_Table (Name, Description : String; Kind : Relation_Kind);
      --  Called when a new table is discovered

      procedure Compute_Foreign_Keys
        (Name  : String; Table : in out Table_Description);
      --  Compute the foreign keys for a specific table

      function Field_From_Index
        (Descr     : Table_Description;
         Index     : Natural) return Field;
      --  Return the field given its index in the table. Information
      --  is extracted from All_Attrs

      --------------
      -- On_Table --
      --------------

      procedure On_Table (Name, Description : String; Kind : Relation_Kind) is
         Descr : Table_Description_Record;
         Ref   : Table_Description;
      begin
         T := T + 1;
         Descr.Id          := T;
         Descr.Kind        := Kind;
         Descr.Name        := new String'(Name);
         Descr.Row         := null;  --  Will default to Descr.Name
         Descr.Description := new String'(Description);
         Set (Ref, Descr);

         Parse_Table (Self, Ref, TDR (Ref.Get).Fields);

         Insert (Schema.Tables, Name, Ref);
         Schema.Ordered_Tables.Append (Name);
      end On_Table;

      ----------------------
      -- Field_From_Index --
      ----------------------

      function Field_From_Index
        (Descr     : Table_Description;
         Index     : Natural) return Field
      is
         A : Field_Lists.Cursor := First (TDR (Descr.Get).Fields);
      begin
         while Has_Element (A) loop
            if Element (A).Id = Index then
               return Element (A);
            end if;
            Next (A);
         end loop;
         return No_Field;
      end Field_From_Index;

      --------------------------
      -- Compute_Foreign_Keys --
      --------------------------

      procedure Compute_Foreign_Keys
        (Name  : String; Table : in out Table_Description)
      is
         Prev_Index : Integer := -1;
         To_Table   : Table_Description;
         Descr      : Foreign_Key_Description;
         R          : Foreign_Key;

         procedure On_Key
           (Index             : Positive;
            Local_Attribute   : Integer;
            Foreign_Table     : String;
            Foreign_Attribute : Integer);
         --  Called for each foreign key in the table

         procedure On_Key
           (Index             : Positive;
            Local_Attribute   : Integer;
            Foreign_Table     : String;
            Foreign_Attribute : Integer)
         is
            From : Field;
         begin
            if Prev_Index /= Index then
               --  A new foreign key, as opposed to a new attribute in the same
               --  key

               if Prev_Index /= -1 then
                  Set (R, Descr);
                  Append (TDR (Table.Get).FK, R);
               end if;

               Prev_Index := Index;

               To_Table := Get_Table (Schema, Foreign_Table);
               Descr :=
                 (Refcounted with
                  To_Table        => Tables_Ref.Get_Weak_Ref (To_Table),
                  Revert_Name     => null,
                  Fields          => Pair_Lists.Empty_List,
                  Ambiguous       => False);

               Mark_FK_As_Ambiguous (Table, To_Table, Descr.Ambiguous);
            end if;

            From := Field_From_Index (Table, Local_Attribute);
            From.Get.FK := True;
            Append
              (Descr.Fields,
               Field_Pair'
                 (From => From,
                  To   => Field_From_Index (To_Table, Foreign_Attribute)));
         end On_Key;

      begin
         Foreach_Foreign_Key
           (Self.DB,
            Table_Name => Name,
            Callback   => On_Key'Access);

         if Prev_Index /= -1 then
            Set (R, Descr);
            Append (TDR (Table.Get).FK, R);
         end if;
      end Compute_Foreign_Keys;

      C : Tables_Maps.Cursor;

   begin
      Foreach_Table (Self.DB, On_Table'Access);

      C := First (Schema.Tables);
      while Has_Element (C) loop
         Update_Element (Schema.Tables, C, Compute_Foreign_Keys'Access);
         Next (C);
      end loop;

      return Schema;
   end Read_Schema;

   --------------
   -- To_Table --
   --------------

   function To_Table (FK : Foreign_Key) return Table_Description'Class is
   begin
      return Table_Description'
        (Tables_Ref.Get (FK.Get.To_Table) with null record);
   end To_Table;

   --------------------------
   -- Mark_FK_As_Ambiguous --
   --------------------------

   procedure Mark_FK_As_Ambiguous
     (Table     : in out Table_Description;
      Foreign   : Table_Description;
      Ambiguous : out Boolean)
   is
      C     : Foreign_Keys.Cursor := First (TDR (Table.Get).FK);
      FK    : Foreign_Key;
   begin
      Ambiguous := False;

      while Has_Element (C) loop
         FK := Element (C);

         if TDR (Tables_Ref.Get (FK.Get.To_Table).Get) = TDR (Foreign.Get) then
            if not FK.Get.Ambiguous then
               FK.Get.Ambiguous := True;
               Replace_Element (TDR (Table.Get).FK, C, FK);
            end if;

            Ambiguous := True;
            return;
         end if;
         Next (C);
      end loop;
   end Mark_FK_As_Ambiguous;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out String_List; Last : in out Natural; Str : String) is
   begin
      Last := Last + 1;
      List (Last) := new String'(Str);
   end Append;

   ------------------
   -- Format_Field --
   ------------------

   procedure Format_Field
     (DB       : access Database_Connection_Record'Class;
      Value    : String;
      Typ      : Field_Type;
      Val      : out GNAT.Strings.String_Access;
      Param    : out SQL_Parameter;
      Has_Xref : Boolean)
   is
      V : constant String := To_Lower (Value);
      B : Boolean;
   begin
      if Typ.Kind = Field_Boolean then
         if V = "true" or else V = "false" then
            B := Boolean'Value (Value);
            Val := new String'(Boolean_Image (DB.all, B));
            Param := +B;
         else
            Val := new String'(Value);
            Param := +Val;
         end if;

      elsif Value'Length = 0 then
         if Has_Xref then
            Val := new String'("''");
         else
            Val := new String'("");
         end if;
         Param := +Val;

      elsif V = "null" then
         Val := new String'("NULL");
         if Has_Xref then
            Param := +Val;
         else
            Param := Null_Parameter;
         end if;

      elsif Typ.Kind = Field_Integer then
         Val := new String'(Value);
         Param := +Val;

      elsif Typ.Kind = Field_Money then
         Param := +T_Money'Value (Value);
         Val := new String'(Image (DB.all, Param));

      else
         if Has_Xref then
            Val := new String'
              (String_To_SQL (DB.all, Value, Quote => True));
         else
            Val := new String'(Value);
         end if;

         Param := +Val;
      end if;
   end Format_Field;

   -----------------
   -- Read_Schema --
   -----------------

   function Read_Schema
     (Self : File_Schema_IO; Data : String) return DB_Schema
   is

      Schema : DB_Schema;
      T     : Natural := 0;  --  Index of the table we are creating
      First : Natural; --  Current index in Data
      Line_Number : Natural := 0;

      Fields_Per_Line : constant := 5;
      --  Maximum number of fields per line (fields are separated with |)

      type Line_Fields is new String_List (1 .. Fields_Per_Line);

      procedure Parse_Line (Result : in out Line_Fields);
      --  Split the line that starts at First into its fields.
      --  On exit, First points to the beginning of the next line

      procedure Parse_Table (Table_Def, Name : String; Is_View : Boolean);
      --  Parse a table description

      procedure Parse_Table_Inheritance
        (Table_Def : String; Table : in out Table_Description);
      --  Parse the description of table inheritance

      procedure Parse_Table_Properties (Name : String);
      --  Parse all foreign keys and indices for table Name, when they are
      --  described on their own line

      function Parse_Properties (Str : String) return Field_Properties;
      --  Parse the third column of a field description

      ----------------------
      -- Parse_Properties --
      ----------------------

      function Parse_Properties (Str : String) return Field_Properties is
         S : String_List_Access := Split (Str, On => ',');
         Props : Field_Properties;
      begin
         for P in S'Range loop
            declare
               T : constant String := Trim (S (P).all, Both);
            begin
               if T = "NOT NULL" then
                  Props.Not_Null := True;
               elsif T = "INDEX" then
                  Props.Indexed := True;
               elsif T = "NOINDEX" then
                  Props.Noindex := True;
               elsif T = "PK" then
                  Props.PK := True;
                  Props.Not_Null := True;
               elsif T = "NOCASE" then
                  Props.Case_Insensitive := True;
               end if;
            end;
         end loop;

         Free (S);
         return Props;
      end Parse_Properties;

      ----------------
      -- Parse_Line --
      ----------------

      procedure Parse_Line (Result : in out Line_Fields) is
         Index  : Natural := Result'First - 1;
         Last, Tmp : Natural;
         Current_Line_End : constant Natural :=
           EOL (Data (First .. Data'Last));
      begin
         pragma Assert (Data (First) = '|');
         Line_Number := Line_Number + 1;

         Free (String_List (Result));

         First := First + 1;

         while First <= Current_Line_End loop
            Skip_Blanks (Data, First);
            --  First now points to first non-blank char

            Last := EOW (Data, First);
            Tmp := Last - 1;

            Skip_Blanks_Backward (Data (First .. Tmp), Tmp);

            Append (String_List (Result), Index, Data (First .. Tmp));
            exit when Index = Fields_Per_Line;

            First := Last + 1;
         end loop;

         First := Current_Line_End + 1;
      end Parse_Line;

      -----------------------------
      -- Parse_Table_Inheritance --
      -----------------------------

      procedure Parse_Table_Inheritance
        (Table_Def : String; Table : in out Table_Description)
      is
         First : Natural := Table_Def'First;
         Last  : Natural;
      begin
         while First <= Table_Def'Last loop
            if Table_Def (First) = '(' then
               Last := First + 1;
               while Last <= Table_Def'Last loop
                  if Table_Def (Last) = ')' then
                     TDR (Table.Get).Super_Table :=
                       Get_Table (Schema, Table_Def (First + 1 .. Last - 1));
                     return;
                  end if;
                  Last := Last + 1;
               end loop;
            end if;

            First := First + 1;
         end loop;
      end Parse_Table_Inheritance;

      -----------------
      -- Parse_Table --
      -----------------

      procedure Parse_Table (Table_Def, Name : String; Is_View : Boolean) is
         Table : Table_Description;
         Line  : Line_Fields;
         Attr_Id : Natural := 0;
         Props : Field_Properties;
         Kind : Relation_Kind;
      begin
         T := T + 1;

         --  The code below might be creating table before we actually see
         --  their schema (in the case of FK to them). Reuse the prior
         --  definition if one is found.
         declare
            C : Tables_Maps.Cursor;
         begin
            C := Find (Schema.Tables, Name);
            if C = Tables_Maps.No_Element then
               if Is_View then
                  Kind := Kind_View;
               else
                  Kind := Kind_Table;
               end if;

               Set (Table, Table_Description_Record'
                   (Weak_Refcounted with
                    Name        => new String'(Name),
                    Row         => null,
                    Kind        => Kind,
                    Id          => T,
                    Description => null,
                    Fields      => Empty_Field_List,
                    Indexes     => String_Lists.Empty_List,
                    Is_Abstract => False,
                    Has_PK      => False,
                    FK          => Foreign_Keys.Empty_List,
                    Active      => True,
                    Super_Table => No_Table));
            else
               Table := Element (C);
            end if;

            TDR (Table.Get).Is_Abstract := Starts_With (Table_Def, "ABSTRACT");
         end;

         Parse_Table_Inheritance (Table_Def, Table);

         while First <= Data'Last and then Data (First) = '|' loop
            Parse_Line (Result => Line);

            if Starts_With (Line (1).all, "--") then
               --  A comment line, skip this line
               null;

            elsif Line (1).all = "FK:"
              or else Line (1).all = "INDEX:"
            then
               null;   --  Skip for now, will do in second pass

            else
               Attr_Id := Attr_Id + 1;

               Props := Parse_Properties (Line (3).all);

               declare
                  Typ       : String renames Line (2).all;
                  Tmp, Tmp2 : Natural;

                  Att : Field;
                  FKD : Foreign_Key_Description;
                  FK  : Foreign_Key;
                  To_Table : Table_Description;

               begin
                  TDR (Table.Get).Has_PK :=
                    Props.PK or else TDR (Table.Get).Has_PK;

                  Set (Att, Field_Description'
                         (Weak_Refcounted with
                          Name        => new String'(Line (1).all),
                          Typ         => (Kind => Field_Boolean), --  Set below
                          Id          => Attr_Id,
                          Description => new String'(Line (5).all),
                          Default     => new String'(Line (4).all),
                          Props       => Props,
                          FK          => Typ'Length > 3
                            and then Typ (Typ'First .. Typ'First + 2) = "FK ",
                          Table       => Tables_Ref.Get_Weak_Ref (Table),
                          Active      => True));
                  Append (TDR (Table.Get).Fields, Att);

                  if Att.Get.FK then
                     Tmp := Find_Char (Typ (Typ'First + 3 .. Typ'Last), '(');

                     if Tmp < Typ'Last then
                        Tmp2 := Find_Char (Typ (Tmp + 1 .. Typ'Last), ')');
                     else
                        Tmp2 := Typ'Last;
                     end if;

                     declare
                        To : constant String :=
                          Trim (Typ (Typ'First + 3 .. Tmp - 1), Both);
                     begin
                        if To = Name then
                           To_Table := Table;
                        else
                           To_Table := Get_Table (Schema, To);
                        end if;

                     exception
                        when Invalid_Table =>
                           --  The table might be declared later on
                           Set (To_Table, Table_Description_Record'
                                  (Weak_Refcounted with
                                   Name   => new String'(To),
                                   others => <>));
                           Include (Schema.Tables, To, To_Table);
                     end;

                     FKD := Foreign_Key_Description'
                       (Refcounted with
                        To_Table    => Tables_Ref.Get_Weak_Ref (To_Table),
                        Revert_Name => new String'(Typ (Tmp + 1 .. Tmp2 - 1)),
                        Fields      => Pair_Lists.Empty_List,
                        Ambiguous   => False);
                     Mark_FK_As_Ambiguous (Table, To_Table, FKD.Ambiguous);

                     Append (FKD.Fields,
                             Field_Pair'
                               (From => Att,
                                To   => No_Field));   --  To primary key

                     Set (FK, FKD);
                     Append (TDR (Table.Get).FK, FK);

                     Att.Get.FK := True;

                  else
                     Att.Get.Typ := From_SQL (Typ);
                  end if;
               end;
            end if;
         end loop;

         --  Check that the table has a valid Primary Key
         --  ??? Code is commented out for reference in case we decide to
         --  output such a warning after all. For now, since there is no way
         --  to hide the warning for the user, this is too verbose.

--           if not TDR (Table.Get).Has_PK
--             and then not Table.Is_Abstract
--             and then (Table.Super_Table = No_Table
--                       or else not TDR (Table.Super_Table.Get).Has_PK)
--           then
--              Put_Line ("Warning: table '"
--                        & Table.Name & "' has no primary key");
--              Put_Line ("    No Delete operation generated for this table");
--           end if;

         Free (String_List (Line));
         Include (Schema.Tables, Name, Table);
         Schema.Ordered_Tables.Append (Name);
      end Parse_Table;

      ----------------------------
      -- Parse_Table_Properties --
      ----------------------------

      procedure Parse_Table_Properties (Name : String) is
         Curs  : constant Tables_Maps.Cursor := Schema.Tables.Find (Name);
         From_Table : Table_Description := Element (Curs);
         To_Table : Table_Description;
         FK    : Foreign_Key;
         Line  : Line_Fields;
         Index_Count : Natural := 1;
      begin
         while First <= Data'Last and then Data (First) = '|' loop
            Parse_Line (Result => Line);

            if Line (1).all = "FK:" then
               To_Table := Get_Table (Schema, Line (2).all);
               Set (FK, Foreign_Key_Description'
                    (Refcounted with
                     To_Table        => Tables_Ref.Get_Weak_Ref (To_Table),
                     Revert_Name     => null,
                     Ambiguous       => False,
                     Fields          => Pair_Lists.Empty_List));
               Mark_FK_As_Ambiguous (From_Table, To_Table, FK.Get.Ambiguous);

               declare
                  From : String renames Line (3).all;
                  To   : String renames Line (4).all;
                  First, First2, Tmp, Tmp2 : Natural;
               begin
                  First  := From'First;
                  First2 := To'First;

                  while First <= From'Last loop
                     Skip_Blanks (From, First);
                     Skip_Blanks (To, First2);

                     Tmp := Find_Char (From (First + 1 .. From'Last), ' ');
                     Tmp2 := Find_Char (To (First2 + 1 .. To'Last), ' ');

                     Append (FK.Get.Fields,
                             (From => From_Table.Field_From_Name
                                        (From (First .. Tmp - 1)),
                              To   => To_Table.Field_From_Name
                                        (To (First2 .. Tmp2 - 1))));
                     First  := Tmp + 1;
                     First2 := Tmp2 + 1;
                  end loop;
               end;

               TDR (From_Table.Get).FK.Append (FK);

            elsif Line (1).all = "INDEX:" then
               if Line (3).all = "" then
                  TDR (From_Table.Get).Indexes.Append
                    (Line (2).all
                     & "|"
                     & Name & "_idx"
                     & Image (Index_Count, Min_Width => 1));
               else
                  TDR (From_Table.Get).Indexes.Append
                    (Line (2).all & "|" & Line (3).all);
               end if;

               Index_Count := Index_Count + 1;
            end if;
         end loop;

         Free (String_List (Line));
         Replace_Element (Schema.Tables, Curs, From_Table);
      end Parse_Table_Properties;

      Line : Line_Fields;
      type Parse_Mode is (Parsing_Table, Parsing_Properties);

   begin
      for Mode in Parse_Mode loop
         First := Data'First;
         Line_Number := 0;

         while First <= Data'Last loop
            if Data (First) = '|' then
               Parse_Line (Result => Line);

               if Starts_With (Line (1).all, "ABSTRACT TABLE")
                 or else Starts_With (Line (1).all, "TABLE")
                 or else Starts_With (Line (1).all, "VIEW")
               then
                  case Mode is
                     when Parsing_Table =>
                        Parse_Table
                          (Line (1).all,
                           Line (2).all,
                           Is_View => Starts_With (Line (1).all, "VIEW"));
                     when Parsing_Properties    =>
                        Parse_Table_Properties (Line (2).all);
                  end case;
               end if;
            else
               First := EOL (Data (First .. Data'Last)) + 1;
               Line_Number := Line_Number + 1;
            end if;
         end loop;
      end loop;

      Free (String_List (Line));
      return Schema;

   exception
      when E : Invalid_Type =>
         Free (String_List (Line));
         Put_Line (Standard_Error,
                   Self.File.Display_Full_Name
                   & ":" & Image (Line_Number, Min_Width => 1) & " "
                   & Exception_Message (E));
         raise;

      when Name_Error =>
         Put_Line ("Could not open " & Self.File.Display_Full_Name);
         return No_Schema;
   end Read_Schema;

   -----------------
   -- Read_Schema --
   -----------------

   overriding function Read_Schema
     (Self : File_Schema_IO) return DB_Schema
   is
      Str    : GNAT.Strings.String_Access := Self.File.Read_File;
      Schema : DB_Schema;
   begin
      Schema := Read_Schema (Self, Str.all);
      Free (Str);
      return Schema;

   exception
      when others =>
         Free (Str);
         raise;
   end Read_Schema;

   ------------------
   -- Write_Schema --
   ------------------

   overriding procedure Write_Schema
     (Self : DB_Schema_IO; Schema : DB_Schema)
   is
      Created : String_Lists.List;
      --  List of tables that have been created. When a table has already been
      --  created, we set the foreign key constraints to it immediately,
      --  otherwise we defer them till all tables have been created.

      Deferred         : String_Lists.List;
      Deferred_Indexes : String_Lists.List;
      --  Statements to execute to create the indexes

      procedure For_Table (Table : in out Table_Description);
      --  Process a table

      procedure Do_Statement (SQL : String);
      --  Execute or output the statement, depending on user's choice

      ------------------
      -- Do_Statement --
      ------------------

      procedure Do_Statement (SQL : String) is
      begin
         if SQL /= "" then
            if Self.DB = null then
               Put_Line (SQL & ";");
            else
               Execute (Self.DB, SQL);
            end if;
         end if;
      end Do_Statement;

      ---------------
      -- For_Table --
      ---------------

      procedure For_Table (Table : in out Table_Description) is
         SQL : Unbounded_String;
         --  The statement to execute

         SQL_PK : Unbounded_String;
         --  The SQL to create the primary key

         Is_First_Attribute : Boolean := True;

         procedure Print_PK (F : in out Field);
         procedure Add_Field_To_SQL (F : in out Field);

         procedure Get_Field_Def
           (F    : Field;
            Stmt : out Unbounded_String;
            Can_Be_Not_Null : Boolean := True;
            FK_Table : String := "");
         --  Set Stmt to the definition for the field F.
         --  If Can_Be_Not_Null is False, the field will never have NOT NULL.
         --  This is needed in sqlite3 when adding FK columns later on. When
         --  Can_Be_Not_Null is set to False, you must set FK_Table to the
         --  table pointed to by the field.

         procedure Print_Indexes (Table : Table_Description);
         --  Create the multi-column indexes for the table

         procedure Print_FK (Table : Table_Description);
         --  Process the foreign key and indexes constraints. They are either
         --  added to the table creation statement, or deferred until all
         --  tables have been created.

         -------------------
         -- Print_Indexes --
         -------------------

         procedure Print_Indexes (Table : Table_Description) is
            C : String_Lists.Cursor := TDR (Table.Get).Indexes.First;
         begin
            while Has_Element (C) loop
               declare
                  Descr : constant String := Element (C);
                  Name_Start : Integer := Descr'First + 1;
               begin
                  while Descr (Name_Start) /= '|' loop
                     Name_Start := Name_Start + 1;
                  end loop;

                  Append (Deferred_Indexes,
                          "CREATE INDEX """
                          & Descr (Name_Start + 1 .. Descr'Last)
                          & """ ON """
                          & Table.Name & """ ("
                          & Descr (Descr'First .. Name_Start - 1)
                          & ")");
               end;

               Next (C);
            end loop;
         end Print_Indexes;

         --------------
         -- Print_FK --
         --------------

         procedure Print_FK (Table : Table_Description) is
            Stmt2 : Unbounded_String;
            --  The deferred statement to execute

            Stmt_FK, Stmt_References : Unbounded_String;

            C : Foreign_Keys.Cursor := TDR (Table.Get).FK.First;
            F : Foreign_Refs.Encapsulated_Access;
            P : Pair_Lists.Cursor;
            Is_First : Boolean;
         begin
            while Has_Element (C) loop
               F := Element (C).Get;

               --  Prepare the constraint

               Stmt_FK := To_Unbounded_String (" FOREIGN KEY (");
               Is_First := True;
               P := F.Fields.First;
               while Has_Element (P) loop
                  if not Is_First then
                     Append (Stmt_FK, ",");
                  end if;
                  Is_First := False;
                  Append (Stmt_FK, '"' & Element (P).From.Name & '"');
                  Next (P);
               end loop;
               Append (Stmt_FK, ")");

               Stmt_References := To_Unbounded_String
                 (" REFERENCES """ & Element (C).To_Table.Name & """ (");
               Is_First := True;
               P := F.Fields.First;
               while Has_Element (P) loop
                  if not Is_First then
                     Append (Stmt_References, ",");
                  end if;
                  Is_First := False;

                  if Element (P).To = No_Field then
                     Append
                       (Stmt_References,
                        '"' & Element (C).To_Table.Get_PK.Name & '"');
                  else
                     Append
                       (Stmt_References, '"' & Element (P).To.Name & '"');
                  end if;
                  Next (P);
               end loop;
               Append (Stmt_References, ")");

               Append (Stmt_FK, Stmt_References);

               --  If the other table has already been created, we can add the
               --  new constraint directly in the table creation which is more
               --  efficient (a single SQL statement).

               if Created.Contains (Element (C).To_Table.Name) then
                  Append (SQL, "," & ASCII.LF & Stmt_FK);

               elsif Self.DB.Can_Alter_Table_Constraints then
                  Append
                    (Deferred,
                     To_String
                       ("ALTER TABLE """ & Table.Name & """ ADD CONSTRAINT "
                        & Element (F.Fields.First).From.Name & "_fk" & Stmt_FK
                        & Deferred_FK));

               else
                  P := F.Fields.First;
                  while Has_Element (P) loop
                     --  Sqlite only allows adding a NON NULL REFERENCES column
                     --  if it has a non-null default. So we need to provide a
                     --  random default in such a case.

                     Get_Field_Def (Element (P).From, Stmt2,
                                    Can_Be_Not_Null => False,
                                    FK_Table => Element (C).To_Table.Name);
                     Stmt2 := "ALTER TABLE """ & Table.Name & """ ADD COLUMN "
                       & Stmt2 & Stmt_References;
                     Append (Deferred, To_String (Stmt2));
                     Next (P);
                  end loop;
               end if;

               --  Create indexes for the reverse relationships, since it is
               --  likely the user will want to use them a lot anyway

               if Length (F.Fields) = 1

                 --  Unless already created explicitly
                 and not Element (F.Fields.First).From.Get.Props.Indexed

                 --  Unless disabled by the user
                 and not Element (F.Fields.First).From.Get.Props.Noindex
               then
                  Append (Deferred_Indexes,
                          "CREATE INDEX """
                          & Table.Name & "_"
                          & Element (F.Fields.First).From.Name
                          & "_idx"" ON """
                          & Table.Name & """ ("""
                          & Element (F.Fields.First).From.Name
                          & """)");
               end if;

               Next (C);
            end loop;
         end Print_FK;

         -------------------
         -- Get_Field_Def --
         -------------------

         procedure Get_Field_Def
           (F    : Field;
            Stmt : out Unbounded_String;
            Can_Be_Not_Null : Boolean := True;
            FK_Table : String := "")
         is
            Val : GNAT.Strings.String_Access;
            Val_Param : SQL_Parameter;
         begin
            Stmt := Null_Unbounded_String;

            if Get_Type (F).Kind = Field_Autoincrement then
               Append (Stmt, " """ & F.Name & """ "
                       & Field_Type_Autoincrement (Self.DB.all));

            elsif Get_Type (F).Kind = Field_Money then
               Append (Stmt, " """ & F.Name & """ "
                       & Field_Type_Money (Self.DB.all));
            else
               Append (Stmt, " """ & F.Name & """ " & To_SQL (Get_Type (F)));
            end if;

            if not F.Can_Be_Null then
               if not Can_Be_Not_Null then
                  Put_Line (Standard_Error,
                            "Warning: '" & F.Get_Table.Name
                            & "." & F.Name
                            & "' cannot be NOT NULL in sqlite, because it"
                            & " references '" & FK_Table
                            & "' which hasn't been defined yet." & ASCII.LF
                            & "  Try reordering the table definitions");
               else
                  Append (Stmt, " NOT NULL");
               end if;
            end if;

            if F.Get.Props.Case_Insensitive then
               Append (Stmt, " COLLATE NOCASE");
            end if;

            if F.Default /= "" then
               Format_Field
                  (Self.DB, F.Default, Get_Type (F), Val, Val_Param, False);
               Append (Stmt, " DEFAULT " & Val.all);
               Free (Val);

               if not Can_Be_Not_Null then
                  --  When adding FK fields to an existing table, sqlite only
                  --  allows a NULL default value.

                  Put_Line
                    (Standard_Error,
                     "Error: '" & F.Get_Table.Name & "." & F.Name
                     & "' is a reference to table '"
                     & F.Is_FK.Get_Table.Name
                     & "' which isn't defined yet. Sqlite imposes a NULL"
                     & " default in this case.");
                  raise Invalid_Schema;
               end if;
            end if;

            if F.Get.Props.Indexed then
               Append (Deferred_Indexes,
                       "CREATE INDEX """
                       & Table.Name & "_"
                       & F.Get.Name.all
                       & "_idx"" ON """
                       & Table.Name & """ ("""
                       & F.Get.Name.all
                       & """)");
            end if;
         end Get_Field_Def;

         ----------------------
         -- Add_Field_To_SQL --
         ----------------------

         procedure Add_Field_To_SQL (F : in out Field) is
            Tmp : Unbounded_String;
         begin
            --  When a field is a FK to a table that hasn't been created yet,
            --  we need to alter the table later to set the constraint. But in
            --  some cases (sqlite3), this isn't possible, so we will create
            --  the field later altogether.

            if not Self.DB.Can_Alter_Table_Constraints
              and then F.Is_FK /= No_Field
              and then not Created.Contains (F.Is_FK.Get_Table.Name)
            then
               if F.Is_PK then
                  Put_Line (Standard_Error,
                            "Error: '" & F.Get_Table.Name & "." & F.Name
                            & "' is a primary key and references the table '"
                            & F.Is_FK.Get_Table.Name & "' which hasn't been"
                            & " defined yet.");
                  raise Invalid_Schema;
               end if;
               return;
            end if;

            if not Is_First_Attribute then
               Append (SQL, "," & ASCII.LF);
            end if;

            Is_First_Attribute := False;

            Get_Field_Def (F, Tmp);
            Append (SQL, Tmp);
         end Add_Field_To_SQL;

         --------------
         -- Print_PK --
         --------------

         procedure Print_PK (F : in out Field) is
         begin
            --  Auto increment fields were already setup as primary keys
            --  via Field_Type_Autoincrement primitive operation.
            if F.Is_PK and then F.Get_Type.Kind /= Field_Autoincrement then
               if SQL_PK = Null_Unbounded_String then
                  Append (SQL_PK, '"' & F.Name & '"');
               else
                  Append (SQL_PK, ",""" & F.Name & '"');
               end if;
            end if;
         end Print_PK;

      begin
         if Self.DB.Success and then not Table.Is_Abstract then
            case Table.Get_Kind is
               when Kind_Table =>
                  Created.Append (Table.Name);   --  mark the table as created

                  Append (SQL, "CREATE TABLE """
                          & Table.Name & """ (" & ASCII.LF);
                  For_Each_Field
                    (Table, Add_Field_To_SQL'Access,
                     Include_Inherited => True);

                  SQL_PK := Null_Unbounded_String;
                  For_Each_Field (Table, Print_PK'Access, True);
                  if SQL_PK /= "" then
                     Append (SQL, ", PRIMARY KEY (" & SQL_PK & ")");
                  end if;

                  Print_FK (Table);
                  Print_Indexes (Table);
                  Append (SQL, ")");
                  Do_Statement (To_String (SQL));

               when Kind_View  =>
                  null;
            end case;
         end if;
      end For_Table;

      S  : String_Lists.Cursor;

   begin
      For_Each_Table (Schema, For_Table'Access, Alphabetical => False);

      if Self.DB.Success then
         S := First (Deferred);
         while Has_Element (S) loop
            Do_Statement (Element (S));
            Next (S);
         end loop;

         S := First (Deferred_Indexes);
         while Has_Element (S) loop
            Do_Statement (Element (S));
            Next (S);
         end loop;
      end if;

      if Self.DB /= null then
         Commit_Or_Rollback (Self.DB);

         if not Self.DB.Success then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;
      end if;

   exception
      when Invalid_Schema =>
         Rollback (Self.DB);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Write_Schema;

   ------------------
   -- Write_Schema --
   ------------------

   overriding procedure Write_Schema
     (Self : File_Schema_IO; Schema : DB_Schema)
   is
   begin
      Write_Schema (Self, Schema, Ada.Text_IO.Put'Access);
   end Write_Schema;

   ------------------
   -- Write_Schema --
   ------------------

   procedure Write_Schema
     (Self   : File_Schema_IO;
      Schema : DB_Schema;
      Puts   : access procedure (S : String);
      Align_Columns : Boolean := True;
      Show_Comments : Boolean := True)
   is
      To_File : File_Type;
      Put : access procedure (S : String) := Puts;

      Not_Null : constant String := "NOT NULL";
      Column_Widths : array (1 .. 4) of Natural;
      --  The maximum width of all columns

      function SQL_Type (Attr : Field) return String;
      --  Return the type to use for Attr. This includes foreign keys when
      --  appropriate

      procedure For_Table (Table : in out Table_Description);
      --  Process a table

      procedure For_Field (F : in out Field);
      --  Process a field

      --------------
      -- SQL_Type --
      --------------

      function SQL_Type (Attr : Field) return String is
         FK : constant Field := Attr.Is_FK;
      begin
         if FK = No_Field then
            if Attr.Get_Type.Kind = Field_Autoincrement then
               return "AUTOINCREMENT";
            else
               return To_SQL (Attr.Get_Type);
            end if;
         else
            return "FK " & FK.Get_Table.Name;
         end if;
      end SQL_Type;

      ---------------
      -- For_Field --
      ---------------

      procedure For_Field (F : in out Field) is
         Name    : constant String := F.Name;
         Default : constant String := F.Default;
      begin
         Put
           ("|" & Name & (1 .. Column_Widths (1) - Name'Length => ' ') & "|");

         declare
            Typ : constant String := SQL_Type (F);
         begin
            Put (Typ & (1 .. Column_Widths (2) - Typ'Length => ' ') & "|");
         end;

         if F.Is_PK then
            Put ("PK");
         elsif not F.Can_Be_Null then
            Put (Not_Null);
         elsif Align_Columns then
            Put ("NULL");
         end if;

         if F.Get.Props.Indexed then
            Put (",INDEX");
         elsif F.Get.Props.Noindex then
            Put (",NOINDEX");
         end if;

         if F.Get.Props.Case_Insensitive then
            Put (",NOCASE");
         end if;

         Put
           ("|" & Default & (1 .. Column_Widths (4) - Default'Length => ' ')
            & "|");

         if Show_Comments then
            Put
              (Translate (F.Description,
               Mapping => To_Mapping ("" & ASCII.LF, " ")) & "|" & ASCII.LF);
         else
            Put ("" & ASCII.LF);
         end if;
      end For_Field;

      ---------------
      -- For_Table --
      ---------------

      procedure For_Table (Table : in out Table_Description) is
         A  : Field_Lists.Cursor;
         F  : Foreign_Keys.Cursor;
         FK : Foreign_Refs.Encapsulated_Access;
         P  : Pair_Lists.Cursor;
      begin
         --  Compute widths
         --  Minimum size of column 1 is 5 (for "TABLE")
         if Align_Columns then
            Column_Widths := (1 => 5, 2 => 0, 3 => Not_Null'Length, 4 => 0);
            A := First (TDR (Table.Get).Fields);
            while Has_Element (A) loop
               Column_Widths (1) := Integer'Max
                 (Column_Widths (1), Element (A).Name'Length);
               Column_Widths (2) := Integer'Max
                 (Column_Widths (2), SQL_Type (Element (A))'Length);
               Column_Widths (4) := Integer'Max
                 (Column_Widths (4), Element (A).Default'Length);
               Next (A);
            end loop;

         else
            Column_Widths := (others => 0);
         end if;

         case Table.Get_Kind is
            when Kind_Table =>
               Put
                 ("|TABLE" & (1 .. Column_Widths (1) - 5 => ' ')
                  & "| " & Table.Name & ASCII.LF);
            when Kind_View  =>
               Put
                 ("|VIEW" & (1 .. Column_Widths (1) - 4 => ' ')
                  & "| " & Table.Name & ASCII.LF);
         end case;

         For_Each_Field (Table, For_Field'Access, True);

         F := TDR (Table.Get).FK.First;
         while Has_Element (F) loop
            FK := Element (F).Get;

            if Length (FK.Fields) > 1 then
               Put ("| FK: | " & Element (F).To_Table.Name & " | ");

               P := FK.Fields.First;
               while Has_Element (P) loop
                  Put (Element (P).From.Name & " ");
                  Next (P);
               end loop;

               Put (" | ");

               P := FK.Fields.First;
               while Has_Element (P) loop
                  Put (Element (P).To.Name & " ");
                  Next (P);
               end loop;

               Put (" |" & ASCII.LF);
            end if;

            Next (F);
         end loop;

         declare
            C : String_Lists.Cursor := TDR (Table.Get).Indexes.First;
         begin
            while Has_Element (C) loop
               declare
                  Descr : constant String := Element (C);
                  Name_Start : Integer := Descr'First + 1;
               begin
                  while Descr (Name_Start) /= '|' loop
                     Name_Start := Name_Start + 1;
                  end loop;

                  Put ("|INDEX:|"
                       & Descr (Descr'First .. Name_Start - 1)
                       & "|" & Descr (Name_Start + 1 .. Descr'Last)
                       & ASCII.LF);
               end;

               Next (C);
            end loop;
         end;

         Put ("" & ASCII.LF);
      end For_Table;

   begin
      if Self.File /= No_File then
         Create (To_File, Out_File, Self.File.Display_Full_Name);
         Set_Output (To_File);
         Put := Ada.Text_IO.Put'Access;
      end if;

      For_Each_Table (Schema, For_Table'Access, Alphabetical => False);

      if Self.File /= No_File then
         Set_Output (Standard_Output);
         Close (To_File);
      end if;
   end Write_Schema;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data
     (File   : GNATCOLL.VFS.Virtual_File;
      Puts   : access procedure (S : String))
   is
      Str          : GNAT.Strings.String_Access;
      Line_Number  : Natural := 0;
      Line         : String_List (1 .. Max_Fields_Per_Line);
      First        : Integer;
      Fields_Count : Natural;  --  Number of fields on current line

   begin
      Str := Read_Whole_File (+File.Full_Name.all);
      if Str /= null then
         First := Str'First;

         while First <= Str'Last loop
            Parse_Line
              (Line, Line_Number, Fields_Count, Str.all, First,
               Replace_Newline => False);

            if Fields_Count > 0 then
               for F in 1 .. Fields_Count loop
                  Puts ("|" & Line (F).all);
               end loop;
               Puts ("|" & ASCII.LF);
            end if;
         end loop;
      end if;

      Free (Str);
   end Load_Data;

   ----------------
   -- Parse_Line --
   ----------------

   procedure Parse_Line
     (Line         : in out String_List;
      Line_Number  : in out Natural;
      Fields_Count : out Natural;
      Data         : String;
      First        : in out Integer;
      Replace_Newline : Boolean := True)
   is
      Line_End : Natural := EOL (Data (First .. Data'Last));
      Last, Tmp : Natural;
   begin
      Free (String_List (Line));
      Fields_Count := Line'First - 1;

      Line_Number := Line_Number + 1;

      while Data (First) = '|'
        and then Data (First + 1) = '-'  --  Skip line like  |---|----|

        --  But we want to parse  |-1|...
        and then (Data'Length < 3 or else Data (First + 2) = '-')
      loop
         First := Line_End + 1;
         Line_End := EOL (Data (First .. Data'Last));
         Line_Number := Line_Number + 1;
      end loop;

      if Data (First) = '|' then
         First := First + 1;

         while First <= Line_End loop
            Skip_Blanks (Data, First);
            exit when First >= Line_End;
            exit when Data (First) = '#';  --  A comment

            --  First now points to first non-blank char

            Last := EOW (Data, First);
            exit when Last > Line_End;

            Tmp := Last - 1;
            Skip_Blanks_Backward (Data (First .. Tmp), Tmp);

            if Replace_Newline then
               declare
                  S : Unbounded_String :=
                    To_Unbounded_String (Data (First .. Tmp));
               begin
                  Replace
                    (S, Pattern => "\n", Replacement => "" & ASCII.LF);
                  Append (Line, Fields_Count, To_String (S));
               end;

            else
               Append (Line, Fields_Count, Data (First .. Tmp));
            end if;

            First := Last + 1;
         end loop;
      end if;

      First := Line_End + 1;
   end Parse_Line;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data
     (DB     : access Database_Connection_Record'Class;
      Data   : String;
      Schema : DB_Schema := No_Schema;
      Location : String := "data";
      Replace_Newline : Boolean := True)
   is
      Line_Number : Natural := 0;

      Line         : String_List (1 .. Max_Fields_Per_Line);
      First        : Integer;
      Fields_Count : Natural;  --  Number of fields on current line

      Table     : Table_Description;
      Is_Xref : array (1 .. Max_Fields_Per_Line) of Boolean;
      --  Whether a given column must be an xref

      DB_Fields       : String_List (1 .. Max_Fields_Per_Line);
      DB_Fields_Count : Natural := DB_Fields'First - 1;
      Xref       : String_List (1 .. Max_Fields_Per_Line);
      Xref_Count : Natural := Xref'First - 1;
      Paren      : Natural;
      DB_Field_Types : array (Line'First .. Max_Fields_Per_Line) of Field_Type;

      --  TODO : convert for parameter_decimal
      Tmp_DB_Fields_Count : Natural := DB_Fields'First - 1;
      Convert_To_Parameter_Type : constant array (Field_Type_Kind)
        of Parameter_Type :=
        (Field_Text          => Parameter_Text,
         Field_Integer       => Parameter_Integer,
         Field_Date          => Parameter_Date,
         Field_Json          => Parameter_Json,
         Field_Time          => Parameter_Time,
         Field_Timestamp     => Parameter_Time,
         Field_Float         => Parameter_Float,
         Field_Boolean       => Parameter_Boolean,
         Field_Autoincrement => Parameter_Integer,
         Field_Money         => Parameter_Money);

      FK : Field;
      Tables : String_List (1 .. Max_Fields_Per_Line);
      Where  : String_List (1 .. Max_Fields_Per_Line);

      Select_Values : String_List (1 .. Max_Fields_Per_Line);
      --  Parameters when values are queries through a SELECT

      DB_Values : String_List (1 .. Max_Fields_Per_Line);
      --  Parameters when all values are provided in the file

      Has_Xref_Column : Boolean;
      --  Whether at least one column can handle xref (values starting with &)

      Q_Values : Prepared_Statement;
      Q_Values_With_Select : Prepared_Statement;

      procedure Parse_Line;

      procedure Free_Vars;
      --  Free all local variables

      procedure Parse_Line is
      begin
         Parse_Line
           (Line, Line_Number, Fields_Count, Data, First, Replace_Newline);
      end Parse_Line;

      ---------------
      -- Free_Vars --
      ---------------

      procedure Free_Vars is
      begin
         Has_Xref_Column := False;

         Free (DB_Fields);
         DB_Fields_Count := DB_Fields'First - 1;
         Tmp_DB_Fields_Count := DB_Fields'First - 1;

         Free (Xref);
         Xref_Count := Xref'First - 1;

         Free (Tables);
         Free (Where);
         Free (Select_Values);
         Free (DB_Values);
      end Free_Vars;

   begin
      Trace (Me, "Loading data from " & Location & " into database");

      First := Data'First;

      if DB.Has_Pragmas then
         Execute (DB, "PRAGMA foreign_keys=OFF");
      end if;

      while First <= Data'Last loop
         Parse_Line;

         if Fields_Count /= 0
           and then (Line (1).all = "TABLE"
                     or else Line (1).all = "VIEW")
         then
            Free_Vars;

            Table := Get_Table (Schema, Line (2).all);

            Parse_Line;  --  Parse fields
            for L in Line'First .. Fields_Count loop
               exit when Line (L).all = "";

               Paren := Ada.Strings.Fixed.Index (Line (L).all, "(&");
               Is_Xref (DB_Fields_Count + 1) := Paren >= Line (L)'First;

               if Is_Xref (DB_Fields_Count + 1) then
                  declare
                     Name : constant String :=
                       Line (L) (Line (L)'First .. Paren - 1);
                  begin
                     Append (DB_Fields, DB_Fields_Count, Quote_Keyword (Name));
                     Append (Xref, Xref_Count,
                             Line (L) (Paren + 2 .. Line (L)'Last - 1));
                     FK := Table.Field_From_Name (Name).Is_FK;
                     Has_Xref_Column := True;
                     Tables (L) := new String'
                       (FK.Get_Table.Name & " t" & Image (L, 0));
                  end;
               else
                  Append (DB_Fields, DB_Fields_Count,
                          Quote_Keyword (Line (L).all));
                  Append (Xref, Xref_Count, "");
               end if;
            end loop;

            declare
               procedure On_Field (F : in out Field);
               procedure On_Field (F : in out Field) is
                  N : constant String := Quote_Keyword (F.Name);
               begin
                  for L in DB_Fields'First .. DB_Fields_Count loop
                     if DB_Fields (L).all = N then
                        DB_Field_Types (L) := F.Get_Type;
                        return;
                     end if;
                  end loop;

                  --  We might not find the field, in case the data only sets
                  --  a subset of the fields. That doesn't matter.
               end On_Field;
            begin
               Table.For_Each_Field
                 (On_Field'Access, Include_Inherited => True);
            end;

            --  Set Select_Values and DB_Values according to DB_Field_Types
            for L in Line'First .. Fields_Count loop
               exit when Line (L).all = "";

               if Is_Xref (Tmp_DB_Fields_Count + 1) then
                  Select_Values (L) :=
                    new String'("t" & Image (L, 0) & "." & FK.Name);
                  Where (L) := new String'
                    ("t" & Image (L, 0) & "." & Xref (L).all
                     & "=" & DB.Parameter_String
                       (L,
                        Convert_To_Parameter_Type (DB_Field_Types (L).Kind)));
               else
                  Select_Values (L) := new String'
                       (DB.Parameter_String (L,
                        Convert_To_Parameter_Type (DB_Field_Types (L).Kind)));
               end if;

               DB_Values (L) := new String'
                       (DB.Parameter_String (L,
                        Convert_To_Parameter_Type (DB_Field_Types (L).Kind)));

               Tmp_DB_Fields_Count := Tmp_DB_Fields_Count + 1;
            end loop;

            Q_Values := Prepare
              ("INSERT INTO """ & Table.Name & """("
               & Join (",", DB_Fields (DB_Fields'First .. DB_Fields_Count))
               & ") VALUES ("
               & Join (",", DB_Values (1 .. DB_Fields_Count)) & ")",
               On_Server => True,
               Name => "insertval");

            if Has_Xref_Column then
               Q_Values_With_Select := Prepare
                 ("INSERT INTO """ & Table.Name & """("
                  & Join (",", DB_Fields (DB_Fields'First .. DB_Fields_Count))
                  & ") SELECT "
                  & Join (",", Select_Values (1 .. DB_Fields_Count))
                  & " FROM " & Join (",", Tables (1 .. DB_Fields_Count))
                  & " WHERE " & Join (" and ", Where (1 .. DB_Fields_Count)),
                  On_Server => True,
                  Name => "insertv");
            end if;

         elsif Fields_Count /= 0
           and then Line (1).all = "QUERIES"
         then
            while First <= Data'Last and then Data (First) = '|' loop
               Parse_Line;
               Execute (DB, Line (1).all);
            end loop;

         elsif Fields_Count /= 0 then
            declare
               Values : SQL_Parameters (1 .. DB_Fields_Count);
               Vals   : String_List (1 .. DB_Fields_Count);
               Has_Xref : Boolean := False;
               Use_Custom : Boolean := False;
               Custom_Tables : String_List (1 .. DB_Fields_Count);
               Custom_Where : String_List (1 .. DB_Fields_Count);
            begin
               --  Check the xref in the columns

               for L in Line'First
                 .. Integer'Min (Fields_Count, DB_Fields_Count)
               loop
                  if Starts_With (Line (L).all, "&") then
                     Has_Xref := True;

                     if not Is_Xref (L) then
                        --  The column was not prepared as an xref
                        raise Invalid_File
                           with Location & ":" & Image (Line_Number, 0)
                          & ": column title must indicate referenced field";
                     end if;

                     Vals (L) := new String'
                       (Line (L) (Line (L)'First + 1 .. Line (L)'Last));
                     Values (L) := +Vals (L);

                  else
                     if Is_Xref (L) then
                        --  The prepared query expects an xref, but we do not
                        --  have. So we'll use a custom query
                        Use_Custom := True;
                     end if;
                  end if;
               end loop;

               if Use_Custom then
                  for L in Line'First
                    .. Integer'Min (Fields_Count, DB_Fields_Count)
                  loop
                     if Starts_With (Line (L).all, "&") then
                        Custom_Tables (L) := Tables (L);
                        Custom_Where (L) := new String'
                          ("t" & Image (L, 0) & "." & Xref (L).all
                           & "='"   --  ??? Beware of SQL injection here
                           & Line (L) (Line (L)'First + 1 .. Line (L)'Last)
                           & "'");
                        Free (Vals (L));
                        Vals (L) := new String'(Select_Values (L).all);
                     end if;
                  end loop;
               end if;

               for L in Line'First
                 .. Integer'Min (Fields_Count, DB_Fields_Count)
               loop
                  if Vals (L) = null then
                     Format_Field
                       (DB,
                        Line (L).all,
                        DB_Field_Types (L),
                        Vals (L),
                        Values (L),
                        Has_Xref => Use_Custom);
                  end if;
               end loop;

               if not Has_Xref then
                  Execute (DB, Q_Values, Params => Values);
               elsif Use_Custom then
                  Execute
                    (DB, "INSERT INTO """ & Table.Name & """("
                     & Join
                       (",", DB_Fields (DB_Fields'First .. DB_Fields_Count))
                     & ") SELECT " & Join (",", Vals)
                     & " FROM " & Join (",", Custom_Tables)
                     & " WHERE " & Join (" and ", Custom_Where));
               else
                  Execute (DB, Q_Values_With_Select, Params => Values);
               end if;

               Free (Custom_Where);
               Free (Vals);
            end;
         end if;

         exit when not Success (DB);
      end loop;

      Free (String_List (Line));
      Free_Vars;
   end Load_Data;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data
     (DB     : access Database_Connection_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Schema : DB_Schema := No_Schema;
      Replace_Newline : Boolean := True)
   is
      Str         : GNAT.Strings.String_Access;
   begin
      Str := Read_Whole_File (+File.Full_Name.all);
      if Str /= null then
         Load_Data (DB, Str.all, Schema, File.Display_Full_Name,
                    Replace_Newline => Replace_Newline);
         Free (Str);
      else
         raise Invalid_File with "File not found: " & File.Display_Full_Name;
      end if;
   end Load_Data;

   -------------------
   -- New_Schema_IO --
   -------------------

   function New_Schema_IO
     (File : GNATCOLL.VFS.Virtual_File) return File_Schema_IO'Class is
   begin
      return Result : File_Schema_IO do
         Result.File := File;
      end return;
   end New_Schema_IO;

   -------------------
   -- New_Schema_IO --
   -------------------

   function New_Schema_IO
     (DB : Database_Connection) return DB_Schema_IO'Class is
   begin
      return Result : DB_Schema_IO do
         Result.DB := DB;
      end return;
   end New_Schema_IO;

   -------------------
   -- Quote_Keyword --
   -------------------

   function Quote_Keyword (Str : String) return String is
   begin
      if Keywords.Is_Empty then
         --  For each keyword (from the postgreSQL documentation):
         --     * reserved (in sql99 or postgreSQL) means the word cannot be
         --       used for identifiers
         --     * non means the word can only be used in some cases.

         --  Keywords.Include ("ABORT");    --  psql:non
         Keywords.Include ("ABS");          --  sql99:non
         Keywords.Include ("ABSOLUTE");     --  sql99:reserved, psql:non
         Keywords.Include ("ACCESS");       --  non postgres
         Keywords.Include ("ACTION");       --  reserved sql99, not postgres
         Keywords.Include ("ADA");          --  non

         Keywords.Include ("ADD");   --  psql:non, sql99:reserved
         Keywords.Include ("ADMIN");   --  psql:reserved, sql99:
         Keywords.Include ("AFTER");   --  psql:non, sql99:reserved
         Keywords.Include ("AGGREGATE");   --  psql:non, sql99:reserved
         Keywords.Include ("ALIAS");   --  psql:reserved, sql99:
         Keywords.Include ("ALL");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ALLOCATE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ALTER");   --  psql:non, sql99:reserved
         Keywords.Include ("ANALYSE");   --  psql:reserved, sql99:
         Keywords.Include ("ANALYZE");   --  psql:reserved, sql99:
         Keywords.Include ("AND");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ANY");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ARE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ARRAY");   --  psql:reserved, sql99:
         Keywords.Include ("AS");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ASC");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ASENSITIVE");   --  psql:non, sql99:
         Keywords.Include ("ASSERTION");   --  psql:non, sql99:reserved
         Keywords.Include ("ASSIGNMENT");   --  psql:non, sql99:non
         Keywords.Include ("ASYMMETRIC");   --  psql:non, sql99:
         Keywords.Include ("AT");   --  psql:non, sql99:reserved
         Keywords.Include ("ATOMIC");   --  psql:non, sql99:
         Keywords.Include ("AUTHORIZATION");  --  psql:reserved, sql99:reserved
         Keywords.Include ("AVG");   --  psql:non, sql99:reserved
         Keywords.Include ("BACKWARD");   --  psql:non, sql99:
         Keywords.Include ("BEFORE");   --  psql:non, sql99:reserved
         Keywords.Include ("BEGIN");   --  psql:non, sql99:reserved
         Keywords.Include ("BETWEEN");   --  psql:reserved, sql99:(can
         Keywords.Include ("BIGINT");   --  psql:non, sql99:(cannot
         Keywords.Include ("BINARY");   --  psql:reserved, sql99:(can
         Keywords.Include ("BIT");   --  psql:non, sql99:(cannot
         Keywords.Include ("BITVAR");   --  psql:non, sql99:
         Keywords.Include ("BIT_LENGTH");   --  psql:non, sql99:reserved
         Keywords.Include ("BLOB");   --  psql:reserved, sql99:
         Keywords.Include ("BOOLEAN");   --  psql:non, sql99:(cannot
         Keywords.Include ("BOTH");   --  psql:reserved, sql99:reserved
         Keywords.Include ("BREADTH");   --  psql:reserved, sql99:
         Keywords.Include ("BY");   --  psql:non, sql99:reserved
         Keywords.Include ("C");   --  psql:non, sql99:non
         Keywords.Include ("CACHE");   --  psql:non, sql99:
         Keywords.Include ("CALL");   --  psql:reserved, sql99:
         Keywords.Include ("CALLED");   --  psql:non, sql99:non
         Keywords.Include ("CARDINALITY");   --  psql:non, sql99:
         Keywords.Include ("CASCADE");   --  psql:non, sql99:reserved
         Keywords.Include ("CASCADED");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CASE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CAST");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CATALOG");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CATALOG_NAME");   --  psql:non, sql99:non
         Keywords.Include ("CHAIN");   --  psql:non, sql99:non
         Keywords.Include ("CHAR");   --  psql:non, sql99:(cannot
         Keywords.Include ("CHARACTER");   --  psql:non, sql99:(cannot
         Keywords.Include ("CHARACTERISTICS");   --  psql:non, sql99:
         Keywords.Include ("CHARACTER_LENGTH");   --  psql:non, sql99:reserved
         Keywords.Include ("CHARACTER_SET_CATALOG");   --  psql:non, sql99:non
         Keywords.Include ("CHARACTER_SET_NAME");   --  psql:non, sql99:non
         Keywords.Include ("CHARACTER_SET_SCHEMA");   --  psql:non, sql99:non
         Keywords.Include ("CHAR_LENGTH");   --  psql:non, sql99:reserved
         Keywords.Include ("CHECK");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CHECKED");   --  psql:non, sql99:
         Keywords.Include ("CHECKPOINT");   --  psql:non, sql99:
         Keywords.Include ("CLASS");   --  psql:non, sql99:reserved
         Keywords.Include ("CLASS_ORIGIN");   --  psql:non, sql99:non
         Keywords.Include ("CLOB");   --  psql:reserved, sql99:
         Keywords.Include ("CLOSE");   --  psql:non, sql99:reserved
         Keywords.Include ("CLUSTER");   --  psql:non, sql99:
         Keywords.Include ("COALESCE");   --  psql:non, sql99:non
         Keywords.Include ("COBOL");   --  psql:non, sql99:non
         Keywords.Include ("COLLATE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("COLLATION");   --  psql:reserved, sql99:reserved
         Keywords.Include ("COLLATION_CATALOG");   --  psql:non, sql99:non
         Keywords.Include ("COLLATION_NAME");   --  psql:non, sql99:non
         Keywords.Include ("COLLATION_SCHEMA");   --  psql:non, sql99:non
         Keywords.Include ("COLUMN");   --  psql:reserved, sql99:reserved
         Keywords.Include ("COLUMN_NAME");   --  psql:non, sql99:non
         Keywords.Include ("COMMAND_FUNCTION");   --  psql:non, sql99:non
         Keywords.Include ("COMMAND_FUNCTION_CODE");   --  psql:non, sql99:
         Keywords.Include ("COMMENT");   --  psql:non, sql99:
         Keywords.Include ("COMMIT");   --  psql:non, sql99:reserved
         Keywords.Include ("COMMITTED");   --  psql:non, sql99:non
         Keywords.Include ("COMPLETION");   --  psql:reserved, sql99:
         Keywords.Include ("CONDITION_NUMBER");   --  psql:non, sql99:non
         Keywords.Include ("CONNECT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CONNECTION");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CONNECTION_NAME");   --  psql:non, sql99:non
         Keywords.Include ("CONSTRAINT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CONSTRAINTS");   --  psql:non, sql99:reserved
         Keywords.Include ("CONSTRAINT_CATALOG");   --  psql:non, sql99:non
         Keywords.Include ("CONSTRAINT_NAME");   --  psql:non, sql99:non
         Keywords.Include ("CONSTRAINT_SCHEMA");   --  psql:non, sql99:non
         Keywords.Include ("CONSTRUCTOR");   --  psql:reserved, sql99:
         Keywords.Include ("CONTAINS");   --  psql:non, sql99:
         Keywords.Include ("CONTINUE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CONVERSION");   --  psql:non, sql99:
         Keywords.Include ("CONVERT");   --  psql:non, sql99:(cannot
         Keywords.Include ("COPY");   --  psql:non, sql99:
         Keywords.Include ("CORRESPONDING");   --  psql:res., sql99:res.
         Keywords.Include ("COUNT");   --  psql:non, sql99:reserved
         Keywords.Include ("CREATE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CREATEDB");   --  psql:non, sql99:
         Keywords.Include ("CREATEUSER");   --  psql:non, sql99:
         Keywords.Include ("CROSS");   --  psql:reserved, sql99:(can
         Keywords.Include ("CUBE");   --  psql:reserved, sql99:
         Keywords.Include ("CURRENT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CURRENT_DATE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CURRENT_PATH");   --  psql:reserved, sql99:
         Keywords.Include ("CURRENT_ROLE");   --  psql:reserved, sql99:
         Keywords.Include ("CURRENT_TIME");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CURRENT_TIMESTAMP");   --  psql:res., sql99:res.
         Keywords.Include ("CURRENT_USER");   --  psql:reserved, sql99:reserved
         Keywords.Include ("CURSOR");   --  psql:non, sql99:reserved
         Keywords.Include ("CURSOR_NAME");   --  psql:non, sql99:non
         Keywords.Include ("CYCLE");   --  psql:non, sql99:reserved
         Keywords.Include ("DATA");   --  psql:reserved, sql99:non
         Keywords.Include ("DATABASE");   --  psql:non, sql99:
         Keywords.Include ("DATE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DATETIME_INTERVAL_CODE");   --  psql:non, sql99:non
         Keywords.Include ("DATETIME_INTERVAL_PRECISION");  --  psql:n, sql99:n
         Keywords.Include ("DAY");   --  psql:non, sql99:reserved
         Keywords.Include ("DEALLOCATE");   --  psql:non, sql99:reserved
         Keywords.Include ("DEC");   --  psql:non, sql99:(cannot
         Keywords.Include ("DECIMAL");   --  psql:non, sql99:(cannot
         Keywords.Include ("DECLARE");   --  psql:non, sql99:reserved
         Keywords.Include ("DEFAULT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DEFERRABLE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DEFERRED");   --  psql:non, sql99:reserved
         Keywords.Include ("DEFINED");   --  psql:non, sql99:
         Keywords.Include ("DEFINER");   --  psql:non, sql99:non
         Keywords.Include ("DELETE");   --  psql:non, sql99:reserved
         Keywords.Include ("DELIMITER");   --  psql:non, sql99:
         Keywords.Include ("DELIMITERS");   --  psql:non, sql99:
         Keywords.Include ("DEPTH");   --  psql:reserved, sql99:
         Keywords.Include ("DEREF");   --  psql:reserved, sql99:
         Keywords.Include ("DESC");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DESCRIBE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DESCRIPTOR");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DESTROY");   --  psql:reserved, sql99:
         Keywords.Include ("DESTRUCTOR");   --  psql:reserved, sql99:
         Keywords.Include ("DETERMINISTIC");   --  psql:reserved, sql99:
         Keywords.Include ("DIAGNOSTICS");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DICTIONARY");   --  psql:reserved, sql99:
         Keywords.Include ("DISCONNECT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DISPATCH");   --  psql:non, sql99:
         Keywords.Include ("DISTINCT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("DO");   --  psql:reserved, sql99:
         Keywords.Include ("DOMAIN");   --  psql:non, sql99:reserved
         Keywords.Include ("DOUBLE");   --  psql:non, sql99:reserved
         Keywords.Include ("DROP");   --  psql:non, sql99:reserved
         Keywords.Include ("DYNAMIC");   --  psql:reserved, sql99:
         Keywords.Include ("DYNAMIC_FUNCTION");   --  psql:non, sql99:non
         Keywords.Include ("DYNAMIC_FUNCTION_CODE");   --  psql:non, sql99:
         Keywords.Include ("EACH");   --  psql:non, sql99:reserved
         Keywords.Include ("ELSE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ENCODING");   --  psql:non, sql99:
         Keywords.Include ("ENCRYPTED");   --  psql:non, sql99:
         Keywords.Include ("END");   --  psql:reserved, sql99:reserved
         Keywords.Include ("END-EXEC");   --  psql:reserved, sql99:reserved
         Keywords.Include ("EQUALS");   --  psql:reserved, sql99:
         Keywords.Include ("ESCAPE");   --  psql:non, sql99:reserved
         Keywords.Include ("EVERY");   --  psql:reserved, sql99:
         Keywords.Include ("EXCEPT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("EXCEPTION");   --  psql:reserved, sql99:reserved
         Keywords.Include ("EXCLUSIVE");   --  psql:non, sql99:
         Keywords.Include ("EXEC");   --  psql:reserved, sql99:reserved
         Keywords.Include ("EXECUTE");   --  psql:non, sql99:reserved
         Keywords.Include ("EXISTING");   --  psql:non, sql99:
         Keywords.Include ("EXISTS");   --  psql:non, sql99:(cannot
         Keywords.Include ("EXPLAIN");   --  psql:non, sql99:
         Keywords.Include ("EXTERNAL");   --  psql:non, sql99:reserved
         Keywords.Include ("EXTRACT");   --  psql:non, sql99:(cannot
         Keywords.Include ("FALSE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("FETCH");   --  psql:non, sql99:reserved
         Keywords.Include ("FINAL");   --  psql:non, sql99:
         Keywords.Include ("FIRST");   --  psql:reserved, sql99:reserved
         Keywords.Include ("FLOAT");   --  psql:non, sql99:(cannot
         Keywords.Include ("FOR");   --  psql:reserved, sql99:reserved
         Keywords.Include ("FORCE");   --  psql:non, sql99:
         Keywords.Include ("FOREIGN");   --  psql:reserved, sql99:reserved
         Keywords.Include ("FORTRAN");   --  psql:non, sql99:non
         Keywords.Include ("FORWARD");   --  psql:non, sql99:
         Keywords.Include ("FOUND");   --  psql:reserved, sql99:reserved
         Keywords.Include ("FREE");   --  psql:reserved, sql99:
         Keywords.Include ("FREEZE");   --  psql:reserved, sql99:(can
         Keywords.Include ("FROM");   --  psql:reserved, sql99:reserved
         Keywords.Include ("FULL");   --  psql:reserved, sql99:(can
         Keywords.Include ("FUNCTION");   --  psql:non, sql99:reserved
         Keywords.Include ("G");   --  psql:non, sql99:
         Keywords.Include ("GENERAL");   --  psql:reserved, sql99:
         Keywords.Include ("GENERATED");   --  psql:non, sql99:
         Keywords.Include ("GET");   --  psql:non, sql99:reserved
         Keywords.Include ("GLOBAL");   --  psql:non, sql99:reserved
         Keywords.Include ("GO");   --  psql:reserved, sql99:reserved
         Keywords.Include ("GOTO");   --  psql:reserved, sql99:reserved
         Keywords.Include ("GRANT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("GRANTED");   --  psql:non, sql99:
         Keywords.Include ("GROUP");   --  psql:reserved, sql99:reserved
         Keywords.Include ("GROUPING");   --  psql:reserved, sql99:
         Keywords.Include ("HANDLER");   --  psql:non, sql99:
         Keywords.Include ("HAVING");   --  psql:reserved, sql99:reserved
         Keywords.Include ("HIERARCHY");   --  psql:non, sql99:
         Keywords.Include ("HOLD");   --  psql:non, sql99:
         Keywords.Include ("HOST");   --  psql:reserved, sql99:
         Keywords.Include ("HOUR");   --  psql:non, sql99:reserved
         Keywords.Include ("IDENTITY");   --  psql:reserved, sql99:reserved
         Keywords.Include ("IGNORE");   --  psql:reserved, sql99:
         Keywords.Include ("ILIKE");   --  psql:reserved, sql99:(can
         Keywords.Include ("IMMEDIATE");   --  psql:non, sql99:reserved
         Keywords.Include ("IMMUTABLE");   --  psql:non, sql99:
         Keywords.Include ("IMPLEMENTATION");   --  psql:non, sql99:
         Keywords.Include ("IMPLICIT");   --  psql:non, sql99:
         Keywords.Include ("IN");   --  psql:reserved, sql99:(can
         Keywords.Include ("INCREMENT");   --  psql:non, sql99:
         Keywords.Include ("INDEX");   --  psql:non, sql99:
         Keywords.Include ("INDICATOR");   --  psql:reserved, sql99:reserved
         Keywords.Include ("INFIX");   --  psql:non, sql99:
         Keywords.Include ("INHERITS");   --  psql:non, sql99:
         Keywords.Include ("INITIALIZE");   --  psql:reserved, sql99:
         Keywords.Include ("INITIALLY");   --  psql:reserved, sql99:reserved
         Keywords.Include ("INNER");   --  psql:reserved, sql99:(can
         Keywords.Include ("INOUT");   --  psql:non, sql99:reserved
         Keywords.Include ("INPUT");   --  psql:non, sql99:reserved
         Keywords.Include ("INSENSITIVE");   --  psql:non, sql99:non
         Keywords.Include ("INSERT");   --  psql:non, sql99:reserved
         Keywords.Include ("INSTANCE");   --  psql:non, sql99:
         Keywords.Include ("INSTANTIABLE");   --  psql:non, sql99:
         Keywords.Include ("INSTEAD");   --  psql:non, sql99:
         Keywords.Include ("INT");   --  psql:non, sql99:(cannot
         Keywords.Include ("INTEGER");   --  psql:non, sql99:(cannot
         Keywords.Include ("INTERSECT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("INTERVAL");   --  psql:non, sql99:(cannot
         Keywords.Include ("INTO");   --  psql:reserved, sql99:reserved
         Keywords.Include ("INVOKER");   --  psql:non, sql99:non
         Keywords.Include ("IS");   --  psql:reserved, sql99:(can
         Keywords.Include ("ISNULL");   --  psql:reserved, sql99:(can
         Keywords.Include ("ISOLATION");   --  psql:non, sql99:reserved
         Keywords.Include ("ITERATE");   --  psql:reserved, sql99:
         Keywords.Include ("JOIN");   --  psql:reserved, sql99:(can
         Keywords.Include ("K");   --  psql:non, sql99:
         Keywords.Include ("KEY");   --  psql:non, sql99:reserved
         Keywords.Include ("KEY_MEMBER");   --  psql:non, sql99:
         Keywords.Include ("KEY_TYPE");   --  psql:non, sql99:
         Keywords.Include ("LANCOMPILER");   --  psql:non, sql99:
         Keywords.Include ("LANGUAGE");   --  psql:non, sql99:reserved
         Keywords.Include ("LARGE");   --  psql:reserved, sql99:
         Keywords.Include ("LAST");   --  psql:reserved, sql99:reserved
         Keywords.Include ("LATERAL");   --  psql:reserved, sql99:
         Keywords.Include ("LEADING");   --  psql:reserved, sql99:reserved
         Keywords.Include ("LEFT");   --  psql:reserved, sql99:(can
         Keywords.Include ("LENGTH");   --  psql:non, sql99:non
         Keywords.Include ("LESS");   --  psql:reserved, sql99:
         Keywords.Include ("LEVEL");   --  psql:non, sql99:reserved
         Keywords.Include ("LIKE");   --  psql:reserved, sql99:(can
         Keywords.Include ("LIMIT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("LISTEN");   --  psql:non, sql99:
         Keywords.Include ("LOAD");   --  psql:non, sql99:
         Keywords.Include ("LOCAL");   --  psql:non, sql99:reserved
         Keywords.Include ("LOCALTIME");   --  psql:reserved, sql99:reserved
         Keywords.Include ("LOCALTIMESTAMP");   --  psql:reserved, sql99:res.
         Keywords.Include ("LOCATION");   --  psql:non, sql99:
         Keywords.Include ("LOCATOR");   --  psql:reserved, sql99:
         Keywords.Include ("LOCK");   --  psql:non, sql99:
         Keywords.Include ("LOWER");   --  psql:non, sql99:reserved
         Keywords.Include ("M");   --  psql:non, sql99:
         Keywords.Include ("MAP");   --  psql:reserved, sql99:
         Keywords.Include ("MATCH");   --  psql:non, sql99:reserved
         Keywords.Include ("MAX");   --  psql:non, sql99:reserved
         Keywords.Include ("MAXVALUE");   --  psql:non, sql99:
         Keywords.Include ("MESSAGE_LENGTH");   --  psql:non, sql99:non
         Keywords.Include ("MESSAGE_OCTET_LENGTH");   --  psql:non, sql99:non
         Keywords.Include ("MESSAGE_TEXT");   --  psql:non, sql99:non
         Keywords.Include ("METHOD");   --  psql:non, sql99:
         Keywords.Include ("MIN");   --  psql:non, sql99:reserved
         Keywords.Include ("MINUTE");   --  psql:non, sql99:reserved
         Keywords.Include ("MINVALUE");   --  psql:non, sql99:
         Keywords.Include ("MOD");   --  psql:non, sql99:
         Keywords.Include ("MODE");   --  psql:non, sql99:
         Keywords.Include ("MODIFIES");   --  psql:reserved, sql99:
         Keywords.Include ("MODIFY");   --  psql:reserved, sql99:
         Keywords.Include ("MODULE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("MONTH");   --  psql:non, sql99:reserved
         Keywords.Include ("MORE");   --  psql:non, sql99:non
         Keywords.Include ("MOVE");   --  psql:non, sql99:
         Keywords.Include ("MUMPS");   --  psql:non, sql99:non
         Keywords.Include ("NAME");   --  psql:non, sql99:non
         Keywords.Include ("NAMES");   --  psql:non, sql99:reserved
         Keywords.Include ("NATIONAL");   --  psql:non, sql99:reserved
         Keywords.Include ("NATURAL");   --  psql:reserved, sql99:(can
         Keywords.Include ("NCHAR");   --  psql:non, sql99:(cannot
         Keywords.Include ("NCLOB");   --  psql:reserved, sql99:
         Keywords.Include ("NEW");   --  psql:reserved, sql99:reserved
         Keywords.Include ("NEXT");   --  psql:non, sql99:reserved
         Keywords.Include ("NO");   --  psql:non, sql99:reserved
         Keywords.Include ("NOCREATEDB");   --  psql:non, sql99:
         Keywords.Include ("NOCREATEUSER");   --  psql:non, sql99:
         Keywords.Include ("NONE");   --  psql:non, sql99:(cannot
         Keywords.Include ("NOT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("NOTHING");   --  psql:non, sql99:
         Keywords.Include ("NOTIFY");   --  psql:non, sql99:
         Keywords.Include ("NOTNULL");   --  psql:reserved, sql99:(can
         Keywords.Include ("NULL");   --  psql:reserved, sql99:reserved
         Keywords.Include ("NULLABLE");   --  psql:non, sql99:non
         Keywords.Include ("NULLIF");   --  psql:non, sql99:(cannot
         Keywords.Include ("NUMBER");   --  psql:non, sql99:non
         Keywords.Include ("NUMERIC");   --  psql:non, sql99:(cannot
         Keywords.Include ("OBJECT");   --  psql:reserved, sql99:
         Keywords.Include ("OCTET_LENGTH");   --  psql:non, sql99:reserved
         Keywords.Include ("OF");   --  psql:non, sql99:reserved
         Keywords.Include ("OFF");   --  psql:reserved, sql99:reserved
         Keywords.Include ("OFFSET");   --  psql:reserved, sql99:
         Keywords.Include ("OIDS");   --  psql:non, sql99:
         Keywords.Include ("OLD");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ON");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ONLY");   --  psql:reserved, sql99:reserved
         Keywords.Include ("OPEN");   --  psql:reserved, sql99:reserved
         Keywords.Include ("OPERATION");   --  psql:reserved, sql99:
         Keywords.Include ("OPERATOR");   --  psql:non, sql99:
         Keywords.Include ("OPTION");   --  psql:non, sql99:reserved
         Keywords.Include ("OPTIONS");   --  psql:non, sql99:
         Keywords.Include ("OR");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ORDER");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ORDINALITY");   --  psql:reserved, sql99:
         Keywords.Include ("OUT");   --  psql:non, sql99:reserved
         Keywords.Include ("OUTER");   --  psql:reserved, sql99:(can
         Keywords.Include ("OUTPUT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("OVERLAPS");   --  psql:reserved, sql99:(can
         Keywords.Include ("OVERLAY");   --  psql:non, sql99:(cannot
         Keywords.Include ("OVERRIDING");   --  psql:non, sql99:
         Keywords.Include ("OWNER");   --  psql:non, sql99:
         Keywords.Include ("PAD");   --  psql:reserved, sql99:reserved
         Keywords.Include ("PARAMETER");   --  psql:reserved, sql99:
         Keywords.Include ("PARAMETERS");   --  psql:reserved, sql99:
         Keywords.Include ("PARAMETER_MODE");   --  psql:non, sql99:
         Keywords.Include ("PARAMETER_NAME");   --  psql:non, sql99:
         Keywords.Include ("PARAMETER_ORDINAL_POSITION");   --  sql99:non
         Keywords.Include ("PARAMETER_SPECIFIC_CATALOG");   --  sql99:non
         Keywords.Include ("PARAMETER_SPECIFIC_NAME");   --  psql:non, sql99:
         Keywords.Include ("PARAMETER_SPECIFIC_SCHEMA");   --  psql:non, sql99:
         Keywords.Include ("PARTIAL");   --  psql:non, sql99:reserved
         Keywords.Include ("PASCAL");   --  psql:non, sql99:non
         Keywords.Include ("PASSWORD");   --  psql:non, sql99:
         Keywords.Include ("PATH");   --  psql:non, sql99:reserved
         Keywords.Include ("PENDANT");   --  psql:non, sql99:
         Keywords.Include ("PLACING");   --  psql:reserved, sql99:
         Keywords.Include ("PLI");   --  psql:non, sql99:non
         Keywords.Include ("POSITION");   --  psql:non, sql99:(cannot
         Keywords.Include ("POSTFIX");   --  psql:reserved, sql99:
         Keywords.Include ("PRECISION");   --  psql:non, sql99:reserved
         Keywords.Include ("PREFIX");   --  psql:reserved, sql99:
         Keywords.Include ("PREORDER");   --  psql:reserved, sql99:
         Keywords.Include ("PREPARE");   --  psql:non, sql99:reserved
         Keywords.Include ("PRESERVE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("PRIMARY");   --  psql:reserved, sql99:reserved
         Keywords.Include ("PRIOR");   --  psql:non, sql99:reserved
         Keywords.Include ("PRIVILEGES");   --  psql:non, sql99:reserved
         Keywords.Include ("PROCEDURAL");   --  psql:non, sql99:
         Keywords.Include ("PROCEDURE");   --  psql:non, sql99:reserved
         Keywords.Include ("PUBLIC");   --  psql:reserved, sql99:reserved
         Keywords.Include ("READ");   --  psql:non, sql99:reserved
         Keywords.Include ("READS");   --  psql:reserved, sql99:
         Keywords.Include ("REAL");   --  psql:non, sql99:(cannot
         Keywords.Include ("RECHECK");   --  psql:non, sql99:
         Keywords.Include ("RECURSIVE");   --  psql:reserved, sql99:
         Keywords.Include ("REF");   --  psql:reserved, sql99:
         Keywords.Include ("REFERENCES");   --  psql:reserved, sql99:reserved
         Keywords.Include ("REFERENCING");   --  psql:reserved, sql99:
         Keywords.Include ("REINDEX");   --  psql:non, sql99:
         Keywords.Include ("RELATIVE");   --  psql:non, sql99:reserved
         Keywords.Include ("RENAME");   --  psql:non, sql99:
         Keywords.Include ("REPEATABLE");   --  psql:non, sql99:non
         Keywords.Include ("REPLACE");   --  psql:non, sql99:
         Keywords.Include ("RESET");   --  psql:non, sql99:
         Keywords.Include ("RESTRICT");   --  psql:non, sql99:reserved
         Keywords.Include ("RESULT");   --  psql:reserved, sql99:
         Keywords.Include ("RETURN");   --  psql:reserved, sql99:
         Keywords.Include ("RETURNED_LENGTH");   --  psql:non, sql99:non
         Keywords.Include ("RETURNED_OCTET_LENGTH");   --  psql:non, sql99:non
         Keywords.Include ("RETURNED_SQLSTATE");   --  psql:non, sql99:non
         Keywords.Include ("RETURNS");   --  psql:non, sql99:reserved
         Keywords.Include ("REVOKE");   --  psql:non, sql99:reserved
         Keywords.Include ("RIGHT");   --  psql:reserved, sql99:(can
         Keywords.Include ("ROLE");   --  psql:reserved, sql99:
         Keywords.Include ("ROLLBACK");   --  psql:non, sql99:reserved
         Keywords.Include ("ROLLUP");   --  psql:reserved, sql99:
         Keywords.Include ("ROUTINE");   --  psql:reserved, sql99:
         Keywords.Include ("ROUTINE_CATALOG");   --  psql:non, sql99:
         Keywords.Include ("ROUTINE_NAME");   --  psql:non, sql99:
         Keywords.Include ("ROUTINE_SCHEMA");   --  psql:non, sql99:
         Keywords.Include ("ROW");   --  psql:non, sql99:(cannot
         Keywords.Include ("ROWS");   --  psql:reserved, sql99:reserved
         Keywords.Include ("ROW_COUNT");   --  psql:non, sql99:non
         Keywords.Include ("RULE");   --  psql:non, sql99:
         Keywords.Include ("SAVEPOINT");   --  psql:reserved, sql99:
         Keywords.Include ("SCALE");   --  psql:non, sql99:non
         Keywords.Include ("SCHEMA");   --  psql:non, sql99:reserved
         Keywords.Include ("SCHEMA_NAME");   --  psql:non, sql99:non
         Keywords.Include ("SCOPE");   --  psql:reserved, sql99:
         Keywords.Include ("SCROLL");   --  psql:non, sql99:reserved
         Keywords.Include ("SEARCH");   --  psql:reserved, sql99:
         Keywords.Include ("SECOND");   --  psql:non, sql99:reserved
         Keywords.Include ("SECTION");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SECURITY");   --  psql:non, sql99:non
         Keywords.Include ("SELECT");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SELF");   --  psql:non, sql99:
         Keywords.Include ("SENSITIVE");   --  psql:non, sql99:
         Keywords.Include ("SEQUENCE");   --  psql:non, sql99:reserved
         Keywords.Include ("SERIALIZABLE");   --  psql:non, sql99:non
         Keywords.Include ("SERVER_NAME");   --  psql:non, sql99:non
         Keywords.Include ("SESSION");   --  psql:non, sql99:reserved
         Keywords.Include ("SESSION_USER");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SET");   --  psql:non, sql99:reserved
         Keywords.Include ("SETOF");   --  psql:non, sql99:(cannot
         Keywords.Include ("SETS");   --  psql:reserved, sql99:
         Keywords.Include ("SHARE");   --  psql:non, sql99:
         Keywords.Include ("SHOW");   --  psql:non, sql99:
         Keywords.Include ("SIMILAR");   --  psql:reserved, sql99:(can
         Keywords.Include ("SIMPLE");   --  psql:non, sql99:non
         Keywords.Include ("SIZE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SMALLINT");   --  psql:non, sql99:(cannot
         Keywords.Include ("SOME");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SOURCE");   --  psql:non, sql99:
         Keywords.Include ("SPACE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SPECIFIC");   --  psql:reserved, sql99:
         Keywords.Include ("SPECIFICTYPE");   --  psql:reserved, sql99:
         Keywords.Include ("SPECIFIC_NAME");   --  psql:non, sql99:
         Keywords.Include ("SQL");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SQLCODE");   --  psql:reserved, sql99:
         Keywords.Include ("SQLERROR");   --  psql:reserved, sql99:
         Keywords.Include ("SQLEXCEPTION");   --  psql:reserved, sql99:
         Keywords.Include ("SQLSTATE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("SQLWARNING");   --  psql:reserved, sql99:
         Keywords.Include ("STABLE");   --  psql:non, sql99:
         Keywords.Include ("START");   --  psql:non, sql99:reserved
         Keywords.Include ("STATE");   --  psql:reserved, sql99:
         Keywords.Include ("STATEMENT");   --  psql:non, sql99:reserved
         Keywords.Include ("STATIC");   --  psql:reserved, sql99:
         Keywords.Include ("STATISTICS");   --  psql:non, sql99:
         Keywords.Include ("STDIN");   --  psql:non, sql99:
         Keywords.Include ("STDOUT");   --  psql:non, sql99:
         Keywords.Include ("STORAGE");   --  psql:non, sql99:
         Keywords.Include ("STRICT");   --  psql:non, sql99:
         Keywords.Include ("STRUCTURE");   --  psql:reserved, sql99:
         Keywords.Include ("STYLE");   --  psql:non, sql99:
         Keywords.Include ("SUBCLASS_ORIGIN");   --  psql:non, sql99:non
         Keywords.Include ("SUBLIST");   --  psql:non, sql99:
         Keywords.Include ("SUBSTRING");   --  psql:non, sql99:(cannot
         Keywords.Include ("SUM");   --  psql:non, sql99:reserved
         Keywords.Include ("SYMMETRIC");   --  psql:non, sql99:
         Keywords.Include ("SYSID");   --  psql:non, sql99:
         Keywords.Include ("SYSTEM");   --  psql:non, sql99:
         Keywords.Include ("SYSTEM_USER");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TABLE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TABLE_NAME");   --  psql:non, sql99:non
         Keywords.Include ("TEMP");   --  psql:non, sql99:
         Keywords.Include ("TEMPLATE");   --  psql:non, sql99:
         Keywords.Include ("TEMPORARY");   --  psql:non, sql99:reserved
         Keywords.Include ("TERMINATE");   --  psql:reserved, sql99:
         Keywords.Include ("THAN");   --  psql:reserved, sql99:
         Keywords.Include ("THEN");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TIME");   --  psql:non, sql99:(cannot
         Keywords.Include ("TIMESTAMP");   --  psql:non, sql99:(cannot
         Keywords.Include ("TIMEZONE_HOUR");   --  psql:reserved, sql99:res.
         Keywords.Include ("TIMEZONE_MINUTE");   --  psql:reserved, sql99:res.
         Keywords.Include ("TO");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TOAST");   --  psql:non, sql99:
         Keywords.Include ("TRAILING");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TRANSACTION");   --  psql:non, sql99:reserved
         Keywords.Include ("TRANSACTIONS_COMMITTED");   --  psql:non, sql99:
         Keywords.Include ("TRANSACTIONS_ROLLED_BACK");   --  psql:non, sql99:
         Keywords.Include ("TRANSACTION_ACTIVE");   --  psql:non, sql99:
         Keywords.Include ("TRANSFORM");   --  psql:non, sql99:
         Keywords.Include ("TRANSFORMS");   --  psql:non, sql99:
         Keywords.Include ("TRANSLATE");   --  psql:non, sql99:reserved
         Keywords.Include ("TRANSLATION");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TREAT");   --  psql:non, sql99:(cannot
         Keywords.Include ("TRIGGER");   --  psql:non, sql99:reserved
         Keywords.Include ("TRIGGER_CATALOG");   --  psql:non, sql99:
         Keywords.Include ("TRIGGER_NAME");   --  psql:non, sql99:
         Keywords.Include ("TRIGGER_SCHEMA");   --  psql:non, sql99:
         Keywords.Include ("TRIM");   --  psql:non, sql99:(cannot
         Keywords.Include ("TRUE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("TRUNCATE");   --  psql:non, sql99:
         Keywords.Include ("TRUSTED");   --  psql:non, sql99:
         Keywords.Include ("TYPE");   --  psql:non, sql99:non
         Keywords.Include ("UNCOMMITTED");   --  psql:non, sql99:non
         Keywords.Include ("UNDER");   --  psql:reserved, sql99:
         Keywords.Include ("UNENCRYPTED");   --  psql:non, sql99:
         Keywords.Include ("UNION");   --  psql:reserved, sql99:reserved
         Keywords.Include ("UNIQUE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("UNKNOWN");   --  psql:non, sql99:reserved
         Keywords.Include ("UNLISTEN");   --  psql:non, sql99:
         Keywords.Include ("UNNAMED");   --  psql:non, sql99:non
         Keywords.Include ("UNNEST");   --  psql:reserved, sql99:
         Keywords.Include ("UNTIL");   --  psql:non, sql99:
         Keywords.Include ("UPDATE");   --  psql:non, sql99:reserved
         Keywords.Include ("UPPER");   --  psql:non, sql99:reserved
         Keywords.Include ("USAGE");   --  psql:non, sql99:reserved
         Keywords.Include ("USER");   --  psql:reserved, sql99:reserved
         Keywords.Include ("USER_DEFINED_TYPE_CATALOG");   --  psql:non, sql99:
         Keywords.Include ("USER_DEFINED_TYPE_NAME");   --  psql:non, sql99:
         Keywords.Include ("USER_DEFINED_TYPE_SCHEMA");   --  psql:non, sql99:
         Keywords.Include ("USING");   --  psql:reserved, sql99:reserved
         Keywords.Include ("VACUUM");   --  psql:non, sql99:
         Keywords.Include ("VALID");   --  psql:non, sql99:
         Keywords.Include ("VALIDATOR");   --  psql:non, sql99:
         Keywords.Include ("VALUE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("VALUES");   --  psql:non, sql99:reserved
         Keywords.Include ("VARCHAR");   --  psql:non, sql99:(cannot
         Keywords.Include ("VARIABLE");   --  psql:reserved, sql99:
         Keywords.Include ("VARYING");   --  psql:non, sql99:reserved
         Keywords.Include ("VERBOSE");   --  psql:reserved, sql99:(can
         Keywords.Include ("VERSION");   --  psql:non, sql99:
         Keywords.Include ("VIEW");   --  psql:non, sql99:reserved
         Keywords.Include ("VOLATILE");   --  psql:non, sql99:
         Keywords.Include ("WHEN");   --  psql:reserved, sql99:reserved
         Keywords.Include ("WHENEVER");   --  psql:reserved, sql99:reserved
         Keywords.Include ("WHERE");   --  psql:reserved, sql99:reserved
         Keywords.Include ("WITH");   --  psql:non, sql99:reserved
         Keywords.Include ("WITHOUT");   --  psql:non, sql99:reserved
         Keywords.Include ("WORK");   --  psql:non, sql99:reserved
         Keywords.Include ("WRITE");   --  psql:non, sql99:reserved
         Keywords.Include ("YEAR");   --  psql:non, sql99:reserved
         Keywords.Include ("ZONE");   --  psql:non, sql99:reserved
      end if;

      --  With postgreSQL, there is also an issue with identifiers using
      --  CamelCase (this is accepted when creating the database, but then
      --  we get an error when creating an index saying that the field does
      --  not exist). So we always convert to lower case.

      if Keywords.Contains (Str)
        or else To_Lower (Str) /= Str
      then
         --  Insert two '"' at start and end, since these names will be quoted
         --  in the generated files.

         return '"' & Str & '"';
      else
         return Str;
      end if;
   end Quote_Keyword;

end GNATCOLL.SQL.Inspect;
