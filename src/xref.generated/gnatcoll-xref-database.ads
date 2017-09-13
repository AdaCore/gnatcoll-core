with GNATCOLL.SQL; use GNATCOLL.SQL;
pragma Warnings (Off, "no entities of * are referenced");
pragma Warnings (On, "no entities of * are referenced");
with GNATCOLL.SQL.Exec;
with GNATCOLL.Xref.Database_Names; use GNATCOLL.Xref.Database_Names;
package GNATCOLL.Xref.Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   subtype F2f_Id is Integer;
   F2f_Has_Ali : constant F2f_Id := 1;
   F2f_Withs   : constant F2f_Id := 0;

   subtype E2e_Id is Integer;
   E2e_Access_Parameter : constant E2e_Id := 15;
   E2e_Component_Type   : constant E2e_Id := 9;
   E2e_From_Enumeration : constant E2e_Id := 10;
   E2e_Has_Discriminant : constant E2e_Id := 16;
   E2e_Has_Index        : constant E2e_Id := 7;
   E2e_Has_Primitive    : constant E2e_Id := 4;
   E2e_In_Out_Parameter : constant E2e_Id := 14;
   E2e_In_Parameter     : constant E2e_Id := 12;
   E2e_Instance_Of      : constant E2e_Id := 5;
   E2e_Is_Formal_Of     : constant E2e_Id := 17;
   E2e_Is_Param_Of      : constant E2e_Id := 6;
   E2e_Of_Type          : constant E2e_Id := 11;
   E2e_Out_Parameter    : constant E2e_Id := 13;
   E2e_Overrides        : constant E2e_Id := 8;
   E2e_Parent_Package   : constant E2e_Id := 18;
   E2e_Parent_Type      : constant E2e_Id := 1;
   E2e_Pointed_Type     : constant E2e_Id := 0;
   E2e_Renames          : constant E2e_Id := 3;

   type T_Abstract_E2e
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Fromentity : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Fromentity, Index);
      Toentity : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Toentity, Index);
      Kind : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Kind, Index);
      --  The type of link.

      Order_By : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Order_By, Index);
      --  Ordering among the references. Used for instance for subprogram
      --  parameters

   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_E2e (Instance : Cst_String_Access)
      is new T_Abstract_E2e (Ta_E2e, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_E2e (Index : Integer)
      is new T_Abstract_E2e (Ta_E2e, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_E2e_Kind
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Id, Index);
      Name : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Name, Index);
   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_E2e_Kind (Instance : Cst_String_Access)
      is new T_Abstract_E2e_Kind (Ta_E2e_Kind, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_E2e_Kind (Index : Integer)
      is new T_Abstract_E2e_Kind (Ta_E2e_Kind, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Entities
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Id, Index);
      Name : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Name, Index);
      --  Can be empty for forward decls (see above). Must be UTF-8

      Kind : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Kind, Index);
      --  The E_Kind for this entity

      Decl_File : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Decl_File, Index);
      --  Set to -1 for a predefined entity

      Decl_Line : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Decl_Line, Index);
      --  Set to -1 for a predefined entity

      Decl_Column : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Decl_Column, Index);
      --  Set to -1 for a predefined entity

      Decl_Caller : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Decl_Caller, Index);
      --  Parent entity

      Mangled_Name : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Mangled_Name, Index);
      --  Mangled name of the entity, if applicable

      Exported : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Exported, Index);
      --  Whether the mangled name is an export or an import of the entity

      Is_Global : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Global, Index);
      --  Whether this is a global entity (library-level in Ada)

      Is_Static_Local : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Static_Local, Index);
      --  Whether this is a 'static' variable in C/C++

   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_Entities (Instance : Cst_String_Access)
      is new T_Abstract_Entities (Ta_Entities, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Entities (Index : Integer)
      is new T_Abstract_Entities (Ta_Entities, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Entity_Kinds
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Id, Index);
      Display : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Display, Index);
      --  How to display this entity kind

      Is_Subprogram : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Subprogram, Index);
      Is_Container : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Container, Index);
      Body_Is_Full_Declaration : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Body_Is_Full_Declaration, Index);
      Is_Abstract : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Abstract, Index);
      Is_Generic : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Generic, Index);
      Is_Access : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Access, Index);
      Is_Type : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Type, Index);
      Is_Printable_In_Gdb : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Printable_In_Gdb, Index);
      Is_Array : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Array, Index);
      Has_Methods : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Has_Methods, Index);
   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_Entity_Kinds (Instance : Cst_String_Access)
      is new T_Abstract_Entity_Kinds (Ta_Entity_Kinds, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Entity_Kinds (Index : Integer)
      is new T_Abstract_Entity_Kinds (Ta_Entity_Kinds, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Entity_Refs
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Entity : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Entity, Index);
      --  The entity to which we have a reference

      File : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_File, Index);
      Line : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Line, Index);
      Column : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Column, Index);
      Kind : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Kind, Index);
      --  Type of reference (same letter as in ALI files)

      Caller : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Caller, Index);
      --  Enclosing entity at that location

      From_Instantiation : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_From_Instantiation, Index);
      --  Instances in which the ref occurs

   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_Entity_Refs (Instance : Cst_String_Access)
      is new T_Abstract_Entity_Refs (Ta_Entity_Refs, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Entity_Refs (Index : Integer)
      is new T_Abstract_Entity_Refs (Ta_Entity_Refs, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_F2f
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Fromfile : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Fromfile, Index);
      Tofile : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Tofile, Index);
      Kind : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Kind, Index);
      --  Kind of dependency

   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_F2f (Instance : Cst_String_Access)
      is new T_Abstract_F2f (Ta_F2f, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_F2f (Index : Integer)
      is new T_Abstract_F2f (Ta_F2f, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_F2f_Kind
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Id, Index);
      Name : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Name, Index);
   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_F2f_Kind (Instance : Cst_String_Access)
      is new T_Abstract_F2f_Kind (Ta_F2f_Kind, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_F2f_Kind (Index : Integer)
      is new T_Abstract_F2f_Kind (Ta_F2f_Kind, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Files
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Id, Index);
      Path : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Path, Index);
      --  Full normalized absolute path for the file. Casing is not normalized

      Stamp : GNATCOLL.SQL.SQL_Field_Time(Table_Name, Instance, N_Stamp, Index);
      --  The timestamp the last time the file was read (only set for LI files
      --  for efficiency)

      Language : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Language, Index);
      --  The language for this file (so that we can limit queries to specific
      --  languages), or "li"

      Project : GNATCOLL.SQL.SQL_Field_Integer(Table_Name, Instance, N_Project, Index);
      --  V2.0: The project to which a source file belongs. With aggregate
      --  projects, a source file might occur several times in this table,
      --  each time with a different project.

   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_Files (Instance : Cst_String_Access)
      is new T_Abstract_Files (Ta_Files, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Files (Index : Integer)
      is new T_Abstract_Files (Ta_Files, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   type T_Abstract_Reference_Kinds
      (Table_Name : Cst_String_Access; --  Name of the table
       Instance   : Cst_String_Access; --  if null, use table name
       Index      : Integer)  --  Create numbered aliases
   is new SQL_Table (Table_Name, Instance, Index) with
   record
      Id : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Id, Index);
      --  The character found in the ALI file

      Display : GNATCOLL.SQL.SQL_Field_Text(Table_Name, Instance, N_Display, Index);
      --  Label to display the reference

      Is_Real : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Real, Index);
      --  Whether the name of the entity appears at that location

      Is_Read : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Read, Index);
      Is_Write : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Write, Index);
      Is_End : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_End, Index);
      --  Whether this marks the end of a scope (spec or body)

      Show_In_Callgraph : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Show_In_Callgraph, Index);
      --  Whether this ref. should be shown in the call graph

      Is_Dispatching : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Dispatching, Index);
      --  Whether this is a dispatching call

      Is_Implicit : GNATCOLL.SQL.SQL_Field_Boolean(Table_Name, Instance, N_Is_Implicit, Index);
   end record;
   --  Use this table directly if you use clones of this table
   --  in your code, for instance in a temporary table

   type T_Reference_Kinds (Instance : Cst_String_Access)
      is new T_Abstract_Reference_Kinds (Ta_Reference_Kinds, Instance, -1) with null record;
   --  To use named aliases of the table in a query
   --  Use Instance=>null to use the default name.

   type T_Numbered_Reference_Kinds (Index : Integer)
      is new T_Abstract_Reference_Kinds (Ta_Reference_Kinds, null, Index) with null record;
   --  To use aliases in the form name1, name2,...

   function FK (Self : T_E2e'Class; Foreign : T_E2e_Kind'Class) return SQL_Criteria;
   function FK (Self : T_Entities'Class; Foreign : T_Entity_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_Entities'Class; Foreign : T_Files'Class) return SQL_Criteria;
   function FK (Self : T_Entities'Class; Foreign : T_Entities'Class) return SQL_Criteria;
   function FK (Self : T_Entity_Refs'Class; Foreign : T_Files'Class) return SQL_Criteria;
   function FK (Self : T_Entity_Refs'Class; Foreign : T_Reference_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_F2f'Class; Foreign : T_F2f_Kind'Class) return SQL_Criteria;
   function FK (Self : T_Files'Class; Foreign : T_Files'Class) return SQL_Criteria;
   E2e : T_E2e (null);
   E2e_Kind : T_E2e_Kind (null);
   Entities : T_Entities (null);
   Entity_Kinds : T_Entity_Kinds (null);
   Entity_Refs : T_Entity_Refs (null);
   F2f : T_F2f (null);
   F2f_Kind : T_F2f_Kind (null);
   Files : T_Files (null);
   Reference_Kinds : T_Reference_Kinds (null);

   procedure Create_Database
      (DB : not null access GNATCOLL.SQL.Exec.Database_Connection_Record'Class);
   --  Create the database and its initial contents
   --  The SQL is not automatically committed
end GNATCOLL.Xref.Database;
